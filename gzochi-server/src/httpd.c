/* httpd.c: Embedded httpd and minimal request mapping framework for gzochid
 * Copyright (C) 2016 Julian Graham
 *
 * gzochi is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <arpa/inet.h>
#include <assert.h>
#include <glib.h>
#include <microhttpd.h>
#include <netinet/in.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "httpd.h"

/* A node in the hierarchy of request mappings. */

struct _gzochid_httpd_pattern_node
{
  GRegex *path_regex; /* The pattern for this segment of the path. */
  GList *terminals; /* The list of terminal handlers. */
  GList *continuations; /* The list of continuation handlers. */
};

typedef struct _gzochid_httpd_pattern_node gzochid_httpd_pattern_node;

static gzochid_httpd_pattern_node *
create_pattern_node (const char *pattern)
{
  gzochid_httpd_pattern_node *pattern_node =
    calloc (1, sizeof (gzochid_httpd_pattern_node));

  /* These patterns may be applied many times over the lifetime of the HTTP 
     server, so the `G_REGEX_OPTIMIZE' is worth it. */

  pattern_node->path_regex = g_regex_new (pattern, G_REGEX_OPTIMIZE, 0, NULL);
  
  return pattern_node;
}

static void
free_pattern_node (gzochid_httpd_pattern_node *pattern_node)
{
  g_regex_unref (pattern_node->path_regex);
  
  g_list_free_full (pattern_node->terminals, (GDestroyNotify) free);
  g_list_free_full (pattern_node->continuations, (GDestroyNotify) free);

  free (pattern_node);
}

static gboolean
traverse_free (GNode *node, gpointer data)
{
  free_pattern_node (node->data);
  return FALSE;
}

/* The HTTP server object. */

struct _GzochidHttpServer
{
  GObject parent_instance;

  GNode *root; /* The root of the request mapping hierarchy. */

  /* The socket address to which the server is bound. */

  struct sockaddr_in addr; 

  socklen_t addrlen; /* The size of the socket address. */
  struct MHD_Daemon *daemon; /* The GNU microhttpd daemon struct. */
};

G_DEFINE_TYPE (GzochidHttpServer, gzochid_http_server, G_TYPE_OBJECT);

static void 
gzochid_http_server_finalize (GObject *gobject)
{
  GzochidHttpServer *server = GZOCHID_HTTP_SERVER (gobject);

  if (server->daemon != NULL)
    gzochid_http_server_stop (server);
  
  g_node_traverse
    (server->root, G_IN_ORDER, G_TRAVERSE_ALL, -1, traverse_free, NULL);
  g_node_destroy (server->root);

  G_OBJECT_CLASS (gzochid_http_server_parent_class)->finalize (gobject);
}

static void
gzochid_http_server_class_init (GzochidHttpServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_http_server_finalize;
}

static void
gzochid_http_server_init (GzochidHttpServer *self)
{
  self->root = g_node_new (create_pattern_node (""));

  self->addrlen = sizeof (struct sockaddr_in);
  memset (&self->addr, 0, self->addrlen);

  self->addr.sin_family = AF_INET;
  self->addr.sin_port = 0;
  self->addr.sin_addr.s_addr = INADDR_ANY;
  
  self->daemon = NULL;
}

/* The HTTP resonse sink. */

struct _gzochid_http_response_sink
{
  /* Whether the response was written within the scope of a handler invocation.
     The response can only be written once over the lifetime of a request 
     flow. */
  
  gboolean response_written; 

  /* Stores the result of enqueueing the response. This is the value returned 
     to the GNU microhttpd framework. */

  int queue_code; 

  /* The GNU microhttpd connecction object. */
  
  struct MHD_Connection *connection; 
};

/* A little bit of legal compiler trickery to make it possible to declare
   `gzochid_httpd_partial' as a struct in the header while still being able to
   cast it back and forth from `GNode'. */

struct _gzochid_httpd_partial
{
  GNode *node;
};

/* Combines a terminal handler with a "user data" pointer. */

struct _gzochid_httpd_terminal_registration
{
  gzochid_httpd_terminal terminal; /* The terminal handler function. */
  gpointer user_data; /* The user data pointer. */
};

typedef struct _gzochid_httpd_terminal_registration
gzochid_httpd_terminal_registration;

/* Combines a continuation handler with a "user data" pointer. */

struct _gzochid_httpd_continuation_registration
{
  /* The continuation handler function. */

  gzochid_httpd_continuation continuation;
  
  gpointer user_data; /* The user data pointer. */
};

typedef struct _gzochid_httpd_continuation_registration
gzochid_httpd_continuation_registration;

/* A match of a pattern in the matching hierarchy against a prefix of the URL 
   path. */

struct _path_match
{
  /* The match structure, to be passed to the handler if this is the best 
     match. */

  GMatchInfo *match_info; 

  /* The match length, cached so it doesn't have to be repeatedly extrated from
     the match info. */

  size_t match_length; 

  GNode *matched_node; /* The node in the hierarchy that owns the pattern. */
};

typedef struct _path_match path_match;

/* Combines a path (with positional offset) and a mutable `path_match' to 
   provide the contextual pointer for the hierarchy traversal. */

struct _node_match_context
{
  const char *path; /* The URL path. */
  int pos; /* The current position. */

  path_match *path_match; /* The path match structure. */
};

typedef struct _node_match_context node_match_context;

/* A pattern to search for and a pointer in which to store the node discovered 
   with that pattern during a pattern hierarchical traversal. */

struct _node_pattern_search_context
{
  const char *pattern; /* The pattern to find. */
  GNode *matching_node; /* A pointer to the node that has that pattern. */
};

typedef struct _node_pattern_search_context node_pattern_search_context;

/* Continuation registration constructor. */

static gzochid_httpd_continuation_registration *
create_continuation_registration (gzochid_httpd_continuation continuation,
				  gpointer user_data)
{
  gzochid_httpd_continuation_registration *registration =
    malloc (sizeof (gzochid_httpd_continuation_registration));

  registration->continuation = continuation;
  registration->user_data = user_data;
  
  return registration;
}

/* Terminal registration constructor. */

static gzochid_httpd_terminal_registration *
create_terminal_registration (gzochid_httpd_terminal terminal,
			      gpointer user_data)
{
  gzochid_httpd_terminal_registration *registration =
    malloc (sizeof (gzochid_httpd_terminal_registration));

  registration->terminal = terminal;
  registration->user_data = user_data;
  
  return registration;
}

/* Convenience function to compute the match length. */

static size_t
match_length (GMatchInfo *match_info)
{
  int start_pos = 0, end_pos = 0;
  g_match_info_fetch_pos (match_info, 0, &start_pos, &end_pos);
  return end_pos - start_pos;
}

static void
path_match_free (path_match *match)
{
  g_match_info_free (match->match_info);
  free (match);
}

static gboolean
find_longest_match_inner (GNode *node, gpointer data)
{
  node_match_context *match_context = data;
  gzochid_httpd_pattern_node *pattern_node = node->data;
  GMatchInfo *match_info = NULL;
  
  if (g_regex_match_full
      (pattern_node->path_regex, match_context->path, -1, match_context->pos,
       G_REGEX_MATCH_ANCHORED, &match_info, NULL))
    {
      size_t len = match_length (match_info);
      gboolean best_match = FALSE;
      
      if (match_context->path_match == NULL)
	{
	  match_context->path_match = malloc (sizeof (path_match));
	  best_match = TRUE;
	}

      /* The matcher favors longer matches; this allows handlers to be 
	 registered for both `/' and `/foo'. */
      
      else if (len > match_context->path_match->match_length)
	{
	  g_match_info_free (match_context->path_match->match_info);
	  best_match = TRUE;
	}

      if (best_match)
	{
	  match_context->path_match->match_info = match_info;
	  match_context->path_match->match_length = len;
	  match_context->path_match->matched_node = node;
	}
    }
  else g_match_info_free (match_info);
      
  if (node->next == NULL)
    return TRUE;
  else return FALSE;
}

static path_match *
find_longest_match (GNode *node, const char *path, const int pos)
{
  node_match_context match_context;
  
  match_context.path = path;
  match_context.pos = pos;
  match_context.path_match = NULL;

  if (node->children != NULL)
    g_node_traverse
      (node, G_POST_ORDER, G_TRAVERSE_ALL, 2,
       find_longest_match_inner, &match_context);

  return match_context.path_match;
}

static gboolean
find_pattern_node_inner (GNode *node, gpointer user_data)
{
  node_pattern_search_context *search_context = user_data;
  gzochid_httpd_pattern_node *pattern_node = node->data;

  /* Do a straight-up match on the raw pattern string. */
  
  if (strcmp (search_context->pattern,
	      g_regex_get_pattern (pattern_node->path_regex)) == 0)
    {
      search_context->matching_node = node;
      return TRUE;
    }
  else if (node->next == NULL)
    return TRUE;
  else return FALSE;
}

static GNode *
find_pattern_node (GNode *node, const char *pattern)
{
  node_pattern_search_context search_context;
  
  search_context.pattern = pattern;
  search_context.matching_node = NULL;

  g_node_traverse
    (node, G_POST_ORDER, G_TRAVERSE_ALL, 2, find_pattern_node_inner,
     &search_context);

  return search_context.matching_node;
}

static void
add_terminal (GNode *node, gzochid_httpd_terminal terminal, gpointer user_data)
{
  gzochid_httpd_pattern_node *pattern_node = node->data;

  pattern_node->terminals = g_list_append
    (pattern_node->terminals, create_terminal_registration
     (terminal, user_data));
}

void
gzochid_httpd_add_terminal (GzochidHttpServer *server, const char *pattern,
			    gzochid_httpd_terminal terminal, gpointer user_data)
{
  GNode *target = find_pattern_node (server->root, pattern);

  if (target == NULL)
    target = g_node_insert
      (server->root, -1, g_node_new (create_pattern_node (pattern)));
  
  add_terminal (target, terminal, user_data);
}

void
gzochid_httpd_append_terminal (gzochid_httpd_partial *partial,
			       const char *pattern,
			       gzochid_httpd_terminal terminal,
			       gpointer user_data)
{
  GNode *node = (GNode *) partial;
  GNode *target = find_pattern_node (node, pattern);

  if (target == NULL)
    target = g_node_insert
      (node, -1, g_node_new (create_pattern_node (pattern)));
  
  add_terminal (target, terminal, user_data);
}

static void
add_continuation (GNode *node, gzochid_httpd_continuation continuation,
		  gpointer user_data)
{
  gzochid_httpd_pattern_node *pattern_node = node->data;
  
  pattern_node->continuations = g_list_append
    (pattern_node->continuations,
     create_continuation_registration (continuation, user_data));
}
  
gzochid_httpd_partial *
gzochid_httpd_add_continuation (GzochidHttpServer *server,
				const char *pattern,
				gzochid_httpd_continuation continuation,
				gpointer user_data)
{
  GNode *target = find_pattern_node (server->root, pattern);

  if (target == NULL)
    target = g_node_insert
      (server->root, -1, g_node_new (create_pattern_node (pattern)));
  
  add_continuation (target, continuation, user_data);

  return (gzochid_httpd_partial *) target;
}

gzochid_httpd_partial *
gzochid_httpd_append_continuation (gzochid_httpd_partial *partial,
				   const char *pattern,
				   gzochid_httpd_continuation continuation,
				   gpointer user_data)
{
  GNode *node = (GNode *) partial;
  GNode *target = find_pattern_node (node, pattern);

  if (target == NULL)
    target = g_node_insert
      (node, -1, g_node_new (create_pattern_node (pattern)));
  
  add_continuation (target, continuation, user_data);

  return (gzochid_httpd_partial *) target;
}

static int 
not_found404 (struct MHD_Connection *connection, const char *page, 
	      int must_free, int must_copy)
{
  struct MHD_Response *response = MHD_create_response_from_data 
    (strlen (page), (void *) page, must_free, must_copy);
  int ret = MHD_queue_response (connection, 404, response);

  MHD_destroy_response (response);

  return ret;
}

static int 
not_found404_default (struct MHD_Connection *connection)
{
  return not_found404 
    (connection, "<html><body>Not found.</body></html>", FALSE, FALSE);
}

void
gzochid_http_write_response
(gzochid_http_response_sink *sink, int code, char *bytes, size_t len)
{
  struct MHD_Response *response =
    MHD_create_response_from_data (len, bytes, TRUE, TRUE);

  sink->queue_code = MHD_queue_response (sink->connection, code, response);
  MHD_destroy_response (response);
  sink->response_written = TRUE;
}

/* The `MHD_AccessHandlerCallback' for GNU microhttpd. */

static int 
dispatch (void *cls, struct MHD_Connection *connection, 
	  const char *path, const char *method, const char *version, 
	  const char *upload_data, size_t *upload_data_size, void **con_cls)
{
  gzochid_http_response_sink sink;
  GzochidHttpServer *server = cls;
  GNode *node = server->root;

  int pos = 0;
  gpointer request_context = NULL;
  path_match *match = NULL;
  
  if (strcmp (method, "GET") != 0)
    return 0;

  sink.response_written = FALSE;
  sink.connection = connection;

  /* Traverse the hierarchy by finding, at each level, the child with the 
     pattern that produces the longest match against the current suffix of the
     URL path. If that node exists and has continuation handlers, invoke them
     before consuming the matched portion of the path (by incrementing the
     position by the length of the match. If that node doesn't exist, return a
     404. 

     Once the path is fully consumed by pattern matches, invoke the terminal
     handlers on the final match node. */
  
  while (path[pos] != 0 && path[pos] != '?')
    {
      GList *continuations = NULL;
      int start_pos = 0, end_pos = 0;
      gzochid_httpd_pattern_node *pattern_node = NULL;
      
      if (match != NULL)
	{
	  node = match->matched_node;
	  path_match_free (match);
	}
      
      match = find_longest_match (node, path, pos);

      if (match == NULL)
	break;

      pattern_node = match->matched_node->data;
      continuations = pattern_node->continuations;
      
      while (continuations != NULL)
	{
	  gzochid_httpd_continuation_registration *registration =
	    continuations->data;
	  
	  request_context = registration->continuation
	    (match->match_info, &sink, request_context,
	     registration->user_data);

	  /* If any continuation handler writes a response, halt 
	     processing. */
	  
	  if (sink.response_written)
	    {
	      path_match_free (match);
	      break;
	    }
	  
	  continuations = continuations->next;
	}

      /* Advance the position by the length of the matched segment. */
      
      g_match_info_fetch_pos (match->match_info, 0, &start_pos, &end_pos);
      pos += (end_pos - start_pos);
    }     

  /* The response may have already been written by one of the continuation 
     handlers - short-circuiting to a 404, for example. */
  
  if (sink.response_written)
    return sink.queue_code;

  /* If the loop ran out of URL path ending on a match node, invoke that node's 
     terminal handlers. */
  
  else if (match != NULL)
    {
      gzochid_httpd_pattern_node *pattern_node = match->matched_node->data;
      GList *terminals = pattern_node->terminals;
      
      while (terminals != NULL)
	{
	  gzochid_httpd_terminal_registration *registration =
	    terminals->data;
	  
	  registration->terminal
	    (match->match_info, &sink, request_context,
	     registration->user_data);

	  /* Stop processing if the handler wrote a response. */
	  
	  if (sink.response_written)
	    break;
	  
	  terminals = terminals->next;
	}

      path_match_free (match);

      if (sink.response_written)
	return sink.queue_code;

      /* If none of the handlers wrote a response, assume 404. */
      
      else return not_found404_default (connection);
    }   
  else return not_found404_default (connection);
}

void 
gzochid_http_server_start (GzochidHttpServer *server, guint port, GError **err)
{
  assert (server->daemon == NULL);

  if (port == 0)
    server->daemon = MHD_start_daemon 
      (MHD_USE_SELECT_INTERNALLY, 1, NULL, NULL, dispatch, server,
       MHD_OPTION_SOCK_ADDR, &server->addr, MHD_OPTION_END);      

  else server->daemon = MHD_start_daemon 
	 (MHD_USE_SELECT_INTERNALLY, port, NULL, NULL, dispatch, server,
	  MHD_OPTION_END);

  if (server->daemon == NULL)
    g_set_error
      (err, GZOCHID_HTTP_SERVER_ERROR, GZOCHID_HTTP_SERVER_ERROR_FAILED,
       "Failed to start HTTP server on port %d.", port);
  else
    {
      _gzochid_http_server_getsockname
	(server, (struct sockaddr *) &server->addr, &server->addrlen);
      
      g_message ("HTTP server listening on port %d.",
		 ntohs (server->addr.sin_port));
    }
}

void
gzochid_http_server_stop (GzochidHttpServer *server)
{
  assert (server->daemon != NULL);

  MHD_stop_daemon (server->daemon);
  server->daemon = NULL;
}

void
_gzochid_http_server_getsockname (GzochidHttpServer *server,
				  struct sockaddr *addr, socklen_t *addrlen)
{
  const union MHD_DaemonInfo *daemon_info = NULL;
  
  /* Ensure the server is listening. */

  assert (server->daemon != NULL);
  
  daemon_info = MHD_get_daemon_info (server->daemon, MHD_DAEMON_INFO_LISTEN_FD);
  getsockname (daemon_info->listen_fd, addr, addrlen);
}

GQuark
gzochid_http_server_error_quark ()
{
  return g_quark_from_static_string ("gzochid-http-server-error-quark");
}
