/* httpd-app.c: Handler implementations for the gzochid admin web console
 * Copyright (C) 2017 Julian Graham
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

#include <assert.h>
#include <ctype.h>
#include <glib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "config.h"
#include "event.h"
#include "event-app.h"
#include "game.h"
#include "httpd.h"
#include "httpd-app.h"
#include "resolver.h"
#include "util.h"

#define HEADER "  <head><title>gzochid v" VERSION "</title></head>"

/* The following declaraitons support rendering the contents of an object in the
   object store. */

#define OID_PREFIX_LEN 26
#define OID_SUFFIX_LEN 29
#define OID_LINE_LEN 80

/* Holds information about the current meta server connection. */

struct _gzochid_metaserver_info
{
  char *connection_description; /* The socket address of the meta server. */

  /* The base URL of the meta server's admin HTTP console, or `NULL' if the
     meta server didn't provide an address. */

  char *admin_server_base_url; 
};

typedef struct _gzochid_metaserver_info gzochid_metaserver_info;

/* Construct and return new `gzochid_metaserver_info' instance, copying the
   specified connection description and admin server base URL (which can be 
   `NULL'). The returned pointer should be freed via 
   `gzochid_metaserver_info_free' when no longer in use. */

static gzochid_metaserver_info *
gzochid_metaserver_info_new (const char *connection_description,
			     const char *admin_server_base_url)
{
  gzochid_metaserver_info *info = malloc (sizeof (gzochid_metaserver_info));

  info->connection_description = strdup (connection_description);

  if (admin_server_base_url != NULL)
    info->admin_server_base_url = strdup (admin_server_base_url);
  else info->admin_server_base_url = NULL;
  
  return info;
}

/* Release the memory associated with the specified `gzochid_metaserver_info'
   object. */

static void
gzochid_metaserver_info_free (gzochid_metaserver_info *info)
{
  free (info->connection_description);

  if (info->admin_server_base_url != NULL)
    free (info->admin_server_base_url);

  free (info);
}

/* The state of the gzochid application container, in terms of the information
   displayed on the admin console. This is effectively an "offline"
   representation, to decouple the rendering of the admin console from what's
   actually going on in the container. */

struct _gzochid_server_state
{
  /* `TRUE' if the server has a metaclient configured, `FALSE' otherwise. */

  gboolean has_metaclient; 
  
  /* Information about the currently-connected meta server, if such a connection
     has been configured and is active; otherwise, `NULL'. */
  
  gzochid_metaserver_info *metaserver_info;

  GMutex mutex; /* Mutex to protect updates to the state object. */
};

typedef struct _gzochid_server_state gzochid_server_state;

/* Construct and return a pointer to a new `gzochid_server_state' object. */

static gzochid_server_state *
gzochid_server_state_new ()
{
  gzochid_server_state *state = malloc (sizeof (gzochid_server_state));

  state->has_metaclient = FALSE;
  state->metaserver_info = NULL;
  g_mutex_init (&state->mutex);
  
  return state;
}

/* Captures serialization state of an object. */

struct data_state 
{
  char *data; /* The raw serialized object bytes. */
  int data_offset; /* The current offset within the object bytes. */
  int data_length; /* the object length. */

  char *current_line; /* The current line in the hex dump display. */
  int current_line_offset; /* The renderng offset within the current line. */
  int current_line_length; /* The length of the current hex line. */
};

static void 
next_line (struct data_state *state)
{
  char *buf = NULL;
  int i = 0, remaining = state->data_length - state->data_offset,
    num_bytes = MIN (16, remaining);
  gboolean needs_prefix = state->data_offset == 0;
  gboolean needs_suffix = remaining <= 16;

  int line_length = OID_LINE_LEN - (16 - num_bytes) + 1;
  
  if (state->data_offset == state->data_length)
    return;

  if (needs_prefix)
    line_length += OID_PREFIX_LEN;
  if (needs_suffix)
    line_length += OID_SUFFIX_LEN;
  
  state->current_line = malloc (sizeof (char) * (line_length + 1));
  state->current_line_length = line_length;

  buf = state->current_line;

  if (needs_prefix)
    {
      snprintf (buf, 8, "<html>\n");
      snprintf (buf + 7, 10, "  <body>\n");
      snprintf (buf + 16, 11, "    <pre>\n");
      buf += OID_PREFIX_LEN;
    }

  snprintf (buf, 13, "%.8x    ", state->data_offset);
  buf += 12;
  
  for (i = 0; i < num_bytes; i++)
    {
      snprintf (buf, 4, "%.2x ", state->data[state->data_offset + i]);
      buf += 3;

      if ((i + 1) % 4 == 0)
	{
	  snprintf (buf, 2, " ");
	  buf += 1;
	}
    }

  for (i = num_bytes; i < 16; i++)
    {
      snprintf (buf, 4, "   ");
      buf += 3;

      if ((i + 1) % 4 == 0)
	{
	  snprintf (buf, 2, " ");
	  buf += 1;
	}
    }

  for (i = 0; i < num_bytes; i++)
    {
      char c = state->data[state->data_offset + i];
      snprintf (buf++, 2, "%c", isgraph (c) ? c : '.');
    }

  snprintf (buf++, 2, "\n");
  
  state->data_offset += num_bytes;

  if (needs_suffix)
    {
      snprintf (buf, 12, "    </pre>\n");
      snprintf (buf + 11, 11, "  </body>\n");
      snprintf (buf + 21, 9, "</html>\n");
      buf += OID_SUFFIX_LEN;
    }
}

/* Writes a portion of the current line to the specified buffer, up the 
   specified maximum character count. */

static ssize_t 
write_data_line (struct data_state *state, char *buf, size_t max)
{
  int n = 0;

  /* If there is no current line, render one to the state buffer. */
  
  if (state->current_line == NULL)
    next_line (state);

  /* ...and if there's still no current line, we must be out of input. */
  
  if (state->current_line == NULL)
    return -1;

  n = MIN (max, state->current_line_length - state->current_line_offset);
  memcpy (buf, state->current_line + state->current_line_offset, n);
  state->current_line_offset += n;

  if (state->current_line_offset == state->current_line_length)
    {
      free (state->current_line);
      
      state->current_line = NULL;
      state->current_line_length = 0;
      state->current_line_offset = 0;
    }

  return n;
}

static void 
free_data_state (void *ptr)
{
  struct data_state *state = ptr;

  free (state->data);

  if (state->current_line != NULL)
    free (state->current_line);

  free (state);
}

/* For convenience, duplicates the the "not found" handler in `httpd.c'. (In the
   future, it might be reasonable to consolidate implementations and make a 
   single, shared one visible. */

static void
not_found404_default (gzochid_http_response_sink *sink)
{
  gzochid_http_write_response
    (sink, 404, "<html><body>Not found.</body></html>", 36);
}

#define GREETING \
  "<h1>Hello, browser!</h1>\n" \
  "<p>This is the administrative web console for the gzochid game " \
  "application server. You can use this console to interrogate the state of " \
  "applications running within container, including their attached data " \
  "stores.</p>"

/* Writes a global header to the specified outbut buffer. */

static void
append_header (GString *response_str)
{
  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, HEADER);
  g_string_append (response_str, "  <body>\n");
}

/* Writes a global footer to the specified output buffer. */

static void
append_footer (GString *response_str)
{
  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>\n");
}

static void
hello_world (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	     gpointer request_context, gpointer user_data)
{
  gzochid_server_state *state = user_data;
  GString *response_str = g_string_new (NULL);

  append_header (response_str);
  g_string_append (response_str, GREETING);

  g_mutex_lock (&state->mutex);

  if (state->metaserver_info != NULL)
    {
      g_string_append (response_str, "<p>Connected to the meta server at ");

      if (state->metaserver_info->admin_server_base_url != NULL)
	g_string_append_printf
	  (response_str, "<a href=\"%s\">%s</a>",
	   state->metaserver_info->admin_server_base_url,
	   state->metaserver_info->connection_description);

      else g_string_append
	     (response_str, state->metaserver_info->connection_description);
      
      g_string_append (response_str, ".</p>\n");
    }
  
  g_mutex_unlock (&state->mutex);
  
  g_string_append (response_str, "<a href=\"app/\">app/</a>");
  append_footer (response_str);

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

static void
list_apps (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	   gpointer request_context, gpointer user_data)
{
  GzochidGameServer *game_server = user_data;
  GList *apps = gzochid_game_server_get_applications (game_server);

  if (apps == NULL)
    gzochid_http_write_response
      (sink, 404, "<html><body>No applications.</body></html>", 22);
  else 
    {
      GList *apps_ptr = apps;
      GString *response_str = g_string_new (NULL);

      append_header (response_str);
      g_string_append (response_str, "    <ul>\n");
      
      while (apps_ptr != NULL)
	{
	  gzochid_application_context *app = apps_ptr->data;
	  
  	  g_string_append_printf 
	    (response_str, 
	     "      <li><a href=\"/app/%s/\">%s</a></li><br />\n", 
	     app->descriptor->name, app->descriptor->name);

	  apps_ptr = apps_ptr->next;
	}

      g_string_append (response_str, "    </ul>\n");
      append_footer (response_str);

      gzochid_http_write_response
	(sink, 200, response_str->str, response_str->len);

      g_string_free (response_str, TRUE);
      g_list_free (apps);
    }
}

static gpointer
bind_app (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	  gpointer request_context, gpointer user_data)
{
  gchar *app = g_match_info_fetch (match_info, 1);
  GzochidGameServer *game_server = user_data;
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (game_server, app);

  g_free (app);
  
  if (app_context == NULL)
    not_found404_default (sink);

  return app_context;
}

static void 
app_info (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	  gpointer request_context, gpointer user_data)
{
  GString *response_str = g_string_new (NULL);
  gzochid_application_context *app_context = request_context;
  gzochid_server_state *state = user_data;

  append_header (response_str);

  g_string_append_printf 
    (response_str, "    <h1>%s</h1>\n", app_context->descriptor->name);
  g_string_append_printf 
    (response_str, "    <p>%s</p>\n", app_context->descriptor->description);

  /* Shouldn't show users links to dump contents of store if we're in 
     distributed mode. */
  
  if (!state->has_metaclient)
    {
      g_string_append (response_str, "    <h2>Application data</h2>\n");
      g_string_append_printf 
	(response_str, "    <a href=\"/app/%s/names/\">names</a><br />\n", 
	 app_context->descriptor->name);
      g_string_append_printf 
	(response_str, "    <a href=\"/app/%s/oids/\">oids</a><br />\n", 
	 app_context->descriptor->name);
    }
  
  g_string_append (response_str, "    <h2>Application statistics</h2>\n");
  g_string_append (response_str, "    <table>\n");

  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Bytes read</td>\n");
  g_string_append_printf (response_str, "        <td>%lu</td>\n", 
			  app_context->stats->bytes_read);
  g_string_append (response_str, "      </tr>\n");
  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Bytes written</td>\n");
  g_string_append_printf (response_str, "        <td>%lu</td>\n", 
			  app_context->stats->bytes_written);
  g_string_append (response_str, "      </tr>\n");

  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Messages received</td>\n");
  g_string_append_printf (response_str, "        <td>%u</td>\n", 
			  app_context->stats->num_messages_received);
  g_string_append (response_str, "      </tr>\n");
  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Messages sent</td>\n");
  g_string_append_printf (response_str, "        <td>%u</td>\n", 
			  app_context->stats->num_messages_sent);
  g_string_append (response_str, "      </tr>\n");

  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Transactions started</td>\n");
  g_string_append_printf (response_str, "        <td>%u</td>\n", 
			  app_context->stats->num_transactions_started);
  g_string_append (response_str, "      </tr>\n");
  g_string_append (response_str, "      <tr>\n");
  g_string_append (response_str, "        <td>Transactions committed</td>\n");
  g_string_append_printf (response_str, "        <td>%u</td>\n", 
			  app_context->stats->num_transactions_committed);
  g_string_append (response_str, "      </tr>\n");
  g_string_append (response_str, "      <tr>\n");
  g_string_append 
    (response_str, "        <td>Transactions rolled back</td>\n");
  g_string_append_printf (response_str, "        <td>%u</td>\n", 
			  app_context->stats->num_transactions_rolled_back);
  g_string_append (response_str, "      </tr>\n");

  if (app_context->stats->num_transactions_committed > 0)
    {
      g_string_append (response_str, "      <tr>\n");
      g_string_append
	(response_str, "        <td>Maximum transaction duration</td>\n");
      g_string_append_printf 
	(response_str, "        <td>%ld</td>\n", 
	 app_context->stats->max_transaction_duration);
      g_string_append (response_str, "      </tr>\n");

      g_string_append (response_str, "      <tr>\n");
      g_string_append
	(response_str, "        <td>Minimum transaction duration</td>\n");
      g_string_append_printf 
	(response_str, "        <td>%ld</td>\n", 
	 app_context->stats->min_transaction_duration);
      g_string_append (response_str, "      </tr>\n");

      g_string_append (response_str, "      <tr>\n");
      g_string_append
	(response_str, "        <td>Average transaction duration</td>\n");
      g_string_append_printf 
	(response_str, "        <td>%.2f</td>\n", 
	 app_context->stats->average_transaction_duration);
      g_string_append (response_str, "      </tr>\n");
    }

  g_string_append (response_str, "    </table>\n");
  append_footer (response_str);

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

static void
list_oids (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	   gpointer request_context, gpointer user_data)
{
  gzochid_application_context *app_context = request_context;
  GString *response_str = g_string_new (NULL);
  
  size_t klen = 0;
  gzochid_storage_transaction *tx = app_context->storage_engine_interface
    ->transaction_begin (app_context->storage_context);
  char *k = app_context->storage_engine_interface
    ->transaction_first_key (tx, app_context->oids, &klen);

  app_context->storage_engine_interface->transaction_rollback (tx);

  assert (klen == sizeof (guint64));  

  append_header (response_str);
  g_string_append_printf 
    (response_str, "    <h1>%s</h1><br />\n", app_context->descriptor->name);
  g_string_append (response_str, "    <ul>\n");
  
  while (k != NULL)
    {
      guint64 encoded_oid = 0;
      char *next_k = NULL;

      memcpy (&encoded_oid, k, sizeof (guint64));
            
      g_string_append (response_str, "      <li><a href=\"");
      g_string_append_printf
	(response_str, "%" G_GUINT64_FORMAT,
	 gzochid_util_decode_oid (encoded_oid));
      g_string_append (response_str, "\">");
      g_string_append_printf
	(response_str, "%" G_GUINT64_FORMAT,
	 gzochid_util_decode_oid (encoded_oid));
      g_string_append (response_str, "</a></li>\n");

      tx = app_context->storage_engine_interface
	->transaction_begin (app_context->storage_context);      
      next_k = app_context->storage_engine_interface->transaction_next_key
	(tx, app_context->oids, k, klen, &klen);
      app_context->storage_engine_interface->transaction_rollback (tx);
      
      free (k);
      k = next_k;
    }
  
  g_string_append (response_str, "    </ul>\n");
  append_footer (response_str);

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

static void
render_oid (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	    gpointer request_context, gpointer user_data)
{
  size_t data_length = 0;
  char *oid_str = g_match_info_fetch (match_info, 1);
  gzochid_application_context *app_context = request_context;
  gzochid_storage_transaction *tx = app_context->storage_engine_interface
    ->transaction_begin (app_context->storage_context);
  guint64 oid = gzochid_util_encode_oid (g_ascii_strtoull (oid_str, NULL, 16)); 
  char *data = app_context->storage_engine_interface->transaction_get
    (tx, app_context->oids, (char *) &oid, sizeof (guint64), &data_length);
  
  app_context->storage_engine_interface->transaction_rollback (tx);
  
  free (oid_str);

  if (data == NULL)
    not_found404_default (sink);
  else
    {
      char buf[256];
      ssize_t len = 0;
      GString *response = g_string_new (NULL);
      struct data_state *state = calloc (1, sizeof (struct data_state));

      state->data = data;
      state->data_length = data_length;

      while ((len = write_data_line (state, buf, 256)) >= 0)
	g_string_append_len (response, buf, len);

      free_data_state (state);
      gzochid_http_write_response (sink, 200, response->str, response->len);
      g_string_free (response, TRUE);
    }
}

static void
list_names (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	    gpointer request_context, gpointer user_data)
{
  GString *response_str = g_string_new (NULL);
  
  size_t klen = 0;
  gzochid_application_context *app_context = request_context;
  gzochid_storage_transaction *tx = app_context->storage_engine_interface
    ->transaction_begin (app_context->storage_context);
  char *k = app_context->storage_engine_interface
    ->transaction_first_key (tx, app_context->names, &klen);

  app_context->storage_engine_interface->transaction_rollback (tx);

  append_header (response_str);
  g_string_append_printf 
    (response_str, "    <h1>%s</h1><br />\n", app_context->descriptor->name);
  g_string_append (response_str, "    <ul>\n");

  while (k != NULL)
    {
      char *next_k = NULL;

      g_string_append (response_str, "      <li>");
      g_string_append_len (response_str, k, klen);
      g_string_append (response_str, "</li>\n");

      tx = app_context->storage_engine_interface
	->transaction_begin (app_context->storage_context);      
      next_k = app_context->storage_engine_interface->transaction_next_key
	(tx, app_context->names, k, klen, &klen);
      app_context->storage_engine_interface->transaction_rollback (tx);
      
      free (k);
      k = next_k;
    }

  g_string_append (response_str, "    </ul>\n");
  append_footer (response_str);

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

/* A no-op continuation used to root a `gzochid_httpd_partial' and just pass 
   through the request context. */

static gpointer
null_continuation (const GMatchInfo *match_info,
		   gzochid_http_response_sink *sink, gpointer request_context,
		   gpointer user_data)
{
  return request_context;
}

/* `gzochid_event_handler' implementation to update the admin console's
   representation of the server state in response to meta server events. */

static void
handle_metaserver_event (GzochidEvent *event, gpointer user_data)
{
  gzochid_server_state *state = user_data;
  GzochidMetaServerEvent *metaserver_event = GZOCHID_META_SERVER_EVENT (event);
  gzochid_meta_server_event_type type;

  g_object_get (metaserver_event, "type", &type, NULL);

  g_mutex_lock (&state->mutex);

  switch (type)
    {
    case META_SERVER_CONNECTED:
      {
	char *connection_description = NULL;
	char *admin_server_base_url = NULL;

	g_object_get
	  (metaserver_event,
	   "connection-description", &connection_description,
	   "admin-server-base-url", &admin_server_base_url,
	   NULL);

	state->metaserver_info = gzochid_metaserver_info_new
	  (connection_description, admin_server_base_url);

	g_free (connection_description);

	if (admin_server_base_url != NULL)
	  g_free (admin_server_base_url);
	break;
      }
      
    case META_SERVER_DISCONNECTED:

      if (state->metaserver_info != NULL)
	{
	  gzochid_metaserver_info_free (state->metaserver_info);
	  state->metaserver_info = NULL;
	}
      break;
    }
  
  g_mutex_unlock (&state->mutex);
}

/* Conditionally attach the admin console's handler for meta server events to
   the data client's event source, if a data client is available. Also set the
   `has_metaclient' flag on the server state. */

static void
attach_data_client_handler (GzochidResolutionContext *res_context,
			    gzochid_server_state *state)
{
  GzochidConfiguration *configuration = gzochid_resolver_require_full
    (res_context, GZOCHID_TYPE_CONFIGURATION, NULL);
  GzochidMetaClientContainer *container = gzochid_resolver_require_full
    (res_context, GZOCHID_TYPE_META_CLIENT_CONTAINER, NULL);
  GzochidMetaClient *metaclient = NULL;

  g_object_get (container, "metaclient", &metaclient, NULL);
  
  if (metaclient != NULL)
    {
      /* If there's a meta server configured, a data client object should be
	 available. Require it and grab its event source to attach the handler
	 for meta server events. */
      
      gzochid_event_source *event_source = NULL;

      g_object_get (metaclient, "event-source", &event_source, NULL);
      gzochid_event_attach (event_source, handle_metaserver_event, state);
      g_source_unref ((GSource *) event_source);

      g_object_unref (metaclient);

      state->has_metaclient = TRUE;
    }

  g_object_unref (container);
  g_object_unref (configuration);
}

void
gzochid_httpd_app_register_handlers (GzochidHttpServer *http_server,
				     GzochidGameServer *game_server,
				     GzochidResolutionContext *res_context)
{
  gzochid_httpd_partial *apps_root = NULL;
  gzochid_httpd_partial *oids_root = NULL;

  gzochid_server_state *state = gzochid_server_state_new ();

  attach_data_client_handler (res_context, state);
  
  gzochid_httpd_add_terminal (http_server, "/", hello_world, state);
  gzochid_httpd_add_terminal (http_server, "/app/", list_apps, game_server);

  apps_root = gzochid_httpd_add_continuation
    (http_server, "/app/([^/]+)", bind_app, game_server);

  gzochid_httpd_append_terminal (apps_root, "/?", app_info, state);

  /* Don't register handlers for dumping contents of the store if the server's
     running in distribured mode. */
  
  if (!state->has_metaclient)
    {
      gzochid_httpd_append_terminal (apps_root, "/oids/", list_oids, NULL);
      oids_root = gzochid_httpd_append_continuation
	(apps_root, "/oids/", null_continuation, NULL);
  
      gzochid_httpd_append_terminal
	(oids_root, "([a-f0-9]+)", render_oid, NULL);

      gzochid_httpd_append_terminal (apps_root, "/names/", list_names, NULL);
    }
}
