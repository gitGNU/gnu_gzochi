/* httpd.c: Embedded informational web server for gzochid
 * Copyright (C) 2013 Julian Graham
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

#include <ctype.h>
#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>

#include <microhttpd.h>

#include "admin.h"
#include "app.h"
#include "context.h"
#include "fsm.h"
#include "game.h"
#include "gzochid.h"
#include "log.h"
#include "httpd.h"
#include "storage.h"
#include "threads.h"

#define OID_PREFIX_LEN 26
#define OID_SUFFIX_LEN 29
#define OID_LINE_LEN 80

struct data_state 
{
  char *data;
  int data_offset;
  int data_length;

  char *current_line;
  int current_line_offset;
  int current_line_length;
};

static void next_line (struct data_state *state)
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

static int write_data_line (void *cls, uint64_t pos, char *buf, int max)
{
  int n = 0;
  struct data_state *state = (struct data_state *) cls;

  if (state->current_line == NULL)
    next_line (state);
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

static void free_data_state (void *ptr)
{
  struct data_state *state = (struct data_state *) ptr;

  free (state->data);

  if (state->current_line != NULL)
    free (state->current_line);

  free (state);
}

static int not_found404 
(struct MHD_Connection *connection, const char *page, 
 int must_free, int must_copy)
{
  struct MHD_Response *response = MHD_create_response_from_data 
    (strlen (page), (void *) page, must_free, must_copy);
  int ret = MHD_queue_response (connection, 404, response);

  MHD_destroy_response (response);

  return ret;
}

static int not_found404_default (struct MHD_Connection *connection)
{
  return not_found404 
    (connection, "<html><body>Not found.</body></html>", FALSE, FALSE);
}

static int dispatch_oid (struct MHD_Connection *connection, 
			 gzochid_application_context *context, mpz_t oid)
{
  int ret = 0;
  size_t data_length = 0;
  char *oid_str = mpz_get_str (NULL, 16, oid);
  char *data = gzochid_storage_get 
    (context->oids, oid_str, strlen (oid_str) + 1, &data_length);
  struct data_state *state = calloc (1, sizeof (struct data_state));
  struct MHD_Response *response = NULL;

  state->data = data;
  state->data_length = data_length;

  response = MHD_create_response_from_callback 
    (-1, OID_PREFIX_LEN + OID_LINE_LEN + OID_SUFFIX_LEN, write_data_line, 
     state, free_data_state);

  ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
  MHD_destroy_response (response);
  return ret;
}

static int dispatch_oids 
(struct MHD_Connection *connection, const char *url,
 gzochid_application_context *context)
{
  int ret = 0;
  if (strlen (url) == 0)
    {
      GString *response_str = g_string_new (NULL);
      struct MHD_Response *response = NULL;
      
      size_t klen = 0;
      char *k = gzochid_storage_first_key (context->oids, &klen);
      
      g_string_append (response_str, "<html>\n");
      g_string_append (response_str, "  <body>\n");
      g_string_append (response_str, "    <ul>\n");
      
      while (k != NULL)
	{
	  char *next_k = NULL;
	  
	  g_string_append (response_str, "      <li><a href=\"");
	  g_string_append_len (response_str, k, klen - 1);
	  g_string_append (response_str, "\">");
	  g_string_append_len (response_str, k, klen - 1);
	  g_string_append (response_str, "</a></li>\n");
	  
	  next_k = gzochid_storage_next_key (context->oids, k, klen, &klen);
	  
	  free (k);
	  k = next_k;
	}

      g_string_append (response_str, "    </ul>\n");
      g_string_append (response_str, "  </body>\n");
      g_string_append (response_str, "</html>");
      
      response = MHD_create_response_from_data
	(response_str->len, response_str->str, TRUE, FALSE);
      g_string_free (response_str, FALSE);
      
      ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
      MHD_destroy_response (response);
      return ret;
    }
  else
    {
      mpz_t oid;

      mpz_init (oid);
      if (mpz_set_str (oid, url, 16) < 0)
	{	  
	  mpz_clear (oid);
	  return not_found404_default (connection);
	}

      ret = dispatch_oid (connection, context, oid);
      mpz_clear (oid);

      return ret;
    }
}

static int dispatch_names (struct MHD_Connection *connection, 
			   gzochid_application_context *context)
{
  int ret = 0;
  GString *response_str = g_string_new (NULL);
  struct MHD_Response *response = NULL;
  
  size_t klen = 0;
  char *k = gzochid_storage_first_key (context->names, &klen);

  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, "  <body>\n");
  g_string_append (response_str, "    <ul>\n");

  while (k != NULL)
    {
      char *next_k = NULL;

      g_string_append (response_str, "      <li>");
      g_string_append_len (response_str, k, klen);
      g_string_append (response_str, "</li>\n");
      
      next_k = gzochid_storage_next_key (context->names, k, klen, &klen);

      free (k);
      k = next_k;
    }

  g_string_append (response_str, "    </ul>\n");
  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>");

  response = MHD_create_response_from_data 
    (response_str->len, response_str->str, TRUE, FALSE);
  g_string_free (response_str, FALSE);
  
  ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
  MHD_destroy_response (response);
  return ret;
}

static int list_apps 
(struct MHD_Connection *connection, gzochid_game_context *game_context)
{
  struct MHD_Response *response = NULL;
  GList *apps = gzochid_game_context_get_applications (game_context);

  if (apps == NULL)
    return not_found404 
      (connection, "<html><body>No applications.</body></html>", FALSE, FALSE);
  else 
    {
      int ret = 0;
      GList *apps_ptr = apps;
      GString *response_str = g_string_new (NULL);

      g_string_append (response_str, "<html>\n");
      g_string_append (response_str, "  <body>\n");
      g_string_append (response_str, "    <ul>\n");
      
      while (apps_ptr != NULL)
	{
	  gzochid_application_context *app = 
	    (gzochid_application_context *) apps_ptr->data;
	  
  	  g_string_append_printf 
	    (response_str, 
	     "      <li><a href=\"/app/%s/\">%s</a></li><br />\n", 
	     app->descriptor->name, app->descriptor->name);

	  apps_ptr = apps_ptr->next;
	}

      g_string_append (response_str, "    </ul>\n");
      g_string_append (response_str, "  </body>\n");
      g_string_append (response_str, "</html>");

      response = MHD_create_response_from_data
	(response_str->len, response_str->str, TRUE, FALSE);
      g_string_free (response_str, FALSE);

      ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
      MHD_destroy_response (response);
      return ret;
    }
}

static int app_info 
(struct MHD_Connection *connection, gzochid_application_context *app_context)
{
  int ret = 0;
  struct MHD_Response *response = NULL;
  GString *response_str = g_string_new (NULL);

  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, "  <body>\n");

  g_string_append_printf 
    (response_str, "    <h1>%s</h1><br />\n", app_context->descriptor->name);
  g_string_append_printf 
    (response_str, "    <a href=\"/app/%s/names/\">names</a><br />\n", 
     app_context->descriptor->name);
  g_string_append_printf 
    (response_str, "    <a href=\"/app/%s/oids/\">oids</a><br />\n", 
     app_context->descriptor->name);

  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>");

  response = MHD_create_response_from_data 
    (response_str->len, response_str->str, TRUE, FALSE);
  g_string_free (response_str, FALSE);

  ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
  MHD_destroy_response (response);
  return ret;
}

static int dispatch_app (struct MHD_Connection *connection, const char *url,
			 gzochid_game_context *game_context)
{
  gzochid_application_context *app_context = NULL;
  char *rest = strchr (url, '/');
  char *name = NULL;
  int name_len = 0;
  
  if (rest == NULL)
    return not_found404_default (connection);

  name_len = rest - url;
  name = strndup (url, name_len);  
  app_context = gzochid_game_context_lookup_application (game_context, name);
  free (name);

  if (app_context != NULL)
    {
      if (strcmp (rest, "/") == 0)
	return app_info (connection, app_context);
      if (strncmp (rest, "/names/", 7) == 0)
	return dispatch_names (connection, app_context);
      else if (strncmp (rest, "/oids/", 6) == 0)
	return dispatch_oids 
	  (connection, rest + 6, app_context);
      else return not_found404_default (connection);
    }
  else return not_found404_default (connection);
}

static int dispatch_apps (struct MHD_Connection *connection, const char *url,
			  gzochid_game_context *game_context)
{
  if (strlen (url) == 0)
    return list_apps (connection, game_context);
  else return dispatch_app (connection, url, game_context);
}

static int dispatch (void *cls, struct MHD_Connection *connection, 
		     const char *url, const char *method, const char *version, 
		     const char *upload_data, size_t *upload_data_size, 
		     void **con_cls)
{
  int ret = 0;
  struct MHD_Response *response = NULL;
  gzochid_httpd_context *context = (gzochid_httpd_context *) cls;

  if (strcmp (method, "GET") != 0)
    return ret;

  if (strcmp (url, "/") == 0)
    {
      const char *page = "<html><body>Hello, browser!</body></html>";
      response = MHD_create_response_from_data 
	(strlen (page), (void *) page, FALSE, FALSE);
      ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
      MHD_destroy_response (response);
    }
  else if (strncmp (url, "/app/", 5) == 0)
    {
      gzochid_admin_context *admin_context = 
	(gzochid_admin_context *) ((gzochid_context *) context)->parent;
      gzochid_server_context *server_context =
	(gzochid_server_context *) ((gzochid_context *) admin_context)->parent; 
      gzochid_game_context *game_context = (gzochid_game_context *)
	server_context->game_context;

      return dispatch_apps (connection, url + 5, game_context);
    }
  else return not_found404_default (connection);

  return ret;
}

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_HTTPD_STATE_RUNNING);
}

static void initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_httpd_context *httpd_context = (gzochid_httpd_context *) context;
  gzochid_admin_context *admin_context = 
    (gzochid_admin_context *) context->parent;

  httpd_context->daemon = MHD_start_daemon 
    (MHD_USE_SELECT_INTERNALLY, httpd_context->port, NULL, NULL, dispatch, 
     httpd_context, MHD_OPTION_END);

  gzochid_notice ("HTTP server listening on port %d", httpd_context->port);

  gzochid_thread_pool_push 
    (admin_context->pool, initialize_async, context, NULL);
}

gzochid_httpd_context *gzochid_httpd_context_new (void)
{
  return calloc (1, sizeof (gzochid_httpd_context));
}

void gzochid_httpd_context_free (gzochid_httpd_context *context)
{
  gzochid_context_free ((gzochid_context *) context);
  free (context);
}

void gzochid_httpd_context_init 
(gzochid_httpd_context *context, gzochid_context *parent, int port)
{
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("httpd", GZOCHID_HTTPD_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_HTTPD_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_HTTPD_STATE_STOPPED, "STOPPED");

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_HTTPD_STATE_INITIALIZING, initialize, context);
   
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_HTTPD_STATE_INITIALIZING, GZOCHID_HTTPD_STATE_RUNNING);
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_HTTPD_STATE_RUNNING, GZOCHID_HTTPD_STATE_STOPPED);

  context->port = port;

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}
