/* httpd-app.c: Handler implementations for the gzochid admin web console
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

#include <ctype.h>
#include <glib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "game.h"
#include "httpd.h"
#include "httpd-app.h"

/* The following declaraitons support rendering the contents of an object in the
   object store. */

#define OID_PREFIX_LEN 26
#define OID_SUFFIX_LEN 29
#define OID_LINE_LEN 80

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

static void
hello_world (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	     gpointer request_context, gpointer user_data)
{
  GString *response_str = g_string_new (NULL);

  g_string_append (response_str, "<html><body>");
  g_string_append (response_str, GREETING);
  g_string_append (response_str, "<a href=\"app/\">app/</a>");
  g_string_append (response_str, "</body></html>");

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

static void
list_apps (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	   gpointer request_context, gpointer user_data)
{
  gzochid_game_context *game_context = user_data;
  GList *apps = gzochid_game_context_get_applications (game_context);

  if (apps == NULL)
    gzochid_http_write_response
      (sink, 404, "<html><body>No applications.</body></html>", 22);
  else 
    {
      GList *apps_ptr = apps;
      GString *response_str = g_string_new (NULL);

      g_string_append (response_str, "<html>\n");
      g_string_append (response_str, "  <body>\n");
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
      g_string_append (response_str, "  </body>\n");
      g_string_append (response_str, "</html>");

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
  gzochid_game_context *game_context = user_data;
  gzochid_application_context *app_context =
    gzochid_game_context_lookup_application (game_context, app);

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

  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, "  <body>\n");

  g_string_append_printf 
    (response_str, "    <h1>%s</h1>\n", app_context->descriptor->name);
  g_string_append_printf 
    (response_str, "    <p>%s</p>\n", app_context->descriptor->description);
  g_string_append (response_str, "    <h2>Application data</h2>\n");
  g_string_append_printf 
    (response_str, "    <a href=\"/app/%s/names/\">names</a><br />\n", 
     app_context->descriptor->name);
  g_string_append_printf 
    (response_str, "    <a href=\"/app/%s/oids/\">oids</a><br />\n", 
     app_context->descriptor->name);

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
  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>");

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
  char *k = APP_STORAGE_INTERFACE (app_context)->first_key 
    (app_context->oids, &klen);
  
  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, "  <body>\n");
  g_string_append_printf 
    (response_str, "    <h1>%s</h1><br />\n", app_context->descriptor->name);
  g_string_append (response_str, "    <ul>\n");
  
  while (k != NULL)
    {
      char *next_k = NULL;
      
      g_string_append (response_str, "      <li><a href=\"");
      g_string_append_len (response_str, k, klen - 1);
      g_string_append (response_str, "\">");
      g_string_append_len (response_str, k, klen - 1);
      g_string_append (response_str, "</a></li>\n");
      
      next_k = APP_STORAGE_INTERFACE (app_context)->next_key 
	(app_context->oids, k, klen, &klen);
      
      free (k);
      k = next_k;
    }
  
  g_string_append (response_str, "    </ul>\n");
  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>");

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

  char *data = APP_STORAGE_INTERFACE (app_context)->get
    (app_context->oids, oid_str, strlen (oid_str) + 1, &data_length);
  
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
  char *k = APP_STORAGE_INTERFACE (app_context)
    ->first_key (app_context->names, &klen);

  g_string_append (response_str, "<html>\n");
  g_string_append (response_str, "  <body>\n");
  g_string_append_printf 
    (response_str, "    <h1>%s</h1><br />\n", app_context->descriptor->name);
  g_string_append (response_str, "    <ul>\n");

  while (k != NULL)
    {
      char *next_k = NULL;

      g_string_append (response_str, "      <li>");
      g_string_append_len (response_str, k, klen);
      g_string_append (response_str, "</li>\n");
      
      next_k = APP_STORAGE_INTERFACE (app_context)->next_key 
	(app_context->names, k, klen, &klen);

      free (k);
      k = next_k;
    }

  g_string_append (response_str, "    </ul>\n");
  g_string_append (response_str, "  </body>\n");
  g_string_append (response_str, "</html>");

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

void
gzochid_httpd_app_register_handlers (GzochidHttpServer *http_server,
				     gzochid_game_context *game_context)
{
  gzochid_httpd_partial *apps_root = NULL;
  gzochid_httpd_partial *oids_root = NULL;
  
  gzochid_httpd_add_terminal (http_server, "/", hello_world, NULL);
  gzochid_httpd_add_terminal (http_server, "/app/", list_apps, game_context);

  apps_root = gzochid_httpd_add_continuation
    (http_server, "/app/([^/]+)", bind_app, game_context);

  gzochid_httpd_append_terminal (apps_root, "/?", app_info, NULL);
  gzochid_httpd_append_terminal (apps_root, "/oids/", list_oids, NULL);
  oids_root = gzochid_httpd_append_continuation
    (apps_root, "/oids/", null_continuation, NULL);
  
  gzochid_httpd_append_terminal (oids_root, "([a-f0-9]+)", render_oid, NULL);

  gzochid_httpd_append_terminal (apps_root, "/names/", list_names, NULL);
}
