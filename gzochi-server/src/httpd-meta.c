/* httpd-meta.c: Handler implementations for the gzochi-metad admin web console
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "dataserver.h"
#include "event.h"
#include "event-meta.h"
#include "httpd.h"
#include "httpd-meta.h"
#include "resolver.h"

#define HEADER "  <head><title>gzochi-metad v" VERSION "</title></head>"

#define GREETING \
  "<h1>Hello, browser!</h1>\n" \
  "<p>This is the administrative web console for the gzochid meta server. " \
  "You can use this console to iterrogate the state of the meta server and " \
  "and its clients, as well as the game applications they manage.</p>"

/* Holds information about a connected application server. */

struct _gzochi_metad_client_info
{
  guint node_id; /* The server-assigned node id. */
  char *connection_description; /* The socket address of the client. */

  /* The base URL of the meta server's admin HTTP console, or `NULL' if the
     meta server didn't provide an address. */

  char *admin_server_base_url;

  /* String representation of client connection time. */

  char *connection_timestamp_str; 
};

typedef struct _gzochi_metad_client_info gzochi_metad_client_info;

/* Construct and return new `gzochi_metad_client_info' instance, copying the
   specified connection description and admin server base URL (which can be 
   `NULL'). The returned pointer should be freed via 
   `gzochi_metad_client_info_free' when no longer in use. */

static gzochi_metad_client_info *
gzochi_metad_client_info_new (guint node_id, const char *connection_description,
			      const char *admin_server_base_url,
			      GTimeVal connection_timestamp)
{
  gzochi_metad_client_info *info = malloc (sizeof (gzochi_metad_client_info));

  info->node_id = node_id;
  info->connection_description = strdup (connection_description);

  if (admin_server_base_url != NULL)  
    info->admin_server_base_url = strdup (admin_server_base_url);
  else info->admin_server_base_url = NULL;

  info->connection_timestamp_str =
    g_time_val_to_iso8601 (&connection_timestamp);
  
  return info;
}

/* Release the memory associated with the specified `gzochi_metad_client_info'
   object. */

static void
gzochi_metad_client_info_free (gpointer data)
{
  gzochi_metad_client_info *info = data;

  free (info->connection_description);

  if (info->admin_server_base_url != NULL)
    free (info->admin_server_base_url);

  g_free (info->connection_timestamp_str);
  
  free (info);
}

/* The state of the gzochi meta server, in terms of the information displayed 
   on the admin console. This is effectively an "offline" representation, to 
   decouple the rendering of the admin console from what's actually going on in
   the meta server. */

struct _gzochi_metad_server_state
{
  /* Table of node ids to `gzochi_metad_client_info' structs representing 
     connected application server nodes. */

  GHashTable *connected_servers;

  GMutex mutex; /* Mutex to protect updates to the state object. */
};

typedef struct _gzochi_metad_server_state gzochi_metad_server_state;

/* Construct and return a pointer to a new `gzochi_metad_server_state' 
   object. */

static gzochi_metad_server_state *
gzochi_metad_server_state_new ()
{
  gzochi_metad_server_state *state =
    malloc (sizeof (gzochi_metad_server_state));

  state->connected_servers = g_hash_table_new_full
    (g_int_hash, g_int_equal, NULL, gzochi_metad_client_info_free);
  g_mutex_init (&state->mutex);
  
  return state;
}

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

/*
  A `GHashFunc' implementation that takes a `GString' as its user data pointer
  and renders the `gzochi_metad_client_info' values to it.

  Callers must hold the meta server state mutex.
*/

static void
render_server (gpointer key, gpointer value, gpointer user_data)
{
  GString *response_str = user_data;
  gzochi_metad_client_info *info = value;  
  
  g_string_append (response_str, "<tr>\n");
  g_string_append_printf (response_str, "  <td>%d</td>\n", info->node_id);

  if (info->admin_server_base_url != NULL)  
    g_string_append_printf
      (response_str, "  <td><a href=\"%s\">%s</a></td>\n",
       info->admin_server_base_url, info->connection_description);
  else g_string_append_printf
	 (response_str, "  <td>%s</td>\n", info->connection_description);

  g_string_append_printf
    (response_str, "  <td>%s</td>\n", info->connection_timestamp_str);
  
  g_string_append (response_str, "</tr>\n");
}

/* Renders a table of connected client servers and summary details to the 
   specified output buffer. */

static void
list_servers (GString *response_str, const gzochi_metad_server_state *state)
{
  int num_servers = g_hash_table_size (state->connected_servers);
  
  g_string_append (response_str, "<table>\n");
  g_string_append (response_str, "  <tr>\n");
  g_string_append_printf
    (response_str, "    <th rowspan=\"%d\" />\n", num_servers + 1);
  g_string_append (response_str, "    <th>node id</th>\n");
  g_string_append (response_str, "    <th>server address</th>\n");
  g_string_append (response_str, "    <th>connection timestamp</th>\n");
  g_string_append (response_str, "  </tr>\n");
  
  g_hash_table_foreach (state->connected_servers, render_server, response_str);

  g_string_append (response_str, "  <tr>\n");
  g_string_append (response_str, "    <th>Total:</th>\n");
  g_string_append_printf
    (response_str, "    <td>servers: %d</td><td /><td /><td />\n", num_servers);
  g_string_append (response_str, "  </tr>\n");
  g_string_append (response_str, "</table>\n");
}

static void
hello_world (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	     gpointer request_context, gpointer user_data)
{
  GString *response_str = g_string_new (NULL);
  gzochi_metad_server_state *state = user_data;
  int num_servers = 0;
  
  append_header (response_str);
  g_string_append (response_str, GREETING);
  g_string_append (response_str, "<h2>Application servers</h2>");

  g_mutex_lock (&state->mutex);

  num_servers = g_hash_table_size (state->connected_servers);
  
  append_header (response_str);

  if (num_servers > 0)
    list_servers (response_str, state);
  else g_string_append (response_str, "<p>No connected clients.</p>\n");
  
  append_footer (response_str);

  g_mutex_unlock (&state->mutex);
    
  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

/* `gzochid_event_handler' implementation to update the admin console's
   representation of the meta server state in response to client events. */

static void
handle_client_event (GzochidEvent *event, gpointer user_data)
{
  gzochi_metad_server_state *state = user_data;  
  GzochiMetadClientEvent *client_event = GZOCHI_METAD_CLIENT_EVENT (event);
  gzochi_metad_client_event_type type;
  guint node_id = 0;

  g_object_get (client_event, "type", &type, "node-id", &node_id, NULL);

  g_mutex_lock (&state->mutex);
  
  switch (type)
    {
    case CLIENT_CONNECTED:
      {
	char *connection_description = NULL;
	char *admin_server_base_url = NULL;
	gzochi_metad_client_info *info = NULL;
	GTimeVal connection_timestamp;
	guint64 timestamp_us;
	
	g_object_get
	  (client_event,
	   "connection-description", &connection_description,
	   "admin-server-base-url", &admin_server_base_url,
	   "timestamp-us", &timestamp_us,
	   NULL);

	connection_timestamp.tv_sec = timestamp_us / 1000000;
	connection_timestamp.tv_usec = timestamp_us % 1000000;
	
	info = gzochi_metad_client_info_new
	  (node_id, connection_description, admin_server_base_url,
	   connection_timestamp);
	
	g_hash_table_insert (state->connected_servers, &info->node_id, info);

	g_free (connection_description);

	if (admin_server_base_url != NULL)
	  g_free (admin_server_base_url);
	
	break;
      }

    case CLIENT_DISCONNECTED:
      g_hash_table_remove (state->connected_servers, &node_id);
      break;
    };

  g_mutex_unlock (&state->mutex);
}

void
gzochid_httpd_meta_register_handlers (GzochidHttpServer *httpd_context,
				      GzochidResolutionContext *res_context)
{
  gzochi_metad_server_state *state = gzochi_metad_server_state_new ();
  GzochiMetadDataServer *data_server = gzochid_resolver_require_full
    (res_context, GZOCHI_METAD_TYPE_DATA_SERVER, NULL);
  gzochid_event_source *event_source = NULL;
  
  g_object_get (data_server, "event-source", &event_source, NULL);
  gzochid_event_attach (event_source, handle_client_event, state);
  g_source_unref ((GSource *) event_source);

  g_object_unref (data_server);
  
  gzochid_httpd_add_terminal (httpd_context, "/", hello_world, state);
}
