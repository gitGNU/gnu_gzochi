/* sessionserver.c: Session server for gzochi-metad
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

#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "meta-protocol.h"
#include "nodemap.h"
#include "nodemap-mem.h"
#include "sessionserver.h"
#include "socket.h"

/* Boilerplate setup for the session server object. */

/* The session server object. */

struct _GzochiMetadSessionServer
{
  GObject parent; /* The parent struct, for casting. */

  gzochi_metad_nodemap *nodemap; /* The session-to-node map. */

  /* Mapping of node id -> `gzochid_client_socket'. */
  
  GHashTable *connected_servers; 
};

G_DEFINE_TYPE (GzochiMetadSessionServer, gzochi_metad_session_server,
	       G_TYPE_OBJECT);

static void
gzochi_metad_session_server_init (GzochiMetadSessionServer *self)
{
  /* The use of the memory-backed nodemap implementation is hard-coded for now.
     In the future, it may be conditional and based on configuration. */
  
  self->nodemap = gzochi_metad_nodemap_mem_new ();
  self->connected_servers = g_hash_table_new_full
    (g_int_hash, g_int_equal, (GDestroyNotify) free, NULL);
}

static void
gzochi_metad_session_server_finalize (GObject *gobject)
{
  GzochiMetadSessionServer *server = GZOCHI_METAD_SESSION_SERVER (gobject);

  gzochi_metad_nodemap_mem_free (server->nodemap);
  g_hash_table_destroy (server->connected_servers);
}

static void
gzochi_metad_session_server_class_init (GzochiMetadSessionServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochi_metad_session_server_finalize;
}

/* Returns a heap-allocated pointer to a four-byte buffer containing the 
   specified int value. The buffer should be freed via `free' when no longer in
   use. */

static inline int *
copy_int (const int v)
{
  int *int_copy = malloc (sizeof (int));
  *int_copy = v;
  return int_copy;
}

void
gzochi_metad_sessionserver_server_connected
(GzochiMetadSessionServer *sessionserver, int node_id,
 gzochid_client_socket *sock, GError **err)
{
  if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
    g_set_error
      (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
       GZOCHI_METAD_SESSIONSERVER_ERROR_ALREADY_CONNECTED,
       "Node with id %d is already connected.", node_id);

  else g_hash_table_insert
	 (sessionserver->connected_servers, copy_int (node_id), sock);
}

void
gzochi_metad_sessionserver_server_disconnected
(GzochiMetadSessionServer *sessionserver, int node_id, GError **err)
{
  if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
    {
      gzochi_metad_nodemap_iface *iface =
	GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);

      /* Remove all the sessions mapped to the disconnected server. */
      
      iface->unmap_all (sessionserver->nodemap, node_id);
      g_hash_table_remove (sessionserver->connected_servers, &node_id);      
    }
  else g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
		    GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
		    "No node with id %d connected.", node_id);
}

/* Translates the specified `GError' from the nodemap API (which must be
   non-`NULL' and have error domain `GZOCHI_METAD_NODEMAP_ERROR') to an
   equivalent `GError' in the `GZOCHI_METAD_SESSIONSERVER_ERROR' domain, setting
   the specified output error object if it is non-`NULL'. Like 
   `g_propagate_error', this function frees the input error. */

static void
propagate_nodemap_error (GError **err, GError *nodemap_err)
{
  assert (nodemap_err != NULL);
  assert (nodemap_err->domain == GZOCHI_METAD_NODEMAP_ERROR);

  int sessionserver_code;
  
  switch (nodemap_err->code)
    {
    case GZOCHI_METAD_NODEMAP_ERROR_ALREADY_MAPPED:
      sessionserver_code = GZOCHI_METAD_SESSIONSERVER_ERROR_ALREADY_CONNECTED;
      break;
    case GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED:
      sessionserver_code = GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED;
      break;
    default: sessionserver_code = GZOCHI_METAD_SESSIONSERVER_ERROR_FAILED;
    }

  g_set_error
    (err, GZOCHI_METAD_SESSIONSERVER_ERROR, sessionserver_code,
     "Session mapping error: %s", nodemap_err->message);

  g_error_free (nodemap_err);
}

void
gzochi_metad_sessionserver_session_connected
(GzochiMetadSessionServer *sessionserver, int node_id, char *app,
 guint64 session_id, GError **err)
{
  if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
    {
      GError *tmp_err = NULL;
      gzochi_metad_nodemap_iface *iface =
	GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);

      iface->map_session
	(sessionserver->nodemap, app, session_id, node_id, &tmp_err);

      if (tmp_err != NULL)
	propagate_nodemap_error (err, tmp_err);
    }
  else g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
		    GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
		    "No node with id %d connected.", node_id);
}

void
gzochi_metad_sessionserver_session_disconnected
(GzochiMetadSessionServer *sessionserver, char *app, guint64 session_id,
 GError **err)
{
  GError *tmp_err = NULL;
  gzochi_metad_nodemap_iface *iface =
    GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);

  iface->unmap_session (sessionserver->nodemap, app, session_id, &tmp_err);

  if (tmp_err != NULL)
    propagate_nodemap_error (err, tmp_err);
}

void
gzochi_metad_sessionserver_relay_disconnect
(GzochiMetadSessionServer *sessionserver, char *app, guint64 session_id,
 GError **err)
{
  GError *tmp_err = NULL;
  gzochi_metad_nodemap_iface *iface =
    GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);
  int node_id = iface->lookup_session
    (sessionserver->nodemap, app, session_id, &tmp_err);

  if (tmp_err == NULL)
    {
      if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
	{
	  size_t app_len = strlen (app);

	  /* Length of app name plus terminating `NULL' byte, plus length 
	     prefix, plus opcode, plus 8-byte session oid. */
	     
	  size_t total_len = app_len + 12;
	  gzochid_client_socket *sock = g_hash_table_lookup
	    (sessionserver->connected_servers, &node_id);

	  unsigned char *buf = malloc (sizeof (unsigned char) * total_len);

	  gzochi_common_io_write_short (total_len - 3, buf, 0);
	  buf[2] = GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_TO;
	  memcpy (buf + 3, app, app_len + 1); 
	  gzochi_common_io_write_long (session_id, buf, app_len + 4);

	  gzochid_client_socket_write (sock, buf, total_len);

	  free (buf);
	}
      else g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
			GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
			"No node with id %d connected.", node_id);
    }
  else propagate_nodemap_error (err, tmp_err);
}

void
gzochi_metad_sessionserver_relay_message
(GzochiMetadSessionServer *sessionserver, char *app, guint64 session_id,
 GBytes *msg, GError **err)
{
  GError *tmp_err = NULL;
  gzochi_metad_nodemap_iface *iface =
    GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);
  int node_id = iface->lookup_session
    (sessionserver->nodemap, app, session_id, &tmp_err);

  if (tmp_err == NULL)
    {
      if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
	{
	  size_t app_len = strlen (app);

	  gsize msg_len = 0;
	  gconstpointer msg_data = g_bytes_get_data (msg, &msg_len);

	  /* Length of app name plus terminating `NULL' byte, plus length 
	     prefix, plus opcode, plus 8-byte session oid, plus 2-byte message
	     length prefix, plus message bytes. */

	  size_t total_len = app_len + msg_len + 14;
	  unsigned char *buf = malloc (sizeof (unsigned char) * total_len);

	  gzochid_client_socket *sock = g_hash_table_lookup
	    (sessionserver->connected_servers, &node_id);

	  gzochi_common_io_write_short (total_len - 3, buf, 0);
	  buf[2] = GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_TO;
	  memcpy (buf + 3, app, app_len + 1);
	  
	  gzochi_common_io_write_long (session_id, buf, app_len + 4);
	  gzochi_common_io_write_short (msg_len, buf, app_len + 12);

	  memcpy (buf + app_len + 14, msg_data, msg_len);
	  
	  gzochid_client_socket_write (sock, buf, total_len);

	  free (buf);
	}
      else g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
			GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
			"No node with id %d connected.", node_id);
    }
  else propagate_nodemap_error (err, tmp_err);
}

GQuark
gzochi_metad_sessionserver_error_quark ()
{
  return g_quark_from_static_string ("gzochi-metad-sessionserver-error-quark");
}
