/* sessionserver.c: Session server for gzochi-metad
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
#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "event-meta.h"
#include "event.h"
#include "log.h"
#include "meta-protocol.h"
#include "nodemap.h"
#include "nodemap-mem.h"
#include "sessionserver.h"
#include "socket.h"

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif /* G_LOG_DOMAIN */
#define G_LOG_DOMAIN "gzochi-metad.sessionserver"

/* Boilerplate setup for the session server object. */

/* The session server object. */

struct _GzochiMetadSessionServer
{
  GObject parent; /* The parent struct, for casting. */

  gzochi_metad_nodemap *nodemap; /* The session-to-node map. */

  /* Mapping of node id -> `gzochid_client_socket'. */
  
  GHashTable *connected_servers; 

  gzochid_event_source *event_source; /* Event source for session events. */
};

G_DEFINE_TYPE (GzochiMetadSessionServer, gzochi_metad_session_server,
	       G_TYPE_OBJECT);

enum gzochi_metad_session_server_properties
  {
    PROP_EVENT_LOOP = 1,
    PROP_EVENT_SOURCE,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochi_metad_session_server_get_property (GObject *object, guint property_id,
					  GValue *value, GParamSpec *pspec)
{
  GzochiMetadSessionServer *self = GZOCHI_METAD_SESSION_SERVER (object);

  switch (property_id)
    {
    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, self->event_source);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochi_metad_session_server_set_property (GObject *object, guint property_id,
					  const GValue *value,
					  GParamSpec *pspec)
{
  GzochiMetadSessionServer *self = GZOCHI_METAD_SESSION_SERVER (object);

  switch (property_id)
    {
    case PROP_EVENT_LOOP:

      /* Don't need to store a reference to the event loop, just attach the
	 event source to it. */
      
      gzochid_event_source_attach
	(g_value_get_object (value), self->event_source);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochi_metad_session_server_init (GzochiMetadSessionServer *self)
{
  /* The use of the memory-backed nodemap implementation is hard-coded for now.
     In the future, it may be conditional and based on configuration. */
  
  self->nodemap = gzochi_metad_nodemap_mem_new ();
  self->connected_servers = g_hash_table_new_full
    (g_int_hash, g_int_equal, g_free, NULL);
  self->event_source = gzochid_event_source_new ();
}

static void
gzochi_metad_session_server_finalize (GObject *gobject)
{
  GzochiMetadSessionServer *server = GZOCHI_METAD_SESSION_SERVER (gobject);

  gzochi_metad_nodemap_mem_free (server->nodemap);
  g_hash_table_destroy (server->connected_servers);

  g_source_destroy ((GSource *) server->event_source);
  g_source_unref ((GSource *) server->event_source);
  
  G_OBJECT_CLASS (gzochi_metad_session_server_parent_class)->finalize (gobject);
}

static void
gzochi_metad_session_server_class_init (GzochiMetadSessionServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochi_metad_session_server_finalize;
  object_class->get_property = gzochi_metad_session_server_get_property;
  object_class->set_property = gzochi_metad_session_server_set_property;

  obj_properties[PROP_EVENT_LOOP] = g_param_spec_object
    ("event-loop", "event-loop", "The global event loop",
     GZOCHID_TYPE_EVENT_LOOP, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "The session server event source",
     G_TYPE_SOURCE, G_PARAM_READABLE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
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
	 (sessionserver->connected_servers, g_memdup (&node_id, sizeof (int)),
	  sock);
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
(GzochiMetadSessionServer *sessionserver, int node_id, const char *app,
 guint64 session_id, GError **err)
{
  gzochid_trace ("Mapping connected session %s/%" G_GUINT64_FORMAT
		 " to node %d.", app, session_id, node_id);

  if (g_hash_table_contains (sessionserver->connected_servers, &node_id))
    {
      GError *tmp_err = NULL;
      gzochi_metad_nodemap_iface *iface =
	GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);

      iface->map_session
	(sessionserver->nodemap, app, session_id, node_id, &tmp_err);

      if (tmp_err != NULL)
	propagate_nodemap_error (err, tmp_err);
      else gzochid_event_dispatch
	     (sessionserver->event_source, g_object_new
	      (GZOCHI_METAD_TYPE_SESSION_EVENT,
	       "type", SESSION_CONNECTED,
	       "node-id", node_id,
	       "application", app,
	       NULL));
    }
  else g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
		    GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
		    "No node with id %d connected.", node_id);
}

void
gzochi_metad_sessionserver_session_disconnected
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
 GError **err)
{
  GError *tmp_err = NULL;
  gzochi_metad_nodemap_iface *iface =
    GZOCHI_METAD_NODEMAP_IFACE (sessionserver->nodemap);
  int node_id = 0;
  
  gzochid_trace ("Unmapping disconnected session %s/%" G_GUINT64_FORMAT ".",
		 app, session_id);

  node_id = iface->lookup_session
    (sessionserver->nodemap, app, session_id, &tmp_err);

  if (tmp_err == NULL)
    {
      iface->unmap_session (sessionserver->nodemap, app, session_id, &tmp_err);

      if (tmp_err != NULL)
	propagate_nodemap_error (err, tmp_err);
      else gzochid_event_dispatch
	     (sessionserver->event_source, g_object_new
	      (GZOCHI_METAD_TYPE_SESSION_EVENT,
	       "type", SESSION_DISCONNECTED,
	       "node-id", node_id,
	       "application", app,
	       NULL));
    }
  else propagate_nodemap_error (err, tmp_err);
}

void
gzochi_metad_sessionserver_relay_disconnect
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
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

	  gzochid_trace ("Relaying disconnect to session %s/%" G_GUINT64_FORMAT
			 " on node %d.", app, session_id, node_id);
	  
	  gzochid_client_socket_write (sock, buf, total_len);

	  free (buf);
	}
      else
	{
	  g_debug ("Failed to relay disconnect to session %s/%" G_GUINT64_FORMAT
		   " on disconnected node %d.", app, session_id, node_id);
	  g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
		       GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
		       "No node with id %d connected.", node_id);
	}
    }
  else
    {
      g_debug ("Failed to identify node mapping for session %s/%"
	       G_GUINT64_FORMAT " for disconnect relay.", app, session_id);
      propagate_nodemap_error (err, tmp_err);
    }
}

void
gzochi_metad_sessionserver_relay_message
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
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

	  gzochid_trace ("Relaying message to session %s/%" G_GUINT64_FORMAT
			 " on node %d.", app, session_id, node_id);
	  
	  gzochid_client_socket_write (sock, buf, total_len);

	  free (buf);
	}
      else
	{
	  g_debug ("Failed to relay message to session %s/%" G_GUINT64_FORMAT
		   " on disconnected node %d.", app, session_id, node_id);
	  g_set_error (err, GZOCHI_METAD_SESSIONSERVER_ERROR,
		       GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
		       "No node with id %d connected.", node_id);
	}
    }
  else
    {
      g_debug ("Failed to identify node mapping for session %s/%"
	       G_GUINT64_FORMAT " for message relay.", app, session_id);
      propagate_nodemap_error (err, tmp_err);
    }
}

GQuark
gzochi_metad_sessionserver_error_quark ()
{
  return g_quark_from_static_string ("gzochi-metad-sessionserver-error-quark");
}
