/* sessionclient.c: Sessionserver client for gzochid
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

#include "app.h"
#include "game.h"
#include "game-protocol.h"
#include "log.h"
#include "meta-protocol.h"
#include "sessionclient.h"
#include "socket.h"
#include "util.h"

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif /* G_LOG_DOMAIN */
#define G_LOG_DOMAIN "gzochid.sessionclient"

/* The data client object. */

struct _GzochidSessionClient
{
  GObject parent_instance;

  GzochidGameServer *game_server;
  
  /* The reconnectable socket wrapping the connection to the metaserver, to be
     used for writes. This field is "inherited" from the metaclient. */
  
  gzochid_reconnectable_socket *socket;
};

/* Boilerplate setup for the session client object. */

G_DEFINE_TYPE (GzochidSessionClient, gzochid_session_client, G_TYPE_OBJECT);

enum gzochid_session_client_properties
  {
    PROP_GAME_SERVER = 1,
    PROP_SOCKET,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_session_client_dispose (GObject *object)
{
  GzochidSessionClient *sessionclient = GZOCHID_SESSION_CLIENT (object);

  g_object_unref (sessionclient->game_server);

  G_OBJECT_CLASS (gzochid_session_client_parent_class)->dispose (object);
}

static void
gzochid_session_client_set_property (GObject *object, guint property_id,
				     const GValue *value, GParamSpec *pspec)
{
  GzochidSessionClient *self = GZOCHID_SESSION_CLIENT (object);

  switch (property_id)
    {
    case PROP_GAME_SERVER:
      self->game_server = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_SOCKET:
      self->socket = g_value_get_pointer (value);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_session_client_class_init (GzochidSessionClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_session_client_dispose;
  object_class->set_property = gzochid_session_client_set_property;

  obj_properties[PROP_GAME_SERVER] = g_param_spec_object
    ("game-server", "game-server", "The game protocol server",
     GZOCHID_TYPE_GAME_SERVER, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  obj_properties[PROP_SOCKET] = g_param_spec_pointer
    ("reconnectable-socket", "socket", "The meta client's reconnectable socket",
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_session_client_init (GzochidSessionClient *self)
{
}

/* End boilerplate. */

void
gzochid_sessionclient_session_connected (GzochidSessionClient *client,
					 const char *app, guint64 session_id)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 12;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
    
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_SESSION_PROTOCOL_SESSION_CONNECTED;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (session_id, relay_msg, app_len + 4);

  gzochid_trace ("Notifying metaserver of connected session %s/%"
		 G_GUINT64_FORMAT ".", app, session_id);
  
  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_sessionclient_session_disconnected (GzochidSessionClient *client,
					    const char *app, guint64 session_id)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 12;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
  
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_SESSION_PROTOCOL_SESSION_DISCONNECTED;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (session_id, relay_msg, app_len + 4);

  gzochid_trace ("Notifying metaserver of disconnect from session %s/%"
		 G_GUINT64_FORMAT ".", app, session_id);
  
  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_sessionclient_relay_disconnect_from (GzochidSessionClient *client,
					     const char *app,
					     guint64 session_id)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 12;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
    
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (session_id, relay_msg, app_len + 4);

  gzochid_trace ("Relaying disconnect for non-local session %s/%"
		 G_GUINT64_FORMAT ".", app, session_id);

  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_sessionclient_relay_message_from (GzochidSessionClient *client,
					  const char *app, guint64 session_id,
					  GBytes *msg_bytes)
{
  size_t msg_len = 0;
  const unsigned char *msg = g_bytes_get_data (msg_bytes, &msg_len);

  size_t app_len = strlen (app);
  size_t total_len = app_len + msg_len + 14;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
  
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (session_id, relay_msg, app_len + 4);
  gzochi_common_io_write_short (msg_len, relay_msg, app_len + 12);
  memcpy (relay_msg + app_len + 14, msg, msg_len);

  gzochid_trace ("Relaying message for non-local session %s/%" G_GUINT64_FORMAT
		 ".", app, session_id);
  
  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_sessionclient_relay_disconnect_to (GzochidSessionClient *client,
					   const char *app, guint64 session_id,
					   GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);

  if (app_context != NULL)
    {
      g_mutex_lock (&app_context->client_mapping_lock);

      if (g_hash_table_contains (app_context->oids_to_clients, &session_id))
	{
	  gzochid_trace ("Relaying disconnect to local session %s/%"
			 G_GUINT64_FORMAT ".", app, session_id);

	  gzochid_game_client_disconnect
	    (g_hash_table_lookup (app_context->oids_to_clients, &session_id));
	}
      else
	{
	  g_debug
	    ("Unable to relay disconnect to unmapped session %s/%"
	     G_GUINT64_FORMAT ".", app, session_id);
	  g_set_error
	    (err, GZOCHID_SESSIONCLIENT_ERROR,
	     GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED,
	     "Unable to relay disconnect to unmapped session %s/%"
	     G_GUINT64_FORMAT ".", app, session_id);
	}

      g_mutex_unlock (&app_context->client_mapping_lock);
    }
  else g_set_error
	 (err, GZOCHID_SESSIONCLIENT_ERROR,
	  GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay disconnect to unknown application %s.", app);
}

void
gzochid_sessionclient_relay_message_to (GzochidSessionClient *client,
					const char *app, guint64 session_id,
					GBytes *msg, GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);

  if (app_context != NULL)
    {
      g_mutex_lock (&app_context->client_mapping_lock);

      if (g_hash_table_contains (app_context->oids_to_clients, &session_id))
	{
	  size_t msg_size = 0;
	  gconstpointer msg_data = g_bytes_get_data (msg, &msg_size);
	  
	  gzochid_game_client *game_client = g_hash_table_lookup
	    (app_context->oids_to_clients, &session_id);

	  gzochid_trace ("Relaying message to local session %s/%"
			 G_GUINT64_FORMAT ".", app, session_id);

	  gzochid_game_client_send
	    (game_client, (unsigned char *) msg_data, msg_size);
	}
      else
	{
	  g_debug
	    ("Unable to relay message to unmapped session %s/%" G_GUINT64_FORMAT
	     ".", app, session_id);
	  g_set_error
	    (err, GZOCHID_SESSIONCLIENT_ERROR,
	     GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED,
	     "Unable to relay message to unmapped session %s/%" G_GUINT64_FORMAT
	     ".", app, session_id);
	}
      
      g_mutex_unlock (&app_context->client_mapping_lock);
    }
  else g_set_error
	 (err, GZOCHID_SESSIONCLIENT_ERROR,
	  GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay message to unknown application %s.", app);
}

GQuark
gzochid_sessionclient_error_quark ()
{
  return g_quark_from_static_string ("gzochid-sessionclient-error-quark");
}
