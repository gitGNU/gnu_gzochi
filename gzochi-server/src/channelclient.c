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
#include "channel.h"
#include "channelclient.h"
#include "game.h"
#include "meta-protocol.h"
#include "socket.h"
#include "util.h"

/* The channel client object. */

struct _GzochidChannelClient
{
  GObject parent_instance;

  GzochidGameServer *game_server;
  
  /* The reconnectable socket wrapping the connection to the metaserver, to be
     used for writes. This field is "inherited" from the metaclient. */
  
  gzochid_reconnectable_socket *socket;
};

/* Boilerplate setup for the channel client object. */

G_DEFINE_TYPE (GzochidChannelClient, gzochid_channel_client, G_TYPE_OBJECT);

enum gzochid_channel_client_properties
  {
    PROP_GAME_SERVER = 1,
    PROP_SOCKET,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_channel_client_dispose (GObject *object)
{
  GzochidChannelClient *channelclient = GZOCHID_CHANNEL_CLIENT (object);

  g_object_unref (channelclient->game_server);

  G_OBJECT_CLASS (gzochid_channel_client_parent_class)->dispose (object);
}

static void
gzochid_channel_client_set_property (GObject *object, guint property_id,
				     const GValue *value, GParamSpec *pspec)
{
  GzochidChannelClient *self = GZOCHID_CHANNEL_CLIENT (object);

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
gzochid_channel_client_class_init (GzochidChannelClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_channel_client_dispose;
  object_class->set_property = gzochid_channel_client_set_property;

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
gzochid_channel_client_init (GzochidChannelClient *self)
{
}

/* End boilerplate. */

void
gzochid_channelclient_relay_join_from (GzochidChannelClient *client,
				       const char *app, guint64 channel_oid,
				       guint64 session_oid)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 20;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
    
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_CHANNEL_PROTOCOL_RELAY_JOIN_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (channel_oid, relay_msg, app_len + 4);
  gzochi_common_io_write_long (session_oid, relay_msg, app_len + 12);

  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_channelclient_relay_leave_from (GzochidChannelClient *client,
					const char *app, guint64 channel_oid,
					guint64 session_oid)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 20;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
    
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_CHANNEL_PROTOCOL_RELAY_LEAVE_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (channel_oid, relay_msg, app_len + 4);
  gzochi_common_io_write_long (session_oid, relay_msg, app_len + 12);

  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_channelclient_relay_close_from (GzochidChannelClient *client,
					const char *app, guint64 channel_oid)
{
  size_t app_len = strlen (app);
  size_t total_len = app_len + 12;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
    
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_CHANNEL_PROTOCOL_RELAY_CLOSE_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (channel_oid, relay_msg, app_len + 4);

  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_channelclient_relay_message_from (GzochidChannelClient *client,
					  const char *app, guint64 channel_oid,
					  GBytes *msg_bytes)
{
  size_t msg_len = 0;
  const unsigned char *msg = g_bytes_get_data (msg_bytes, &msg_len);

  size_t app_len = strlen (app);
  size_t total_len = app_len + msg_len + 14;
  unsigned char *relay_msg = malloc (sizeof (unsigned char) * total_len);
  
  gzochi_common_io_write_short (total_len - 3, relay_msg, 0);
  relay_msg[2] = GZOCHID_CHANNEL_PROTOCOL_RELAY_MESSAGE_FROM;
  memcpy (relay_msg + 3, app, app_len + 1);
  gzochi_common_io_write_long (channel_oid, relay_msg, app_len + 4);
  gzochi_common_io_write_short (msg_len, relay_msg, app_len + 12);
  memcpy (relay_msg + app_len + 14, msg, msg_len);
  
  gzochid_reconnectable_socket_write (client->socket, relay_msg, total_len);
  
  free (relay_msg);
}

void
gzochid_channelclient_relay_join_to (GzochidChannelClient *client,
				     const char *app, guint64 channel_oid,
				     guint64 session_oid, GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);

  if (app_context != NULL)
    {
      GError *local_err = NULL;

      gzochid_channel_join_direct
	(app_context, channel_oid, session_oid, &local_err);

      if (local_err != NULL)
	{
	  g_debug
	    ("Ignoring remote channel JOIN notification: %s",
	     local_err->message);
	  g_error_free (local_err);
	}
    }
  else g_set_error
	 (err, GZOCHID_CHANNELCLIENT_ERROR,
	  GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay channel join to unknown application %s.", app);
}

void
gzochid_channelclient_relay_leave_to (GzochidChannelClient *client,
				      const char *app, guint64 channel_oid,
				      guint64 session_oid, GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);

  if (app_context != NULL)
    {
      GError *local_err = NULL;

      gzochid_channel_leave_direct
	(app_context, channel_oid, session_oid, &local_err);

      if (local_err != NULL)
	{
	  g_debug
	    ("Ignoring remote channel LEAVE notification: %s",
	     local_err->message);
	  g_error_free (local_err);
	}
    }
  else g_set_error
	 (err, GZOCHID_CHANNELCLIENT_ERROR,
	  GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay channel leave to unknown application %s.", app);
}

void
gzochid_channelclient_relay_close_to (GzochidChannelClient *client,
				      const char *app, guint64 channel_oid,
				      GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);
  
  if (app_context != NULL)
    gzochid_channel_close_direct (app_context, channel_oid);
  else g_set_error
	 (err, GZOCHID_CHANNELCLIENT_ERROR,
	  GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay channel close to unknown application %s.", app);
}

void
gzochid_channelclient_relay_message_to (GzochidChannelClient *client,
					const char *app, guint64 channel_oid,
					GBytes *msg_bytes, GError **err)
{
  gzochid_application_context *app_context =
    gzochid_game_server_lookup_application (client->game_server, app);
  
  if (app_context != NULL)
    gzochid_channel_message_direct (app_context, channel_oid, msg_bytes);
  else g_set_error
	 (err, GZOCHID_CHANNELCLIENT_ERROR,
	  GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED,
	  "Unable to relay channel message to unknown application %s.", app);
}

GQuark
gzochid_channelclient_error_quark ()
{
  return g_quark_from_static_string ("gzochid-channelclient-error-quark");
}
