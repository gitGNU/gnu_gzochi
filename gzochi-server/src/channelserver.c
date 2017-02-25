/* channelserver.c: Channel server for gzochi-metad
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

#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "channelserver.h"
#include "log.h"
#include "meta-protocol.h"
#include "socket.h"
#include "util.h"

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif /* G_LOG_DOMAIN */
#define G_LOG_DOMAIN "gzochi-metad.channelserver"

/* Boilerplate setup for the channel server object. */

/* The channel server object. */

struct _GzochiMetadChannelServer
{
  GObject parent; /* The parent struct, for casting. */

  /* Mapping of node id -> `gzochid_client_socket'. */
  
  GHashTable *connected_servers; 
};

G_DEFINE_TYPE (GzochiMetadChannelServer, gzochi_metad_channel_server,
	       G_TYPE_OBJECT);

static void
gzochi_metad_channel_server_init (GzochiMetadChannelServer *self)
{
  self->connected_servers = g_hash_table_new_full
    (g_int_hash, g_int_equal, (GDestroyNotify) free, NULL);
}

static void
channel_server_finalize (GObject *gobject)
{
  GzochiMetadChannelServer *server = GZOCHI_METAD_CHANNEL_SERVER (gobject);

  g_hash_table_destroy (server->connected_servers);

  G_OBJECT_CLASS (gzochi_metad_channel_server_parent_class)->finalize (gobject);
}

static void
gzochi_metad_channel_server_class_init (GzochiMetadChannelServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = channel_server_finalize;
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
gzochi_metad_channelserver_server_connected
(GzochiMetadChannelServer *channelserver, int node_id,
 gzochid_client_socket *sock, GError **err)
{
  if (g_hash_table_contains (channelserver->connected_servers, &node_id))
    g_set_error
      (err, GZOCHI_METAD_CHANNELSERVER_ERROR,
       GZOCHI_METAD_CHANNELSERVER_ERROR_ALREADY_CONNECTED,
       "Node with id %d is already connected.", node_id);

  else g_hash_table_insert
	 (channelserver->connected_servers, copy_int (node_id), sock);
}

void
gzochi_metad_channelserver_server_disconnected
(GzochiMetadChannelServer *channelserver, int node_id, GError **err)
{
  if (g_hash_table_contains (channelserver->connected_servers, &node_id))
    g_hash_table_remove (channelserver->connected_servers, &node_id);      

  else g_set_error (err, GZOCHI_METAD_CHANNELSERVER_ERROR,
		    GZOCHI_METAD_CHANNELSERVER_ERROR_NOT_CONNECTED,
		    "No node with id %d connected.", node_id);
}

/* A `GHFunc' implementation for use against the channel server's node id-to-
   client socket mapping. The "user data" pointer should be a pointer array
   containing a pointer to an int giving the originating node id, and a pointer
   to a `GBytes' holding the message to be sent. The message bytes are relayed
   to every node (via `gzochid_client_socket_write') except for the originating
   node. */

static void
relay_message (gpointer key, gpointer value, gpointer user_data)
{
  gpointer *args = user_data;

  int *to_node_id = key;
  int *from_node_id = args[0];

  if (*to_node_id != *from_node_id)
    {
      size_t len = 0;
      const unsigned char *data = g_bytes_get_data (args[1], &len);
  
      gzochid_client_socket_write (value, data, len);
    }
}

void
gzochi_metad_channelserver_relay_join (GzochiMetadChannelServer *channelserver,
				       int from_node_id, const char *app,
				       guint64 channel_oid, guint64 session_oid)
{
  GBytes *msg_bytes = NULL;
  GByteArray *msg_byte_array = g_byte_array_new ();
  unsigned char opcode = GZOCHID_CHANNEL_PROTOCOL_RELAY_JOIN_TO;

  guint64 encoded_channel_oid = gzochid_util_encode_oid (channel_oid);
  guint64 encoded_session_oid = gzochid_util_encode_oid (session_oid);
  gpointer args[2] = { NULL };

  /* Pad the message bytes with a two-byte length prefix. */
  
  g_byte_array_append (msg_byte_array, (unsigned char *) "\x00\x00", 2);

  g_byte_array_append (msg_byte_array, (unsigned char *) &opcode, 1);
  g_byte_array_append (msg_byte_array, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_channel_oid, 8);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_session_oid, 8);

  /* Fill in the length prefix. */
  
  gzochi_common_io_write_short
    (msg_byte_array->len - 3, msg_byte_array->data, 0); 
  
  msg_bytes = g_byte_array_free_to_bytes (msg_byte_array);

  args[0] = &from_node_id;
  args[1] = msg_bytes;

  gzochid_trace ("Relaying notification for session %s/%" G_GUINT64_FORMAT
		 " joining channel %s/%" G_GUINT64_FORMAT ".", app, session_oid,
		 app, channel_oid);
  
  g_hash_table_foreach (channelserver->connected_servers, relay_message, args);

  g_bytes_unref (msg_bytes);
}

void
gzochi_metad_channelserver_relay_leave (GzochiMetadChannelServer *channelserver,
					int from_node_id, const char *app,
					guint64 channel_oid,
					guint64 session_oid)
{
  GBytes *msg_bytes = NULL;
  GByteArray *msg_byte_array = g_byte_array_new ();
  unsigned char opcode = GZOCHID_CHANNEL_PROTOCOL_RELAY_LEAVE_TO;

  guint64 encoded_channel_oid = gzochid_util_encode_oid (channel_oid);
  guint64 encoded_session_oid = gzochid_util_encode_oid (session_oid);
  gpointer args[2] = { NULL };
  
  /* Pad the message bytes with a two-byte length prefix. */

  g_byte_array_append (msg_byte_array, (unsigned char *) "\x00\x00", 2);

  g_byte_array_append (msg_byte_array, (unsigned char *) &opcode, 1);
  g_byte_array_append (msg_byte_array, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_channel_oid, 8);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_session_oid, 8);

  /* Fill in the length prefix. */

  gzochi_common_io_write_short
    (msg_byte_array->len - 3, msg_byte_array->data, 0); 
  
  msg_bytes = g_byte_array_free_to_bytes (msg_byte_array);
  
  args[0] = &from_node_id;
  args[1] = msg_bytes;

  gzochid_trace ("Relaying notification from session %s/%" G_GUINT64_FORMAT
		 " leaving channel %s/%" G_GUINT64_FORMAT ".", app, session_oid,
		 app, channel_oid);

  g_hash_table_foreach (channelserver->connected_servers, relay_message, args);

  g_bytes_unref (msg_bytes);
}

void
gzochi_metad_channelserver_relay_close (GzochiMetadChannelServer *channelserver,
					int from_node_id, const char *app,
					guint64 channel_oid)
{
  GBytes *msg_bytes = NULL;
  GByteArray *msg_byte_array = g_byte_array_new ();
  unsigned char opcode = GZOCHID_CHANNEL_PROTOCOL_RELAY_CLOSE_TO;

  guint64 encoded_channel_oid = gzochid_util_encode_oid (channel_oid);
  gpointer args[2] = { NULL };
  
  /* Pad the message bytes with a two-byte length prefix. */

  g_byte_array_append (msg_byte_array, (unsigned char *) "\x00\x00", 2);

  g_byte_array_append (msg_byte_array, (unsigned char *) &opcode, 1);
  g_byte_array_append (msg_byte_array, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_channel_oid, 8);

  /* Fill in the length prefix. */
  
  gzochi_common_io_write_short
    (msg_byte_array->len - 3, msg_byte_array->data, 0); 
  
  msg_bytes = g_byte_array_free_to_bytes (msg_byte_array);
  
  args[0] = &from_node_id;
  args[1] = msg_bytes;

  gzochid_trace ("Relaying notification to close channel %s/%" G_GUINT64_FORMAT
		 ".", app, channel_oid);

  g_hash_table_foreach (channelserver->connected_servers, relay_message, args);

  g_bytes_unref (msg_bytes);
}

void
gzochi_metad_channelserver_relay_message
(GzochiMetadChannelServer *channelserver, int from_node_id, const char *app,
 guint64 channel_oid, GBytes *payload)
{
  GBytes *msg_bytes = NULL;
  GByteArray *msg_byte_array = g_byte_array_new ();
  unsigned char opcode = GZOCHID_CHANNEL_PROTOCOL_RELAY_MESSAGE_TO;

  size_t payload_len = 0;
  const unsigned char *payload_data = g_bytes_get_data (payload, &payload_len);
  
  guint64 encoded_channel_oid = gzochid_util_encode_oid (channel_oid);
  gpointer args[2] = { NULL };
  
  /* Pad the message bytes with a two-byte length prefix. */

  g_byte_array_append (msg_byte_array, (unsigned char *) "\x00\x00", 2);

  g_byte_array_append (msg_byte_array, (unsigned char *) &opcode, 1);
  g_byte_array_append (msg_byte_array, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append
    (msg_byte_array, (unsigned char *) &encoded_channel_oid, 8);

  /* Do the same for message payload. */
  
  g_byte_array_append (msg_byte_array, (unsigned char *) "\x00\x00", 2);

  /* Fill in the message paylaod length prefix. */

  gzochi_common_io_write_short
    (payload_len, msg_byte_array->data, msg_byte_array->len - 2);

  g_byte_array_append (msg_byte_array, payload_data, payload_len);
  
  /* Fill in the length prefix. */

  gzochi_common_io_write_short
    (msg_byte_array->len - 3, msg_byte_array->data, 0); 
  
  msg_bytes = g_byte_array_free_to_bytes (msg_byte_array);

  args[0] = &from_node_id;
  args[1] = msg_bytes;

  gzochid_trace ("Relaying message to channel %s/%" G_GUINT64_FORMAT ".", app,
		 channel_oid);

  g_hash_table_foreach (channelserver->connected_servers, relay_message, args);

  g_bytes_unref (msg_bytes);
}

GQuark
gzochi_metad_channelserver_error_quark ()
{
  return g_quark_from_static_string ("gzochi-metad-channelserver-error-quark");
}
