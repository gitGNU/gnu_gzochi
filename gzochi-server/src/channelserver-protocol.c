/* channelserver-protocol.c: Implementation of channelserver protocol.
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

#include "meta-protocol.h"
#include "protocol-common.h"
#include "channelserver-protocol.h"
#include "channelserver.h"
#include "socket.h"

/* Channelserver client struct. */

struct _gzochi_metad_channelserver_client
{
  guint node_id; /* The client emphemeral node id. */

  /* Reference to the channel server. */

  GzochiMetadChannelServer *channelserver;
};

gzochi_metad_channelserver_client *
gzochi_metad_channelserver_client_new (GzochiMetadChannelServer *channelserver,
				       gzochid_client_socket *sock,
				       unsigned int node_id)
{
  gzochi_metad_channelserver_client *client =
    malloc (sizeof (gzochi_metad_channelserver_client));

  client->channelserver = g_object_ref (channelserver);
  client->node_id = node_id;
  
  return client;  
}

void
gzochi_metad_channelserver_client_free
(gzochi_metad_channelserver_client *client)
{
  g_object_unref (client->channelserver);

  free (client);
}

/*
  Returns `TRUE' if the specified buffer has a complete message that it is 
  ready (with respect to its length) to be dispatched to a handler. The length
  encoding is similar to the one used for the game protocol: A two-byte, 
  big-endian prefix giving the length of the message payload minus the opcode,
  which is the byte directly following the length prefix. 
  
  (So the smallest possible message would be three bytes, in which the first 
  two bytes were `NULL'.) 
*/

static gboolean
can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  if (buffer->len >= 3)
    {
      unsigned short payload_len =
	gzochi_common_io_read_short (buffer->data, 0);
      return buffer->len >= payload_len + 3;
    }
  else return FALSE;
}

/* Processes the message payload following the 
   `GZOZCHID_CHANNEL_PROTOCOL_RELAY_JOIN_FROM' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static void
dispatch_relay_join (gzochi_metad_channelserver_client *client,
		     unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);
  
  if (app == NULL || str_len <= 1 || len - str_len < 16)
    g_warning
      ("Received malformed 'RELAY_JOIN_FROM' message from node %d.",
       client->node_id);
  else
    {
      guint64 channel_oid = gzochi_common_io_read_long (message, str_len);
      guint64 session_oid = gzochi_common_io_read_long (message, str_len + 8);

      gzochi_metad_channelserver_relay_join
	(client->channelserver, client->node_id, app, channel_oid, session_oid);
    }
}

/* Processes the message payload following the 
   `GZOZCHID_CHANNEL_PROTOCOL_RELAY_LEAVE_FROM' opcode. Returns `TRUE' if 
   the message was successfully decoded, `FALSE' otherwise. */

static void
dispatch_relay_leave (gzochi_metad_channelserver_client *client,
		      unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);

  if (app == NULL || str_len <= 1 || len - str_len < 16)
    g_warning
      ("Received malformed 'RELAY_LEAVE_FROM' message from node %d.",
       client->node_id);
  else
    {
      guint64 channel_oid = gzochi_common_io_read_long (message, str_len);
      guint64 session_oid = gzochi_common_io_read_long (message, str_len + 8);

      gzochi_metad_channelserver_relay_leave
	(client->channelserver, client->node_id, app, channel_oid, session_oid);
    }
}

/* Processes the message payload following the 
   `GZOZCHID_CHANNEL_PROTOCOL_RELAY_CLOSE_FROM' opcode. Returns `TRUE' if 
   the message was successfully decoded, `FALSE' otherwise. */

static void
dispatch_relay_close (gzochi_metad_channelserver_client *client,
		      unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);

  if (app == NULL || str_len <= 1 || len - str_len < 8)
    g_warning
      ("Received malformed 'RELAY_CLOSE_FROM' message from node %d.",
       client->node_id);
  else
    {
      guint64 channel_oid = gzochi_common_io_read_long (message, str_len);
      
      gzochi_metad_channelserver_relay_close
	(client->channelserver, client->node_id, app, channel_oid);
    }
}

/* Processes the message payload following the 
   `GZOZCHID_CHANNEL_PROTOCOL_RELAY_MESSAGE_FROM' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static void
dispatch_relay_message (gzochi_metad_channelserver_client *client,
			unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);

  if (app == NULL || str_len <= 1 || len - str_len < 8)
    g_warning
      ("Received malformed 'RELAY_MESSAGE_FROM' message from node %d.",
       client->node_id);
  else
    {
      guint64 channel_oid = gzochi_common_io_read_long (message, str_len);
      GBytes *msg = gzochid_protocol_read_bytes
	(message + str_len + 8, len - str_len - 8);

      if (msg == NULL)
	g_warning
	  ("Received malformed 'RELAY_MESSAGE_FROM' message from node %d.",
	   client->node_id);
      else
	{	      
	  gzochi_metad_channelserver_relay_message
	    (client->channelserver, client->node_id, app, channel_oid, msg);
	  g_bytes_unref (msg);
	}
    }
}

/* Attempt to dispatch a fully-buffered message from the specified client based
   on its opcode. */

static void 
dispatch_message (gzochi_metad_channelserver_client *client,
		  unsigned char *message, unsigned short len)
{
  int opcode = message[0];
  unsigned char *payload = message + 1;
  
  len--;
  
  switch (opcode)
    {
    case GZOCHID_CHANNEL_PROTOCOL_RELAY_JOIN_FROM:
      dispatch_relay_join (client, payload, len); break; 
    case GZOCHID_CHANNEL_PROTOCOL_RELAY_LEAVE_FROM:
      dispatch_relay_leave (client, payload, len); break;
    case GZOCHID_CHANNEL_PROTOCOL_RELAY_CLOSE_FROM:
      dispatch_relay_close (client, payload, len); break;
    case GZOCHID_CHANNEL_PROTOCOL_RELAY_MESSAGE_FROM:
      dispatch_relay_message (client, payload, len); break;
      
    default:
      g_warning ("Unexpected opcode %d received from client", opcode);
    }
  
  return;
}

/* Attempts to dispatch all messages in the specified buffer. Returns the 
   number of successfully dispatched messages. */

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  gzochi_metad_channelserver_client *client = user_data;

  int offset = 0, total = 0;
  int remaining = buffer->len;

  while (remaining >= 3)
    {
      unsigned short len = gzochi_common_io_read_short
        ((unsigned char *) buffer->data, offset);
      
      if (++len > remaining - 2)
        break;
      
      offset += 2;

      dispatch_message (client, (unsigned char *) buffer->data + offset, len);
      
      offset += len;
      remaining -= len + 2;
      total += len + 2;
    }

  return total;
}

/* The client error handler. */

static void
client_error (gpointer user_data)
{
  GError *err = NULL;
  gzochi_metad_channelserver_client *client = user_data;

  gzochi_metad_channelserver_server_disconnected
    (client->channelserver, client->node_id, &err);
}

/* Client finalization callback. */

static void
client_free (gpointer user_data)
{
  gzochi_metad_channelserver_client_free (user_data);
}

gzochid_client_protocol gzochi_metad_channelserver_client_protocol =
  { can_dispatch, client_dispatch, client_error, client_free };
