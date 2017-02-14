/* metaserver-protocol.c: Implementation of metaserver protocol.
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

#include "channelserver-protocol.h"
#include "channelserver.h"
#include "dataserver-protocol.h"
#include "dataserver.h"
#include "event-meta.h"
#include "event.h"
#include "meta-protocol.h"
#include "metaserver-protocol.h"
#include "protocol-common.h"
#include "protocol.h"
#include "resolver.h"
#include "sessionserver-protocol.h"
#include "sessionserver.h"

/* This protocol implementation is a "router" for segments of the meta protocol
   that correspond to more specific server components; overall client lifecycle
   management and "global" opcodes (like `LOGIN') are handled inline, whereas
   processing for messages in the `GZOCHID_DATA_PROTOCOL_*' family, for example,
   are delegated to the handlers in the 
   `gzochi_metad_dataserver_client_protocol' callback struct. */

/* The version of the data protocol understood by the server. */

#define METASERVER_PROTOCOL_VERSION 0x02

static volatile guint next_node_id = 0;

/* Metaserver client struct. */

struct _gzochi_metad_metaserver_client
{
  guint node_id; /* The client ephemeral node id. */
  gzochid_client_socket *sock; /* The client socket. */

  /* Reference to the root context, as on opaque `GObject', since all 
     interactions with it happen through property accessors. */

  GObject *root_context; 

  /* Delegate client for channelserver operations. */

  gzochi_metad_channelserver_client *channelserver_client;
  
  /* Delegate client for dataserver operations. */
  
  gzochi_metad_dataserver_client *dataserver_client;

  /* Delegate client for sessionserver operations. */
  
  gzochi_metad_sessionserver_client *sessionserver_client;
};

static gzochid_client_socket *
server_accept (GIOChannel *channel, const char *desc, gpointer data)
{
  GObject *root_context = data;
  gzochi_metad_metaserver_client *client = calloc
    (1, sizeof (gzochi_metad_metaserver_client));
  gzochid_client_socket *sock = gzochid_client_socket_new
    (channel, desc, gzochi_metad_metaserver_client_protocol, client);  
  GzochiMetadChannelServer *channelserver = NULL;
  GzochiMetadDataServer *dataserver = NULL;
  GzochiMetadSessionServer *sessionserver = NULL;

  g_object_get
    (root_context,
     "channel-server", &channelserver,
     "data-server", &dataserver,
     "session-server", &sessionserver,
     NULL);

  /* Generate the next node id. */
  
  client->node_id = g_atomic_int_add (&next_node_id, 1);
  client->sock = sock;
  client->root_context = g_object_ref (root_context);
  
  client->channelserver_client = gzochi_metad_channelserver_client_new
    (channelserver, sock, client->node_id);
  client->dataserver_client = gzochi_metad_dataserver_client_new
    (dataserver, sock, client->node_id);
  client->sessionserver_client = gzochi_metad_sessionserver_client_new
    (sessionserver, sock, client->node_id);

  g_message
    ("Received connection from %s; assigning id %d", desc, client->node_id);

  g_object_unref (channelserver);
  g_object_unref (dataserver);
  g_object_unref (sessionserver);
  
  return sock;
}

gzochid_server_protocol gzochi_metad_metaserver_server_protocol =
  { server_accept };

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
  return buffer->len >= 3
    && buffer->len >= gzochi_common_io_read_short (buffer->data, 0) + 3;
}

/* Processes the message payload following the `GZOZCHID_META_PROTOCOL_LOGIN'
   opcode. Returns `TRUE' if the message was successfully decoded, `FALSE'
   otherwise. */

static gboolean
dispatch_login (gzochi_metad_metaserver_client *client, unsigned char *data,
		unsigned short len)
{
  size_t str_len = 0;
  unsigned char version = data[0];
  const char *client_admin_server_base_url = NULL;

  if (version != METASERVER_PROTOCOL_VERSION)
    return FALSE;

  client_admin_server_base_url =
    gzochid_protocol_read_str (data + 1, len - 1, &str_len);

  /* The admin server base URL can be empty, but not absent. */
  
  if (client_admin_server_base_url == NULL)
    {
      g_warning
	("Received malformed 'LOGIN' message from node %d.", client->node_id);
      return FALSE;
    }
  else
    {
      char *admin_server_base_url = NULL;
      GByteArray *login_response_message = g_byte_array_new ();

      GzochiMetadClientEvent *event = NULL;
      GzochiMetadChannelServer *channelserver = NULL;
      GzochiMetadSessionServer *sessionserver = NULL;
      gzochid_event_source *event_source = NULL;
      const char *conn_desc = gzochid_client_socket_get_connection_description
	(client->sock);
      
      g_object_get
	(client->root_context,
	 "admin-server-base-url", &admin_server_base_url,
	 "channel-server", &channelserver,
	 "event-source", &event_source,
	 "session-server", &sessionserver,
	 NULL);

      /* This is the first moment at which it's reasonable to let the channel 
	 server and the session server know that there's a new application 
	 server node connected. TODO: Add some actual error handling. */

      gzochi_metad_channelserver_server_connected
	(channelserver, client->node_id, client->sock, NULL);
      gzochi_metad_sessionserver_server_connected
	(sessionserver, client->node_id, client->sock, NULL);
      g_object_unref (sessionserver);      

      if (str_len > 1)
	gzochid_event_dispatch
	  (event_source, g_object_new
	   (GZOCHI_METAD_TYPE_CLIENT_EVENT,
	    "type", CLIENT_CONNECTED,
	    "node-id", client->node_id,
	    "connection-description", conn_desc,	 
	    "admin-server-base-url", client_admin_server_base_url,
	    NULL));
      
      else gzochid_event_dispatch
	     (event_source, g_object_new
	      (GZOCHI_METAD_TYPE_CLIENT_EVENT,
	       "type", CLIENT_CONNECTED,
	       "node-id", client->node_id,
	       "connection-description", conn_desc,	 
	       NULL));

      g_source_unref ((GSource *) event_source);
       
      /* Pad with two `NULL' bytes to leave space for the actual length to be 
	 encoded. */
      
      g_byte_array_append
	(login_response_message,
	 (unsigned char[]) { 0x00, 0x00, GZOCHID_META_PROTOCOL_LOGIN_RESPONSE,
	    METASERVER_PROTOCOL_VERSION }, 4);

      g_byte_array_append
	(login_response_message, (unsigned char *) admin_server_base_url,
	 strlen (admin_server_base_url) + 1);

      gzochi_common_io_write_short
	(login_response_message->len - 3, login_response_message->data, 0);

      gzochid_client_socket_write
	(client->sock, login_response_message->data,
	 login_response_message->len);

      g_byte_array_unref (login_response_message);
      g_free (admin_server_base_url);
      
      return TRUE;
    }  
}

/* Attempts to dispatch all messages in the specified buffer. Returns the 
   number of successfully dispatched messages. */

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  gzochi_metad_metaserver_client *client = user_data;

  int offset = 0, total = 0;
  int remaining = buffer->len;

  while (remaining >= 3)
    {
      char opcode = 0;
      unsigned short len = gzochi_common_io_read_short
        ((unsigned char *) buffer->data, offset);
      
      if (++len > remaining - 2)
        break;

      offset += 2;
      opcode = buffer->data[offset];
      
      switch (opcode)
	{
	  /* Handling for "top-level" opcodes. */
	  
	case GZOCHID_META_PROTOCOL_LOGIN:
	  dispatch_login
	    (client, (unsigned char *) buffer->data + offset + 1, len - 1);
	  break;

	  /* Opcodes understood by the channelserver protocol. */

	case GZOCHID_CHANNEL_PROTOCOL_RELAY_JOIN_FROM:
	case GZOCHID_CHANNEL_PROTOCOL_RELAY_LEAVE_FROM:
	case GZOCHID_CHANNEL_PROTOCOL_RELAY_CLOSE_FROM:
	case GZOCHID_CHANNEL_PROTOCOL_RELAY_MESSAGE_FROM:
	  {
	    GByteArray *delegate_buffer = g_byte_array_sized_new (len);
	    
	    g_byte_array_append
	      (delegate_buffer, buffer->data + offset - 2, len + 2);
	    gzochi_metad_channelserver_client_protocol.dispatch
	      (delegate_buffer, client->channelserver_client);
	    g_byte_array_unref (delegate_buffer);
	    
	    break;
	  }
	  
	  /* Opcodes understood by the dataserver protocol. */
	  
	case GZOCHID_DATA_PROTOCOL_REQUEST_OIDS:
	case GZOCHID_DATA_PROTOCOL_REQUEST_VALUE:
	case GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY:
	case GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET:
	case GZOCHID_DATA_PROTOCOL_RELEASE_KEY:
	case GZOCHID_DATA_PROTOCOL_RELEASE_KEY_RANGE:
	  {
	    GByteArray *delegate_buffer = g_byte_array_sized_new (len);
	    
	    g_byte_array_append
	      (delegate_buffer, buffer->data + offset - 2, len + 2);
	    gzochi_metad_dataserver_client_protocol.dispatch
	      (delegate_buffer, client->dataserver_client);
	    g_byte_array_unref (delegate_buffer);
	    
	    break;
	  }

	  /* Opcodes understood by the sessionserver protocol. */
	  
	case GZOCHID_SESSION_PROTOCOL_SESSION_CONNECTED:
	case GZOCHID_SESSION_PROTOCOL_SESSION_DISCONNECTED:
	case GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_FROM:
	case GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_FROM:
	  {
	    GByteArray *delegate_buffer = g_byte_array_sized_new (len);
	    
	    g_byte_array_append
	      (delegate_buffer, buffer->data + offset - 2, len + 2);
	    gzochi_metad_sessionserver_client_protocol.dispatch
	      (delegate_buffer, client->sessionserver_client);
	    g_byte_array_unref (delegate_buffer);
	    
	    break;
	  }

	default:
	  g_warning ("Unexpected opcode %d received from client", opcode);
	}
      
      offset += len;
      remaining -= len + 2;
      total += len + 2;
    }

  return total;
}

/* The client error handler. Fires a `CLIENT_DISCONNECTED' event and then 
   delegates to the `error' handlers of all delegate protocol clients. */

static void
client_error (gpointer user_data)
{
  gzochi_metad_metaserver_client *client = user_data;
  gzochid_event_source *event_source = NULL;

  g_object_get (client->root_context, "event-source", &event_source, NULL);

  gzochid_event_dispatch
    (event_source, g_object_new
     (GZOCHI_METAD_TYPE_CLIENT_EVENT,
      "type", CLIENT_DISCONNECTED, "node-id", client->node_id, NULL));

  g_source_unref ((GSource *) event_source);
  
  gzochi_metad_channelserver_client_protocol.error
    (client->channelserver_client);
  gzochi_metad_dataserver_client_protocol.error (client->dataserver_client);
  gzochi_metad_sessionserver_client_protocol.error
    (client->sessionserver_client);
}

/* Client finalization callback. Invokes the associated protocol `free' callback
   for all delegate clients and then frees the top-level metaserver protocol
   client. */

static void
client_free (gpointer user_data)
{
  gzochi_metad_metaserver_client *client = user_data;
  
  gzochi_metad_channelserver_client_protocol.free
    (client->channelserver_client);
  gzochi_metad_dataserver_client_protocol.free (client->dataserver_client);
  gzochi_metad_sessionserver_client_protocol.free
    (client->sessionserver_client);

  free (client);
}

gzochid_client_protocol gzochi_metad_metaserver_client_protocol =
  { can_dispatch, client_dispatch, client_error, client_free };
