/* metaclient-protocol.c: Implementation of metaclient protocol.
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
#include <string.h>

#include "dataclient-protocol.h"
#include "dataclient.h"
#include "event-app.h"
#include "event.h"
#include "meta-protocol.h"
#include "metaclient-protocol.h"
#include "metaclient.h"
#include "protocol-common.h"
#include "sessionclient-protocol.h"
#include "sessionclient.h"

/* This protocol implementation is a "router" for segments of the meta protocol
   that correspond to more specific client components; overall connection
   lifecycle management and "global" opcodes (like `LOGIN_RESPONSE') are 
   handled inline, whereas processing for messages in the 
   `GZOCHID_DATA_PROTOCOL_*' family, for example, are delegated to the handlers
   in the `gzochid_dataclient_client_protocol' callback struct. */

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
client_can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return buffer->len >= 3
    && buffer->len >= gzochi_common_io_read_short (buffer->data, 0) + 3;
}

/* Processes the message payload following the 
   `GZOZCHID_META_PROTOCOL_LOGIN_RESPONSE' opcode. Returns `TRUE' if the 
   message was successfully decoded and the meta protocol version advertised by
   the meta server is supported by the client, `FALSE' otherwise. */

static gboolean
dispatch_login_response (GzochidMetaClient *client, const unsigned char *data,
			 unsigned short len)
{
  size_t url_len = 0;
  unsigned char version = data[0];
  const char *admin_server_base_url = NULL;
  
  if (version != GZOCHID_METACLIENT_PROTOCOL_VERSION)
    return FALSE;

  admin_server_base_url =
    gzochid_protocol_read_str (data + 1, len - 1, &url_len);
  
  /* The admin server base URL can be empty, but not absent. */
  
  if (admin_server_base_url == NULL)
    {
      g_warning
	("Received malformed 'LOGIN RESPONSE' message from metaserver.");
      return FALSE;
    }
  else
    {
      GzochidMetaServerEvent *event = NULL;

      gzochid_event_source *event_source = NULL;
      char *conn_desc = NULL;

      g_object_get
	(client,
	 "connection-description", &conn_desc,
	 "event-source", &event_source,
	 NULL);
      
      if (url_len > 0)
	event = g_object_new
	  (GZOCHID_TYPE_META_SERVER_EVENT,
	   "type", META_SERVER_CONNECTED,
	   "connection-description", conn_desc,	 
	   "admin-server-base-url", admin_server_base_url,
	   NULL);

      /* When there's no admin console available, the base URL will be empty. */
      
      else event = g_object_new
	     (GZOCHID_TYPE_META_SERVER_EVENT,
	      "type", META_SERVER_CONNECTED,
	      "connection-description", conn_desc,	 
	      NULL);
      
      gzochid_event_dispatch (event_source, GZOCHID_EVENT (event));
      g_object_unref (event);
      
      g_free (conn_desc);
      g_source_unref ((GSource *) event_source);
      
      return TRUE;
    }
}

/* Attempts to dispatch all messages in the specified buffer. Returns the 
   number of bytes consumed from the buffer. */

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  GzochidMetaClient *client = user_data;

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

	case GZOCHID_META_PROTOCOL_LOGIN_RESPONSE:
	  dispatch_login_response
	    (client, (unsigned char *) buffer->data + offset + 1, len - 1);
	  break;
	  
	  /* Opcodes understood by the dataserver protocol. */

	case GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE:
	case GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE:
	case GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE:
	  {
	    GzochidDataClient *dataclient = NULL;
	    GByteArray *delegate_buffer = g_byte_array_sized_new (len);

	    g_object_get (client, "dataclient", &dataclient, NULL);
	    
	    g_byte_array_append
	      (delegate_buffer, buffer->data + offset - 2, len + 2);
	    gzochid_dataclient_client_protocol.dispatch
	      (delegate_buffer, dataclient);

	    g_byte_array_unref (delegate_buffer);
	    g_object_unref (dataclient);
	    
	    break;
	  }	  
	  
	case GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_TO:
	case GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_TO:
	  {
	    GzochidSessionClient *sessionclient = NULL;
	    GByteArray *delegate_buffer = g_byte_array_sized_new (len);

	    g_object_get (client, "sessionclient", &sessionclient, NULL);
	    
	    g_byte_array_append
	      (delegate_buffer, buffer->data + offset - 2, len + 2);
	    gzochid_sessionclient_client_protocol.dispatch
	      (delegate_buffer, sessionclient);

	    g_byte_array_unref (delegate_buffer);
	    g_object_unref (sessionclient);
	    
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

static void
client_error (gpointer user_data)
{
  gzochid_metaclient_nullify_connection (user_data);
}

/*
  Client finalization callback. 

  This is currently a no-op, as the meta client protocol has no state other than
  the meta client, to which it does not even hold a reference.
*/

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol gzochid_metaclient_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };
