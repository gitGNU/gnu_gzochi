/* sessionclient-protocol.c: Implementation of sessionclient protocol.
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
#include <gzochi-common.h>
#include <stddef.h>
#include <string.h>

#include "meta-protocol.h"
#include "protocol-common.h"
#include "protocol.h"
#include "sessionclient-protocol.h"
#include "sessionclient.h"

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
   `GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_TO' opcode. */

static void
dispatch_relay_disconnect_to (GzochidSessionClient *client,
			      unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);

  if (app == NULL || str_len <= 1 || len - str_len < 8)
    g_warning ("Received malformed 'RELAY_DISCONNECT_TO' message.");
  else
    {
      GError *err = NULL;
      guint64 session_id = gzochi_common_io_read_long (message, str_len);

      gzochid_sessionclient_relay_disconnect_to
	(client, app, session_id, &err);
	      
      if (err != NULL)
	{
	  g_warning ("Failed to disconnect session: %s", err->message);
	  g_error_free (err);
	}
    }
}

/* Processes the message payload following the 
   `GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_TO' opcode. */

static void
dispatch_relay_message_to (GzochidSessionClient *client,
			   unsigned char *message, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (message, len, &str_len);

  if (app == NULL || str_len <= 1 || len - str_len < 8)
    g_warning ("Received malformed 'RELAY_MESSAGE_TO' message.");
  else
    {
      guint64 session_id = gzochi_common_io_read_long (message, str_len);
      GBytes *msg = gzochid_protocol_read_bytes
	(message + str_len + 8, len - str_len - 8);

      if (msg == NULL)
	g_warning ("Received malformed 'RELAY_MESSAGE_TO' message.");
      else
	{
	  GError *err = NULL;
	  gzochid_sessionclient_relay_message_to
	    (client, app, session_id, msg, &err);
	      
	  if (err != NULL)
	    {
	      g_warning
		("Failed to deliver message to session: %s", err->message);
	      g_error_free (err);
	    }

	  g_bytes_unref (msg);
	}
    }
}

/* Attempt to dispatch a fully-buffered message from the server based on the 
   message opcode. */

static void 
dispatch_message (GzochidSessionClient *client, unsigned char *message,
		  unsigned short len)
{
  int opcode = message[0];
  unsigned char *payload = message + 1;
  
  len--;
  
  switch (opcode)
    {
    case GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_TO:
      dispatch_relay_disconnect_to (client, payload, len);
      break;      
    case GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_TO:
      dispatch_relay_message_to (client, payload, len);
      break;      
      
    default:
      g_warning ("Unexpected opcode %d received from server", opcode);
    }
  
  return;
}

/* Attempts to dispatch all messages in the specified buffer. Returns the 
   number of bytes consumed from the buffer. */

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  GzochidSessionClient *client = user_data;

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

/* The client error handler. This is currently a no-op. */

static void
client_error (gpointer user_data)
{
}

/*
  Client finalization callback. 

  This is currently a no-op, as the session client protocol has no state other 
  than the session client, to which it does not even hold a reference.
*/

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol gzochid_sessionclient_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };
