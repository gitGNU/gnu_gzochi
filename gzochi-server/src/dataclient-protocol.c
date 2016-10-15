/* dataclient-protocol.c: Implementation of dataclient protocol.
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
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "data-protocol.h"
#include "dataclient.h"
#include "dataclient-protocol.h"
#include "event.h"
#include "event-app.h"
#include "protocol.h"

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

/*
  Finds the bounds of the `NULL'-terminated string that begins at `bytes', 
  returning a pointer to that string and setting `str_len' appropriately.
  Returns `NULL' if the string is not `NULL'-terminated. 
  
  TODO: This function duplicates a function in `data-protocol.c'. Consider 
  making them available via a shared utility.
*/

static char *
read_str (const unsigned char *bytes, const size_t bytes_len, size_t *str_len)
{
  unsigned char *end = memchr (bytes, 0, bytes_len);

  if (end == NULL)
    return NULL;
  else
    {
      if (str_len != NULL)
        *str_len = end - bytes + 1;
      return (char *) bytes;
    }
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_LOGIN_RESPONSE' opcode. Returns `TRUE' if the 
   message was successfully decoded and the data protocol version advertised by
   the meta server is supported by the client, `FALSE' otherwise. */

static gboolean
dispatch_login_response (GzochidDataClient *client, const unsigned char *data,
			 unsigned short len)
{
  size_t url_len = 0;
  unsigned char version = data[0];
  char *admin_server_base_url = NULL;
  
  if (version != GZOCHID_DATACLIENT_PROTOCOL_VERSION)
    return FALSE;

  admin_server_base_url = read_str (data + 1, len - 1, &url_len);
  
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

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_OIDS_RESPONSE' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_oids_response (GzochidDataClient *client, const unsigned char *data,
			unsigned short len)
{
  GBytes *response_bytes = g_bytes_new_with_free_func (data, len, NULL, NULL);
  gzochid_data_reserve_oids_response *response =
    gzochid_data_protocol_reserve_oids_response_read (response_bytes);
  gboolean ret = TRUE;

  if (response == NULL)
    ret = FALSE;
  else
    {
      /* Invoke the waiting callback. */

      gzochid_dataclient_received_oids (client, response);
      gzochid_data_reserve_oids_response_free (response);
    }
  
  g_bytes_unref (response_bytes);

  return ret;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_VALUE_RESPONSE' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_value_response (GzochidDataClient *client, const unsigned char *data,
			 unsigned short len)
{
  GBytes *response_bytes = g_bytes_new_with_free_func (data, len, NULL, NULL);
  gzochid_data_response *response =
    gzochid_data_protocol_response_read (response_bytes);
  gboolean ret = TRUE;

  if (response == NULL)
    ret = FALSE;
  else
    {
      /* Invoke any waiting callbacks. */

      gzochid_dataclient_received_value (client, response);
      gzochid_data_response_free (response);
    }
  
  g_bytes_unref (response_bytes);

  return ret;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_next_key_response (GzochidDataClient *client,
			    const unsigned char *data, unsigned short len)
{
  GBytes *response_bytes = g_bytes_new_with_free_func (data, len, NULL, NULL);
  gzochid_data_response *response =
    gzochid_data_protocol_response_read (response_bytes);
  gboolean ret = TRUE;

  if (response == NULL)
    ret = FALSE;
  else
    {
      /* Invoke any waiting callbacks. */
      
      gzochid_dataclient_received_next_key (client, response);
      gzochid_data_response_free (response);
    }

  g_bytes_unref (response_bytes);

  return ret;
}

/* Attempt to dispatch a fully-buffered message from the server based on the 
   message opcode. */

static void 
dispatch_message (GzochidDataClient *client, unsigned char *message,
		  unsigned short len)
{
  int opcode = message[0];
  unsigned char *payload = message + 1;
  
  len--;
  
  switch (opcode)
    {
    case GZOCHID_DATA_PROTOCOL_LOGIN_RESPONSE:
      dispatch_login_response (client, payload, len);
      break;
    case GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE:
      dispatch_oids_response (client, payload, len);
      break;      
    case GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE:
      dispatch_value_response (client, payload, len);
      break;
    case GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE:
      dispatch_next_key_response (client, payload, len);
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
  GzochidDataClient *client = user_data;

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

/* The client error handler. Notifies the data client that its connection to the
   meta server is no longer valid. */

static void
client_error (gpointer user_data)
{
  gzochid_dataclient_nullify_connection (user_data);
}

/*
  Client finalization callback. 

  This is currently a no-op, as the data client protocol has no state other than
  the data client, to which it does not even hold a reference.
*/

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol gzochid_dataclient_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };
