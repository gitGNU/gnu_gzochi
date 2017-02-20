/* dataserver-protocol.c: Implementation of dataserver protocol.
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
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "data-protocol.h"
#include "dataserver-protocol.h"
#include "dataserver.h"
#include "meta-protocol.h"
#include "protocol-common.h"
#include "protocol.h"
#include "socket.h"

/* Dataserver client struct. */

struct _gzochi_metad_dataserver_client
{
  guint node_id; /* The client ephemeral node id. */

  GzochiMetadDataServer *dataserver; /* Reference to the data server. */
  gzochid_client_socket *sock; /* The client socket. */
};

gzochi_metad_dataserver_client *
gzochi_metad_dataserver_client_new (GzochiMetadDataServer *dataserver,
				    gzochid_client_socket *sock,
				    unsigned int node_id)
{
  gzochi_metad_dataserver_client *client = malloc
    (sizeof (gzochi_metad_dataserver_client));

  client->dataserver = g_object_ref (dataserver);
  client->sock = sock;
  client->node_id = node_id;
  
  return client;
}

void
gzochi_metad_dataserver_client_free (gzochi_metad_dataserver_client *client)
{
  g_object_unref (client->dataserver);
  
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
   `GZOZCHID_DATA_PROTOCOL_REQUEST_OIDS' opcode. Returns `TRUE' if the message 
   was successfully decoded, `FALSE' otherwise. 

   If the message was successfully decoded, the bytes encoding a 
   `gzochid_data_reserve_oids_response' structure will be written to the client
   socket's send buffer. 
*/

static gboolean 
dispatch_request_oids (gzochi_metad_dataserver_client *client,
		       unsigned char *data, unsigned short len)
{
  size_t str_len = 0;
  const char *app = gzochid_protocol_read_str (data, len, &str_len);
  gzochid_data_reserve_oids_response *response = NULL;
  GByteArray *bytes = NULL;

  if (app == NULL || str_len <= 1)
    {
      g_warning
	("Received malformed 'REQUEST_OIDS' message from node %d.",
	 client->node_id);
      return FALSE;
    }
  
  bytes = g_byte_array_new ();
  response = gzochi_metad_dataserver_reserve_oids
    (client->dataserver, client->node_id, app);

  gzochid_data_protocol_reserve_oids_response_write (response, bytes);

  /* Pad with two `NULL' bytes to leave space for the actual length to be 
     encoded. */

  g_byte_array_prepend
    (bytes, (unsigned char *) &(unsigned char[])
     { 0, 0, GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE }, 3);
  gzochi_common_io_write_short (bytes->len - 3, bytes->data, 0);
  gzochid_client_socket_write (client->sock, bytes->data, bytes->len);
  
  gzochid_data_reserve_oids_response_free (response);
  g_byte_array_unref (bytes);

  return TRUE;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_REQUEST_VALUE' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. 

   If the message was successfully decoded, the bytes encoding a 
   `gzochid_data_object_response' structure will be written to the client
   socket's send buffer. 
*/

static gboolean
dispatch_request_value (gzochi_metad_dataserver_client *client,
			unsigned char *data, unsigned short len)
{
  size_t str_len = 0, offset = 0;
  const char *app = gzochid_protocol_read_str (data, len, &str_len),
    *store = NULL;
  gboolean for_write = FALSE;
  GByteArray *bytes = NULL;
  GBytes *key = NULL;
  GError *err = NULL;

  gzochid_data_response *response = NULL;
  
  if (app == NULL || str_len <= 1)
    {
      g_warning
	("Received malformed 'REQUEST_VALUE' message from node %d.",
	 client->node_id);
      return FALSE;
    }
  
  len -= str_len;
  offset += str_len;

  store = gzochid_protocol_read_str (data + offset, len, &str_len);

  if (store == NULL || str_len <= 1)
    {
      g_warning
	("Received malformed 'REQUEST_VALUE' message from node %d.",
	 client->node_id);
      return FALSE;
    }

  len -= str_len;
  offset += str_len;
  
  if (len <= 0)
    return FALSE;

  for_write = data[offset] == 1;

  len--;
  offset++;
  
  key = gzochid_protocol_read_bytes (data + offset, len);

  if (key == NULL)
    {
      g_warning
	("Received malformed 'REQUEST_VALUE' message from node %d.",
	 client->node_id);
      return FALSE;
    }

  response = gzochi_metad_dataserver_request_value
    (client->dataserver, client->node_id, app, store, key, for_write, &err);

  if (response == NULL)
    {
      assert (err != NULL);

      g_warning
	("Failed to request value for application '%s': %s", app, err->message);

      g_error_free (err);
      g_bytes_unref (key);
      return FALSE;
    }
  
  bytes = g_byte_array_new ();  
  gzochid_data_protocol_response_write (response, bytes);

  /* Pad with two `NULL' bytes to leave space for the actual length to be 
     encoded. */

  g_byte_array_prepend
    (bytes, (unsigned char *) &(unsigned char[])
     { 0, 0, GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE }, 3);
  gzochi_common_io_write_short (bytes->len - 3, bytes->data, 0);
  gzochid_client_socket_write (client->sock, bytes->data, bytes->len);

  gzochid_data_response_free (response);
  g_byte_array_unref (bytes);
  
  g_bytes_unref (key);
  return TRUE;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_request_next_key (gzochi_metad_dataserver_client *client,
			   unsigned char *data, unsigned short len)
{
  size_t str_len = 0, offset = 0;
  const char *app = gzochid_protocol_read_str (data, len, &str_len),
    *store = NULL;
  GByteArray *bytes = NULL;
  GBytes *key = NULL; 
  GError *err = NULL;
  
  gzochid_data_response *response = NULL;

  if (app == NULL || str_len <= 1)
    {
      g_warning
	("Received malformed 'REQUEST_NEXT_KEY' message from node %d.",
	 client->node_id);
      return FALSE;
    }

  len -= str_len;
  offset += str_len;

  store = gzochid_protocol_read_str (data + offset, len, &str_len);

  if (store == NULL || str_len <= 1)
    {
      g_warning
	("Received malformed 'REQUEST_NEXT_KEY' message from node %d.",
	 client->node_id);
      return FALSE;
    }

  len -= str_len;
  offset += str_len;

  key = gzochid_protocol_read_bytes (data + offset, len);

  if (key == NULL)
    {
      g_warning
	("Received malformed 'REQUEST_NEXT_KEY' message from node %d.",
	 client->node_id);
      return FALSE;
    }

  /* The data protocol specifies that an empty key in a "next key" request
     indicates a request to lock the entire keyspace; but the data server uses
     `NULL' to represent this condition. */
  
  response = gzochi_metad_dataserver_request_next_key
    (client->dataserver, client->node_id, app, store,
     g_bytes_get_size (key) == 0 ? NULL : key, &err);

  if (response == NULL)
    {
      assert (err != NULL);

      g_warning
	("Failed to request key range for application '%s': %s", app,
	 err->message);

      g_error_free (err);
      g_bytes_unref (key);
      return FALSE;
    }

  bytes = g_byte_array_new ();
  gzochid_data_protocol_response_write (response, bytes);

  /* Pad with two `NULL' bytes to leave space for the actual length to be 
     encoded. */

  g_byte_array_prepend
    (bytes, (unsigned char *) &(unsigned char[])
     { 0, 0, GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE }, 3);
  gzochi_common_io_write_short (bytes->len - 3, bytes->data, 0);
  gzochid_client_socket_write (client->sock, bytes->data, bytes->len);

  gzochid_data_response_free (response);
  g_byte_array_unref (bytes);
  g_bytes_unref (key);
  
  return TRUE;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_PROCESS_CHANGESET' opcode. Returns `TRUE' if the 
   message was successfully decoded and the changeset ingested, `FALSE' 
   otherwise. */

static gboolean
dispatch_submit_changeset (gzochi_metad_dataserver_client *client,
			   unsigned char *data, unsigned short len)
{
  GError *local_err = NULL;
  GBytes *bytes = g_bytes_new (data, len);
  gzochid_data_changeset *changeset =
    gzochid_data_protocol_changeset_read (bytes);

  if (changeset == NULL)
    {
      g_warning
	("Received malformed 'SUBMIT_CHANGESET' message from node %d.",
	 client->node_id);
      g_bytes_unref (bytes);
      return FALSE;
    }
  
  gzochi_metad_dataserver_process_changeset
    (client->dataserver, client->node_id, changeset, &local_err);

  if (local_err != NULL)
    {
      g_bytes_unref (bytes);
      return FALSE;
    }
  
  gzochid_data_changeset_free (changeset);
  g_bytes_unref (bytes);
  return TRUE;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_RELEASE_KEY' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_release_key (gzochi_metad_dataserver_client *client,
		      unsigned char *data, unsigned short len)
{
  size_t str_len = 0, offset = 0;
  const char *app = gzochid_protocol_read_str (data, len, &str_len),
    *store = NULL;
  GBytes *key = NULL;

  if (app == NULL || str_len <= 1)
    return FALSE;

  len -= str_len;
  offset += str_len;

  store = gzochid_protocol_read_str (data + offset, len, &str_len);

  if (store == NULL || str_len <= 1)
    return FALSE;

  len -= str_len;
  offset += str_len;

  key = gzochid_protocol_read_bytes (data + offset, len);

  if (key == NULL)
    return FALSE;
  
  gzochi_metad_dataserver_release_key
    (client->dataserver, client->node_id, app, store, key);
  g_bytes_unref (key);

  return TRUE;
}

/* Processes the message payload following the 
   `GZOZCHID_DATA_PROTOCOL_RELEASE_KEY_RANGE' opcode. Returns `TRUE' if the 
   message was successfully decoded, `FALSE' otherwise. */

static gboolean
dispatch_release_key_range (gzochi_metad_dataserver_client *client,
			    unsigned char *data, unsigned short len)
{
  size_t str_len = 0, offset = 0;
  const char *app = gzochid_protocol_read_str (data, len, &str_len),
    *store = NULL;
  GBytes *from_key = NULL, *to_key = NULL;

  if (app == NULL || str_len <= 1)
    return FALSE;

  len -= str_len;
  offset += str_len;

  store = gzochid_protocol_read_str (data + offset, len, &str_len);

  if (store == NULL || str_len <= 1)
    return FALSE;

  len -= str_len;
  offset += str_len;

  from_key = gzochid_protocol_read_bytes (data + offset, len);

  if (from_key == NULL)
    return FALSE;

  len -= 2 + g_bytes_get_size (from_key);
  offset += 2 + g_bytes_get_size (from_key);
  
  to_key = gzochid_protocol_read_bytes (data + offset, len);

  if (to_key == NULL)
    {
      g_bytes_unref (from_key);
      return FALSE;
    }
  
  /* The data protocol specifies that an empty "from" or "to" key in a key range
     release indicates the beginning or end, respectively of the keyspace; but
     the data server uses `NULL' to represent these boundaries. */

  gzochi_metad_dataserver_release_range
    (client->dataserver, client->node_id, app, store,
     g_bytes_get_size (from_key) == 0 ? NULL : from_key,
     g_bytes_get_size (to_key) == 0 ? NULL : to_key);

  g_bytes_unref (from_key);
  g_bytes_unref (to_key);
  
  return TRUE;
}

/* Attempt to dispatch a fully-buffered message from the specified client based
   on its opcode. */

static void 
dispatch_message (gzochi_metad_dataserver_client *client,
		  unsigned char *message, unsigned short len)
{
  int opcode = message[0];
  unsigned char *payload = message + 1;
  
  len--;
  
  switch (opcode)
    {
    case GZOCHID_DATA_PROTOCOL_REQUEST_OIDS:
      dispatch_request_oids (client, payload, len); break;
    case GZOCHID_DATA_PROTOCOL_REQUEST_VALUE:
      dispatch_request_value (client, payload, len); break;
    case GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY:
      dispatch_request_next_key (client, payload, len); break;
    case GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET:
      dispatch_submit_changeset (client, payload, len); break;
    case GZOCHID_DATA_PROTOCOL_RELEASE_KEY:
      dispatch_release_key (client, payload, len); break;
    case GZOCHID_DATA_PROTOCOL_RELEASE_KEY_RANGE:
      dispatch_release_key_range (client, payload, len); break;
      
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
  gzochi_metad_dataserver_client *client = user_data;

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

/* The client error handler. Releases all locks held on behalf of the client. */

static void
client_error (gpointer user_data)
{
  gzochi_metad_dataserver_client *client = user_data;
  gzochi_metad_dataserver_release_all (client->dataserver, client->node_id);
}

/* Client finalization callback. */

static void
client_free (gpointer user_data)
{
  gzochi_metad_dataserver_client_free (user_data);
}

gzochid_client_protocol gzochi_metad_dataserver_client_protocol =
  { can_dispatch, client_dispatch, client_error, client_free };
