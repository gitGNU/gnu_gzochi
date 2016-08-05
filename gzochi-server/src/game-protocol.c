/* game-protocol.c: Implementation of game application protocol.
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

#include <assert.h>
#include <glib.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "event.h"
#include "game.h"
#include "game-protocol.h"
#include "gzochid-auth.h"
#include "lifecycle.h"
#include "protocol.h"
#include "socket.h"

struct _gzochid_game_client
{
  gzochid_game_context *game_context;
  
  gzochid_application_context *app_context;
  gzochid_auth_identity *identity;
  
  gboolean disconnected;
  gzochid_client_socket *sock;
};

static gzochid_client_socket *
server_accept (GIOChannel *channel, const char *desc, gpointer data)
{
  gzochid_game_context *game_context = data;
  gzochid_game_client *client = calloc (1, sizeof (gzochid_game_client));
  gzochid_client_socket *sock = gzochid_client_socket_new
    (channel, desc, gzochid_game_client_protocol, client);

  /* This is the initial bootstrap context for the client. Once they 
     authenticate, we'll assign them a real application context. */
  
  client->game_context = game_context;
  client->sock = sock;
  
  return sock;
}

gzochid_server_protocol gzochid_game_server_protocol = { server_accept };

static gboolean
client_can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return buffer->len >= 3
    && buffer->len >= gzochi_common_io_read_short (buffer->data, 0) + 3;
}

static void 
dispatch_login_request (gzochid_game_client *client, char *endpoint,
			unsigned char *cred, short cred_len)
{
  GError *error = NULL;

  if (client->identity != NULL)
    {
      g_warning
	("Client with identity %s attempted to re-authenticate", 
	 gzochid_auth_identity_name (client->identity));
      return;
    }

  client->app_context = gzochid_game_context_lookup_application
    (client->game_context, endpoint);
  if (client->app_context == NULL)
    {
      g_warning
	("Client at %s attempted to authenticate to unknown endpoint %s", 
	 gzochid_client_socket_get_connection_description (client->sock),
	 endpoint);

      /* TODO: Disconnect! */
      
      return;
    }

  assert (client->app_context->authenticator != NULL);
  client->identity = client->app_context->authenticator 
    (cred, cred_len, client->app_context->auth_data, &error);
 
  if (client->identity == NULL)
    {
      if (error != NULL)
	g_critical 
	  ("Error from authenticator for endpoint '%s': %s", endpoint,
	   error->message);
      else g_warning 
	     ("Client at %s failed to authenticate to endpoint %s", 
	      gzochid_client_socket_get_connection_description (client->sock),
	      endpoint);

      g_clear_error (&error);
    }
  else 
    {
      g_info
	("Client at %s authenticated to endpoint %s as %s",
	 gzochid_client_socket_get_connection_description (client->sock),
	 endpoint, gzochid_auth_identity_name (client->identity));
      gzochid_application_client_logged_in (client->app_context, client);
    }
}

static void 
dispatch_logout_request (gzochid_game_client *client)
{
  if (client->identity == NULL)
    g_warning
      ("Received logout request from unauthenticated client at %s",
       gzochid_client_socket_get_connection_description (client->sock));
  else gzochid_application_client_disconnected (client->app_context, client);

  client->disconnected = TRUE;

  /* This will trigger the client's read source to be destroyed and ultimately
     the protocol's `free' function to be called. */
  
  gzochid_client_socket_free (client->sock);

}

static void 
dispatch_session_message (gzochid_game_client *client, unsigned char *msg,
			  short len)
{
  if (client->identity == NULL)
      g_warning 
	("Received session message from unauthenticated client at %s",
	 gzochid_client_socket_get_connection_description (client->sock));
  else
    {
      gzochid_application_event_dispatch
	(client->app_context->event_source,
	 gzochid_application_event_new (MESSAGE_RECEIVED));
      gzochid_application_session_received_message 
	(client->app_context, client, msg, len);
    }
}

static void 
dispatch_message (gzochid_game_client *client, unsigned char *message,
		  unsigned short len)
{
  int opcode = message[0];

  unsigned char *payload = message + 1;
  char *pfx = NULL;
  unsigned char *sfx = NULL;
  short pfx_len = 0, sfx_len = 0;

  len--;
  
  switch (opcode)
    {
    case GZOCHI_COMMON_PROTOCOL_LOGIN_REQUEST:

      pfx = strndup ((char *) payload, len);
      pfx_len = strlen (pfx) + 1;
      sfx = payload + pfx_len;
      sfx_len = len - pfx_len;

      dispatch_login_request (client, pfx, sfx, sfx_len);

      break;
    case GZOCHI_COMMON_PROTOCOL_LOGOUT_REQUEST:
      dispatch_logout_request (client); break;
    case GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE:
      dispatch_session_message (client, (unsigned char *) payload, len); break;

    default:
      g_warning ("Unexpected opcode %d received from client", opcode);
    }

  if (pfx != NULL)
    free (pfx);

  return;
}

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  gzochid_game_client *client = user_data;
  
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

static void 
client_free (gpointer data)
{
  gzochid_game_client *client = data;
  
  free (client);
}

static void
client_error (gpointer data)
{
  gzochid_game_client *client = data;

  if (!client->disconnected)
    {
      if (client->identity != NULL)
	gzochid_application_client_disconnected (client->app_context, client);
      client->disconnected = TRUE;

      /* This will trigger the client's read source to be destroyed and 
	 ultimately the protocol's `free' function to be called. */
  
      gzochid_client_socket_free (client->sock);
    }
}

gzochid_client_protocol gzochid_game_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };

gzochid_auth_identity *
gzochid_game_client_get_identity (gzochid_game_client *client)
{
  return client->identity;
}

void 
gzochid_game_client_disconnect (gzochid_game_client *client)
{
  unsigned char buf[3] = 
    { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_login_success (gzochid_game_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_SUCCESS };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_login_failure (gzochid_game_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_FAILURE };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_send
(gzochid_game_client *client, unsigned char *msg, unsigned short len)
{
  unsigned char *buf = malloc (sizeof (unsigned char) * (len + 3));

  gzochi_common_io_write_short (len, buf, 0);
  buf[2] = GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE;
  memcpy (buf + 3, msg, len);

  gzochid_client_socket_write (client->sock, buf, len + 3);
  free (buf);
}

gboolean
_gzochid_game_client_disconnected (gzochid_game_client *client)
{
  return client->disconnected;
}
