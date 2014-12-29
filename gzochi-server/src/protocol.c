/* protocol.c: Application communication protocol routines for gzochid
 * Copyright (C) 2014 Julian Graham
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
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include "app.h"
#include "context.h"
#include "game.h"
#include "log.h"
#include "protocol.h"

struct _gzochid_protocol_client
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  mpz_t oid;
  
  gboolean disconnected;
  gzochid_client_socket *sock;
};

gzochid_auth_identity *
gzochid_protocol_client_get_identity (gzochid_protocol_client *client)
{
  return client->identity;
}

gzochid_protocol_client *
gzochid_protocol_client_accept (gzochid_client_socket *sock)
{
  gzochid_protocol_client *client = 
    calloc (1, sizeof (gzochid_protocol_client));

  mpz_init (client->oid);
  mpz_set_si (client->oid, -1);

  client->sock = sock;
  
  return client;
}

void 
gzochid_protocol_client_disconnected (gzochid_protocol_client *client)
{
  if (client->identity != NULL && !client->disconnected)
    {
      gzochid_application_client_disconnected (client->context, client);
      client->disconnected = TRUE;
    }
}

void 
gzochid_protocol_client_free (gzochid_protocol_client *client)
{
  gzochid_client_socket_free (client->sock);
  mpz_clear (client->oid);
  free (client);
}

static void 
dispatch_login_request 
(gzochid_protocol_client *client, char *endpoint, unsigned char *cred, 
 short cred_len)
{
  GError *error = NULL;
  gzochid_context *context = (gzochid_context *)
    gzochid_socket_get_server_context (client->sock);
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  if (client->identity != NULL)
    {
      gzochid_warning
	("Client with identity %s attempted to re-authenticate", 
	 client->identity->name);
      return;
    }

  client->context = gzochid_game_context_lookup_application 
    (game_context, endpoint);
  if (client->context == NULL)
    {
      gzochid_warning
	("Client at %s attempted to authenticate to unknown endpoint %s", 
	 gzochid_socket_get_connection_description (client->sock), endpoint);
      return;
    }

  assert (client->context->authenticator != NULL);
  client->identity = client->context->authenticator 
    (cred, cred_len, client->context->auth_data, &error);
 
  if (client->identity == NULL)
    {
      if (error != NULL)
	gzochid_err 
	  ("Error from authenticator for endpoint '%s': %s", error->message);
      else gzochid_warning 
	     ("Client at %s failed to authenticate to endpoint %s", 
	      gzochid_socket_get_connection_description (client->sock),
	      endpoint);

      g_clear_error (&error);
    }
  else 
    {
      gzochid_info
	("Client at %s authenticated to endpoint %s as %s",
	 gzochid_socket_get_connection_description (client->sock), endpoint, 
	 client->identity->name);
      gzochid_application_client_logged_in (client->context, client);
    }
}

static void 
dispatch_logout_request (gzochid_protocol_client *client)
{
  if (client->identity == NULL)
    gzochid_warning
      ("Received logout request from unauthenticated client at %s",
       gzochid_socket_get_connection_description (client->sock));
  else gzochid_application_client_disconnected (client->context, client);

  client->disconnected = TRUE;
}

static void 
dispatch_session_message 
(gzochid_protocol_client *client, unsigned char *msg, short len)
{
  if (client->identity == NULL)
      gzochid_warning 
	("Received session message from unauthenticated client at %s",
	 gzochid_socket_get_connection_description (client->sock));
  else gzochid_application_session_received_message 
	 (client->context, client, msg, len);
}

void 
gzochid_protocol_client_dispatch 
(gzochid_protocol_client *client, unsigned char *message, short len)
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
      gzochid_warning ("Unexpected opcode %d received from client", opcode);
    }

  if (pfx != NULL)
    free (pfx);

  return;
}

void 
gzochid_protocol_client_disconnect (gzochid_protocol_client *client)
{
  unsigned char buf[3] = 
    { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_protocol_client_login_success (gzochid_protocol_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_SUCCESS };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_protocol_client_login_failure (gzochid_protocol_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_FAILURE };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_protocol_client_send 
(gzochid_protocol_client *client, unsigned char *msg, short len)
{
  unsigned char *buf = malloc (sizeof (unsigned char) * (len + 3));

  gzochi_common_io_write_short (len, buf, 0);
  buf[2] = GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE;
  memcpy (buf + 3, msg, len);

  gzochid_client_socket_write (client->sock, buf, len + 3);
  free (buf);
}
