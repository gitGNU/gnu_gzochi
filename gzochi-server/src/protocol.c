/* protocol.c: Application communication protocol routines for gzochid
 * Copyright (C) 2011 Julian Graham
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
#include <libserveez.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include "app.h"
#include "context.h"
#include "game.h"
#include "log.h"
#include "protocol.h"

static void dispatch_login_request 
(gzochid_protocol_client *client, char *endpoint, unsigned char *cred, 
 short cred_len)
{
  gzochid_context *context = (gzochid_context *) client->sock->data;
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
	 client->connection_description, endpoint);
      return;
    }

  assert (client->context->authenticator != NULL);
  client->identity = client->context->authenticator 
    (client->context, cred, cred_len);
 
  if (client->identity == NULL)
    gzochid_warning ("Client at %s failed to authenticate to endpoint %s", 
		     client->connection_description, endpoint);
  else 
    {
      gzochid_info
	("Client at %s authenticated to endpoint %s as %s",
	 client->connection_description, endpoint, client->identity->name);
      gzochid_application_client_logged_in (client->context, client);
    }
}

static void dispatch_logout_request (gzochid_protocol_client *client)
{
  if (client->identity == NULL)
    gzochid_warning
      ("Received logout request from unauthenticated client at %s",
       client->connection_description);
  else gzochid_application_client_disconnected (client->context, client);

  client->disconnected = TRUE;
}

static void dispatch_session_message 
(gzochid_protocol_client *client, unsigned char *msg, short len)
{
  if (client->identity == NULL)
      gzochid_warning 
	("Received session message from unauthenticated client at %s",
	 client->connection_description);
  else gzochid_application_session_received_message 
	 (client->context, client, msg, len);
}

static void dispatch_channel_message
(gzochid_protocol_client *client, char *channel, unsigned char *msg, short len)
{
  if (client->identity == NULL)
    gzochid_warning 
      ("Received channel message from unauthenticated client at %s",
       client->connection_description);
  gzochid_application_channel_message_received 
    (client->context, client, channel, msg, len);
}

void gzochid_protocol_client_disconnected (gzochid_protocol_client *client)
{
  if (client->identity != NULL && !client->disconnected)
    {
      gzochid_application_client_disconnected (client->context, client);
      client->disconnected = TRUE;
    }
}

gzochid_protocol_client *gzochid_protocol_client_accept (svz_socket_t *sock)
{
  gzochid_protocol_client *client = 
    calloc (1, sizeof (gzochid_protocol_client));

  mpz_init (client->oid);
  mpz_set_si (client->oid, -1);

  client->sock = sock;
  client->connection_description = g_strdup_printf 
    ("%s:%d", "addr", sock->remote_port);
  
  return client;
}

void gzochid_protocol_client_free (gzochid_protocol_client *client)
{
  mpz_clear (client->oid);
  g_free (client->connection_description);
  free (client);
}

void gzochid_protocol_client_dispatch 
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
    case GZOCHI_COMMON_PROTOCOL_CHANNEL_MESSAGE:

      pfx = strndup ((char *) payload, len);
      pfx_len = strlen (pfx) + 1;
      sfx = payload + pfx_len;
      sfx_len = len - pfx_len;

      dispatch_channel_message (client, pfx, sfx, sfx_len);

      break;
    default:
      gzochid_warning ("Unexpected opcode %d received from client", opcode);
    }

  if (pfx == NULL)
    free (pfx);

  return;
}

void gzochid_protocol_client_disconnect (gzochid_protocol_client *client)
{
  char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED };
  svz_sock_write (client->sock, buf, 3);
}

void gzochid_protocol_client_login_success (gzochid_protocol_client *client)
{
  char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_SUCCESS };
  svz_sock_write (client->sock, buf, 3);
}

void gzochid_protocol_client_login_failure (gzochid_protocol_client *client)
{
  char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_FAILURE };
  svz_sock_write (client->sock, buf, 3);
}

void gzochid_protocol_client_send 
(gzochid_protocol_client *client, unsigned char *msg, short len)
{
  unsigned char len_str[3];

  gzochi_common_io_write_short (len, len_str, 0);
  len_str[2] = GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE;

  svz_sock_write (client->sock, (char *) len_str, 3);
  svz_sock_write (client->sock, (char *) msg, len);
}

void gzochid_protocol_client_joined_channel 
(gzochid_protocol_client *client, char *name, unsigned char *id, short id_len)
{
  short name_len = strlen (name);

  unsigned char name_len_str[2];
  unsigned char id_len_str[2];
  unsigned char len_str[3];
  
  gzochi_common_io_write_short (name_len + id_len + 4, len_str, 0);
  len_str[2] = GZOCHI_COMMON_PROTOCOL_CHANNEL_JOIN;

  gzochi_common_io_write_short (name_len, name_len_str, 0);
  gzochi_common_io_write_short (id_len, id_len_str, 0);

  svz_sock_write (client->sock, (char *) len_str, 3);
  svz_sock_write (client->sock, (char *) name_len_str, 2);
  svz_sock_write (client->sock, name, name_len);
  svz_sock_write (client->sock, (char *) id_len_str, 2);
  svz_sock_write (client->sock, (char *) id, id_len);
}

void gzochid_protocol_client_left_channel 
(gzochid_protocol_client *client, unsigned char *id, short id_len)
{
  unsigned char id_len_str[2];
  unsigned char len_str[3];

  gzochi_common_io_write_short (id_len + 2, len_str, 0);
  gzochi_common_io_write_short (id_len, id_len_str, 0);

  len_str[2] = GZOCHI_COMMON_PROTOCOL_CHANNEL_DISCONNECTED;

  svz_sock_write (client->sock, (char *) len_str, 3);
  svz_sock_write (client->sock, (char *) id_len_str, 2);
  svz_sock_write (client->sock, (char *) id, id_len);
}

void gzochid_protocol_client_channel_send 
(gzochid_protocol_client *client, unsigned char *id, short id_len, 
 unsigned char *msg, short msg_len)
{
  unsigned char id_len_str[2];
  unsigned char msg_len_str[2];
  unsigned char len_str[3];
  
  gzochi_common_io_write_short (id_len + msg_len + 4, len_str, 0);
  gzochi_common_io_write_short (id_len, id_len_str, 0);
  gzochi_common_io_write_short (msg_len, msg_len_str, 0);

  len_str[2] = GZOCHI_COMMON_PROTOCOL_CHANNEL_MESSAGE;

  svz_sock_write (client->sock, (char *) len_str, 3);
  svz_sock_write (client->sock, (char *) id_len_str, 2);
  svz_sock_write (client->sock, (char *) id, id_len);
  svz_sock_write (client->sock, (char *) msg_len_str, 2);
  svz_sock_write (client->sock, (char *) msg, msg_len);
}
