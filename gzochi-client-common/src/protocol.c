/* protocol.c: Client-side protocol I/O routines for libgzochi
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

#include <gzochi-common.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "protocol.h"
#include "session.h"

#define CHUNK_SIZE 512

#if defined (__APPLE__) && defined (__MACH__)
#define SEND_FLAGS 0x00
#else
#define SEND_FLAGS MSG_NOSIGNAL
#endif

static int send_fully (int sock, unsigned char *data, int len)
{
  int total_sent = 0;
  while (total_sent < len)
    {      
      int bytes_sent = send
	(sock, (char *) data + total_sent, len - total_sent, SEND_FLAGS);
      
      if (bytes_sent < 0)
	return -1;

      total_sent += bytes_sent;
    }

  return total_sent;
}

static int send_protocol_message 
(int sock, unsigned char opcode, unsigned char *message, short len)
{
  unsigned char len_bytes[2] = { 0, 0 };
  gzochi_common_io_write_short (len, len_bytes, 0);
  if (send_fully (sock, len_bytes, 2) < 0)
    return -1;
  if (send_fully (sock, &opcode, 1) < 0)
    return -1;
  if (len > 0)
    return send_fully (sock, message, len);
  else return 0;
}

int gzochi_client_protocol_send_login_request 
(gzochi_client_common_session *session, char *endpoint, 
 unsigned char *credentials, int len)
{
  int ret = 0;
  int endpoint_len = strlen (endpoint) + 1;
  int buffer_len = endpoint_len + len;
  unsigned char *buffer = malloc (sizeof (unsigned char) * buffer_len);

  memcpy (buffer, endpoint, endpoint_len);
  memcpy (buffer + endpoint_len, credentials, len);

  ret = send_protocol_message 
    (session->socket, GZOCHI_COMMON_PROTOCOL_LOGIN_REQUEST, buffer, buffer_len);
  free (buffer);
  return ret;
}

int gzochi_client_protocol_send_disconnect 
(gzochi_client_common_session *session)
{
  return send_protocol_message 
    (session->socket, GZOCHI_COMMON_PROTOCOL_LOGOUT_REQUEST, NULL, 0);
}

int gzochi_client_protocol_send_session_message 
(gzochi_client_common_session *session, unsigned char *msg, short len)
{
  return send_protocol_message
    (session->socket, GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE, 
     (unsigned char *) msg, len);
}

static void dispatch_session_message 
(gzochi_client_common_session *session, unsigned char *message, short len)
{
  if (session->received_message_callback != NULL)
    session->received_message_callback 
      (session, message, len, session->received_message_user_data);
}

static void dispatch_session_disconnected 
(gzochi_client_common_session *session)
{
  session->connected = FALSE;
  if (session->disconnected_callback != NULL)
    session->disconnected_callback (session, session->disconnected_user_data);
}

static void dispatch 
(gzochi_client_common_session *session, int opcode, unsigned char *payload, 
 short len)
{
  switch (opcode)
    {
    case GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE:
      dispatch_session_message (session, payload, len); break;
    case GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED:
      dispatch_session_disconnected (session); break;
 
    default:
      break;
    }
}

static int attempt_dispatch (gzochi_client_common_session *session, int limit)
{
  int remaining = limit;
  int offset = 0;
  int dispatched = 0;

  while (limit == 0 || remaining > 0)
    {
      short message_len = 0;
      char opcode = 0x00;

      if (session->buffer_length - offset < 3)
	break;

      message_len = gzochi_common_io_read_short (session->buffer, offset);
      if (session->buffer_length - offset < message_len + 3) 
	break;

      opcode = session->buffer[offset + 2];
      dispatch (session, opcode, session->buffer + offset + 3, message_len);
      dispatched++;
      
      offset += 3 + message_len;
      
      if (limit != 0)
	remaining--;
    }

  if (offset > 0)
    {
      memmove (session->buffer, session->buffer + offset, 
	       session->buffer_length - offset);
      session->buffer_length -= offset;
    }

  return dispatched;
}

int gzochi_client_protocol_dispatch_all (gzochi_client_common_session *session)
{
  return attempt_dispatch (session, 0);
}

int gzochi_client_protocol_dispatch (gzochi_client_common_session *session)
{
  return attempt_dispatch (session, 1);
}

int gzochi_client_protocol_read (gzochi_client_common_session *session)
{
  char chunk[CHUNK_SIZE];
  int available_buffer = GZOCHI_CLIENT_MAX_BUFFER_SIZE - session->buffer_length;
  int bytes_requested = MIN(CHUNK_SIZE, available_buffer);
  int bytes_read = recv (session->socket, chunk, bytes_requested, 0);

  if (bytes_read < 0)
    return -1;
      
  memcpy (session->buffer + session->buffer_length, chunk, bytes_read);
  session->buffer_length += bytes_read;
  available_buffer -= bytes_read;
  
  return 0;
}

