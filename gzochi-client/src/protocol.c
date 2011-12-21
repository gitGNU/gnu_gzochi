/* protocol.c: Client-side protocol I/O routines for libgzochi
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

#include <gzochi-common.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "channel.h"
#include "protocol.h"
#include "session.h"
#include "sys/socket.h"

#define CHUNK_SIZE 512

static int send_fully (int sock, unsigned char *data, int len)
{
  int total_sent = 0;
  while (total_sent < len)
    {
      int bytes_sent = send 
	(sock, (char *) data + total_sent, len - total_sent, 0);
      
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

int gzochi_protocol_send_login_request 
(gzochi_client_session *session, char *endpoint, unsigned char *credentials, 
 int len)
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

int gzochi_protocol_send_disconnect (gzochi_client_session *session)
{
  return send_protocol_message 
    (session->socket, GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECT_REQUEST, NULL, 
     0);
}

int gzochi_protocol_send_session_message 
(gzochi_client_session *session, unsigned char *msg, short len)
{
  return send_protocol_message
    (session->socket, GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE, 
     (unsigned char *) msg, len);
}

int gzochi_protocol_send_channel_message
(gzochi_client_session *session, char *channel, unsigned char *msg, short len)
{
  int ret = 0;
  int channel_len = strlen (channel) + 1;
  int buffer_len = channel_len + len;
  unsigned char *buffer = malloc (sizeof (char) * buffer_len);

  memcpy (buffer, channel, channel_len);
  memcpy (buffer + channel_len, msg, len);

  ret = send_protocol_message 
    (session->socket, GZOCHI_COMMON_PROTOCOL_CHANNEL_MESSAGE, buffer, 
     buffer_len);
  
  free (buffer);
  return ret;
}

static void dispatch_channel_join 
(gzochi_client_session *session, char *name, unsigned char *id, short id_len)
{
  int i;
  gzochi_client_channel *channel = gzochi_client_channel_new 
    (session, name, id, id_len);

  channel->connected = TRUE;

  for (i = 0; i < session->channels_length; i++)
    {
      if (session->channels[i] == NULL)
	{
	  session->channels[i] = channel;
	  break;
	}
    }
  
  if (i == session->channels_length)
    {
      gzochi_client_channel **new_channels = 
	calloc (session->channels_length * 2, sizeof (gzochi_client_channel *));

      memcpy (new_channels, session->channels, 
	      sizeof (gzochi_client_channel *) * session->channels_length);
      free (session->channels);
      session->channels = new_channels;
      session->channels_length *= 2;

      session->channels[i] = channel;
    }
  
  if (session->joined_channel_callback != NULL)
    session->joined_channel_callback (channel);
}

static void dispatch_channel_disconnected 
(gzochi_client_session *session, unsigned char *id, short id_len)
{
  int i;
  gzochi_client_channel *channel = NULL;
  for (i = 0; i < session->channels_length; i++)
    if (session->channels[i] != NULL 
	&& memcmp (session->channels[i]->id, id, id_len) == 0)
      {
	channel = session->channels[i];
	session->channels[i] = NULL;
	break;
      }
  
  if (channel != NULL)
    {
      channel->connected = FALSE;
      if (channel->disconnected_callback != NULL)
	channel->disconnected_callback (channel);
      gzochi_client_channel_free (channel);
    }
}

static void dispatch_channel_message
(gzochi_client_session *session, unsigned char *id, short id_len, 
 unsigned char *message, short message_len)
{
  int i;
  gzochi_client_channel *channel = NULL;

  for (i = 0; i < session->channels_length; i++)
    if (session->channels[i] != NULL 
	&& memcmp (session->channels[i]->id, id, id_len) == 0)
      {
	channel = session->channels[i];
	break;
      }

  if (channel != NULL)
    if (channel->received_message_callback != NULL)
      channel->received_message_callback (channel, message, message_len);
}

static void dispatch_session_message 
(gzochi_client_session *session, unsigned char *message, short len)
{
  if (session->received_message_callback != NULL)
    session->received_message_callback (session, message, len);
}

static void dispatch_session_disconnected (gzochi_client_session *session)
{
  if (session->disconnected_callback != NULL)
    session->disconnected_callback (session);
}

static void dispatch 
(gzochi_client_session *session, int opcode, unsigned char *payload, short len)
{
  unsigned short channel_name_len;
  char *channel_name;

  unsigned short channel_id_len;
  unsigned short channel_message_len;

  switch (opcode)
    {
    case GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE:
      dispatch_session_message (session, payload, len); break;
    case GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED:
      dispatch_session_disconnected (session); break;

    case GZOCHI_COMMON_PROTOCOL_CHANNEL_JOIN:
      channel_name_len = gzochi_common_io_read_short (payload, 0);
      channel_name = calloc (channel_name_len + 1, sizeof (char));
      channel_name = memcpy (channel_name, payload + 2, channel_name_len);
      
      channel_id_len = gzochi_common_io_read_short 
	(payload, channel_name_len + 2);
      
      dispatch_channel_join 
	(session, channel_name, payload + channel_name_len + 4, channel_id_len);
      
      free (channel_name);

      break;

    case GZOCHI_COMMON_PROTOCOL_CHANNEL_DISCONNECTED:

      channel_id_len = gzochi_common_io_read_short (payload, 0);
      dispatch_channel_disconnected (session, payload + 2, channel_id_len); 

      break;
    
    case GZOCHI_COMMON_PROTOCOL_CHANNEL_MESSAGE:
      channel_id_len = gzochi_common_io_read_short (payload, 0);
      channel_message_len = gzochi_common_io_read_short
	(payload, channel_id_len + 2);

      dispatch_channel_message 
	(session, 
	 payload + 2, channel_id_len, 
	 payload + channel_id_len + 4, channel_message_len);

    default:
      break;
    }
}

static int attempt_dispatch (gzochi_client_session *session, int limit)
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
      memmove (session->buffer, session->buffer + offset, offset);
      session->buffer_length -= offset;
    }

  return dispatched;
}

static int read (gzochi_client_session *session)
{
  char chunk[CHUNK_SIZE];
  int available_buffer = GZOCHI_CLIENT_MAX_BUFFER_SIZE - session->buffer_length;

  while (available_buffer > 0)
    {
      int bytes_requested = MIN(CHUNK_SIZE, available_buffer);
      int bytes_read = recv (session->socket, chunk, bytes_requested, 0);

      if (bytes_read < 0)
	return -1;
      
      memcpy (session->buffer + session->buffer_length, chunk, bytes_read);
      session->buffer_length += bytes_read;
      available_buffer -= bytes_read;

      if (bytes_read < bytes_requested)
	break;
    }

  return 0;
}

static int read_and_dispatch (gzochi_client_session *session, int limit)
{
  int remaining = limit;
  int dispatched = attempt_dispatch (session, limit);

  while (TRUE)
    {
      if (limit != 0)
	{
	  remaining -= dispatched;
	  if (remaining <= 0)
	    break;
	}
  
      if (read (session) < 0)
	{
	  dispatch_session_disconnected (session);
	  return -1;
	}

      dispatched = attempt_dispatch (session, limit == 0 ? 0 : remaining);
    }

  return 0;
}

void gzochi_protocol_run (gzochi_client_session *session)
{
  read_and_dispatch (session, 0);
}

void gzochi_protocol_run_once (gzochi_client_session *session)
{
  read_and_dispatch (session, 1);
}
