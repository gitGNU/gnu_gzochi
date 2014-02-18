/* client.c: Main client API routines for libgzochi
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

#include <gzochi-client-common.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

typedef gzochi_client_common_session gzochi_client_session;

typedef gzochi_client_common_session_disconnected_callback
gzochi_client_session_disconnected_callback;

typedef gzochi_client_common_session_received_message_callback
gzochi_client_session_received_message_callback;

static int read_and_dispatch (gzochi_client_session *session, int connecting)
{
  do
    {
      int dispatched = connecting 
	? gzochi_client_protocol_dispatch (session)
	: gzochi_client_protocol_dispatch_all (session);

      if (connecting && dispatched)
	break;
  
      if (gzochi_client_protocol_read (session) < 0)
	{
	  session->connected = FALSE;
	  gzochi_client_common_session_disconnect (session);
	  return -1;
	}
    }
  while (TRUE);

  return 0;
}

void gzochi_client_run (gzochi_client_session *session)
{
  read_and_dispatch (session, FALSE);
}

gzochi_client_session *gzochi_client_connect 
(char *hostname, int port, char *endpoint, unsigned char *credentials, 
 int credentials_length)
{
  int sock;
  struct sockaddr_in name;
  struct hostent *hostinfo = NULL;
  gzochi_client_session *session = NULL;
  int flag = 1;

  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    return NULL;

  name.sin_family = AF_INET;
  name.sin_port = htons (port);
  hostinfo = gethostbyname (hostname);

  if (hostinfo == NULL)
    return NULL;

  name.sin_addr = *(struct in_addr *) hostinfo->h_addr;

  if (connect (sock, (struct sockaddr *) &name, 
	       sizeof (struct sockaddr_in)) < 0)
    return NULL;

  setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof (int));
#if defined (__APPLE__) && defined (__MACH__)
  setsockopt (sock, SOL_SOCKET, SO_NOSIGPIPE, (char *) &flag, sizeof (int));
#endif

  session = gzochi_client_common_session_new ();
  session->connected = TRUE;
  session->socket = sock;

  gzochi_client_protocol_send_login_request 
    (session, endpoint, credentials, credentials_length);

  read_and_dispatch (session, TRUE);

  if (session->connected)
    return session;
  else
    {
      free (session);
      return NULL;
    }
}

void gzochi_client_disconnect (gzochi_client_session *session)
{
  gzochi_client_common_session_disconnect (session);
}

void gzochi_client_send 
(gzochi_client_session *session, unsigned char *msg, short len)
{
  if (gzochi_client_protocol_send_session_message (session, msg, len) < 0)
    gzochi_client_common_session_disconnect (session);
}

char *gzochi_client_session_endpoint (gzochi_client_session *session)
{
  return gzochi_client_common_session_endpoint (session);
}

char *gzochi_client_session_hostname (gzochi_client_session *session)
{
  return gzochi_client_common_session_hostname (session);
}

int gzochi_client_session_port (gzochi_client_session *session)
{
  return gzochi_client_common_session_port (session);
}

void gzochi_client_session_set_disconnected_callback
(gzochi_client_session *session, 
 gzochi_client_session_disconnected_callback callback, void *user_data)
{
  gzochi_client_common_session_set_disconnected_callback 
    (session, callback, user_data);
}

void gzochi_client_session_set_received_message_callback
(gzochi_client_session *session, 
 gzochi_client_session_received_message_callback callback, void *user_data)
{
  gzochi_client_common_session_set_received_message_callback 
    (session, callback, user_data);
}
