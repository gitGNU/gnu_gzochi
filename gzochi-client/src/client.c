/* client.c: Main client API routines for libgzochi
 * Copyright (C) 2012 Julian Graham
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
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "io.h"
#include "protocol.h"
#include "session.h"

void gzochi_client_run (gzochi_client_session *session)
{
  gzochi_protocol_run (session);
}

gzochi_client_session *gzochi_client_connect 
(char *hostname, int port, char *endpoint, unsigned char *credentials, 
 int credentials_length)
{
  int sock;
  struct sockaddr_in name;
  struct hostent *hostinfo = NULL;
  gzochi_client_session *session = NULL;
  
  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    return NULL;

  name.sin_family = AF_INET;
  name.sin_port = htons (port);
  hostinfo = gethostbyname (hostname);

  if (hostinfo == NULL)
    return NULL;

  name.sin_addr  = *(struct in_addr *) hostinfo->h_addr;

  if (connect (sock, (struct sockaddr *) &name, 
	       sizeof (struct sockaddr_in)) < 0)
    return NULL;

  session = gzochi_client_session_new ();
  session->connected = TRUE;
  session->socket = sock;

  gzochi_protocol_send_login_request 
    (session, endpoint, credentials, credentials_length);
  gzochi_protocol_run_once (session);
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
  gzochi_client_session_disconnect (session);
}

void gzochi_client_send 
(gzochi_client_session *session, unsigned char *msg, short len)
{
  gzochi_protocol_send_session_message (session, msg, len);
}
