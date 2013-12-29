/* client.c: GSource interface for libgzochi-glib
 * Copyright (C) 2013 Julian Graham
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

#include <fcntl.h>
#include <glib.h>
#include <gzochi-client-common.h>
#include <stdlib.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

typedef gzochi_client_common_session gzochi_glib_client_session;

typedef gzochi_client_common_session_disconnected_callback
gzochi_glib_client_session_disconnected_callback;

typedef gzochi_client_common_session_received_message_callback
gzochi_glib_client_session_received_message_callback;

typedef struct _GzochiSource
{
  GSource source;
  gzochi_glib_client_session *session;
  GPollFD *poll_fd;
} GzochiSource;

static gboolean prepare (GSource *source, gint *timeout)
{
  GzochiSource *gzochi_source = (GzochiSource *) source;
  return gzochi_client_common_session_is_dispatchable 
    (gzochi_source->session);
}

static gboolean check (GSource *source)
{
  GzochiSource *gzochi_source = (GzochiSource *) source;
  GPollFD *poll_fd = gzochi_source->poll_fd;

  if (poll_fd->revents & G_IO_IN)
    {
      gzochi_client_protocol_read (gzochi_source->session);
      return gzochi_client_common_session_is_dispatchable 
	(gzochi_source->session);
    }
  if (poll_fd->revents & G_IO_HUP || poll_fd->revents & G_IO_ERR)
    return TRUE;
  return FALSE;
}

static gboolean dispatch (GSource *source, GSourceFunc cb, gpointer user_data)
{
  GzochiSource *gzochi_source = (GzochiSource *) source;
  return gzochi_client_protocol_dispatch_all (gzochi_source->session) > 0;
}

static void finalize (GSource *source)
{
  GzochiSource *gzochi_source = (GzochiSource *) source;
  free (gzochi_source->poll_fd);
}

static GSourceFuncs source_funcs = { prepare, check, dispatch, finalize };

GzochiSource *gzochi_source_new (gzochi_glib_client_session *session)
{
  GzochiSource *source = (GzochiSource *) g_source_new 
    (&source_funcs, sizeof (GzochiSource));
  GPollFD *poll_fd = malloc (sizeof (GPollFD));

  poll_fd->fd = session->socket;
  poll_fd->events = G_IO_IN | G_IO_HUP | G_IO_ERR;
  poll_fd->revents = 0;

  g_source_add_poll ((GSource *) source, poll_fd);
  source->poll_fd = poll_fd;

  fcntl (session->socket, F_SETFL, O_NONBLOCK);

  source->session = session;
  return source;
}

static int 
read_and_dispatch (gzochi_glib_client_session *session, int connecting)
{
  while (TRUE)
    {
      if (gzochi_client_protocol_dispatch (session))
	return 0;
  
      if (gzochi_client_protocol_read (session) < 0)
	{
	  if (session->disconnected_callback != NULL)
	    session->disconnected_callback 
	      (session, session->disconnected_user_data);
	  
	  return -1;
	}
    }
}

gzochi_glib_client_session *gzochi_glib_client_connect
(char *hostname, int port, char *endpoint, unsigned char *credentials, 
 int credentials_length)
{
  int sock;
  struct sockaddr_in name;
  struct hostent *hostinfo = NULL;
  gzochi_glib_client_session *session = NULL;
  int flag = 1;

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

  setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof (int));

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

void gzochi_glib_client_disconnect (gzochi_glib_client_session *session)
{
  gzochi_client_common_session_disconnect (session);
}

void gzochi_glib_client_send 
(gzochi_glib_client_session *session, unsigned char *msg, short len)
{
  if (gzochi_client_protocol_send_session_message (session, msg, len) < 0)
    gzochi_client_common_session_disconnect (session);
}

char *gzochi_glib_client_session_endpoint (gzochi_glib_client_session *session)
{
  return gzochi_client_common_session_endpoint (session);
}

char *gzochi_glib_client_session_hostname (gzochi_glib_client_session *session)
{
  return gzochi_client_common_session_hostname (session);
}

int gzochi_glib_client_session_port (gzochi_glib_client_session *session)
{
  return gzochi_client_common_session_port (session);
}

void gzochi_glib_client_session_set_disconnected_callback
(gzochi_glib_client_session *session, 
 gzochi_glib_client_session_disconnected_callback callback, gpointer user_data)
{
  gzochi_client_common_session_set_disconnected_callback 
    (session, callback, user_data);
}

void gzochi_glib_client_session_set_received_message_callback
(gzochi_glib_client_session *session, 
 gzochi_glib_client_session_received_message_callback callback, 
 gpointer user_data)
{
  gzochi_client_common_session_set_received_message_callback 
    (session, callback, user_data);
}
