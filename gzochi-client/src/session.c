/* session.c: Session management routines for libgzochi
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

#include "session.h"

char *gzochi_client_session_endpoint (gzochi_client_session *session)
{
  return session->endpoint;
}

char *gzochi_client_session_hostname (gzochi_client_session *session)
{
  return session->hostname;
}

int gzochi_client_session_port (gzochi_client_session *session)
{
  return session->port;
}

void gzochi_client_session_set_disconnected_callback
(gzochi_client_session *session, void (*callback) (gzochi_client_session *))
{
  session->disconnected_callback = callback;
}

void gzochi_client_session_set_joined_channel_callback
(gzochi_client_session *session, void (*callback) (gzochi_client_channel *))
{
  session->joined_channel_callback = callback;
}

void gzochi_client_session_set_received_message_callback
(gzochi_client_session *session, 
 void (*callback) (gzochi_client_session *, unsigned char *, short))
{
  session->received_message_callback = callback;
}

gzochi_client_session *gzochi_client_session_new (void)
{
  return calloc (1, sizeof (gzochi_client_session));
}

void gzochi_client_session_free (gzochi_client_session *session)
{
  free (session);
}

void gzochi_client_session_disconnect (gzochi_client_session *session)
{
  int i = 0;

  session->connected = FALSE;

  for (; i < session->channels_length; i++)
    {
      gzochi_client_channel *channel = session->channels[i];
      if (channel != NULL && channel->disconnected_callback != NULL)
	channel->disconnected_callback (channel);
    }

  if (session->disconnected_callback != NULL)
    session->disconnected_callback (session);
}
