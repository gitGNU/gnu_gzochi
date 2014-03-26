/* session.c: Session management routines for libgzochi
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

#include "session.h"

char *gzochi_client_common_session_endpoint 
(gzochi_client_common_session *session)
{
  return session->endpoint;
}

char *gzochi_client_common_session_hostname 
(gzochi_client_common_session *session)
{
  return session->hostname;
}

int gzochi_client_common_session_port 
(gzochi_client_common_session *session)
{
  return session->port;
}

void gzochi_client_common_session_set_disconnected_callback
(gzochi_client_common_session *session, 
 void (*cb) (gzochi_client_common_session *, void *), void *user_data)
{
  session->disconnected_callback = cb;
  session->disconnected_user_data = user_data;
}

void gzochi_client_common_session_set_received_message_callback
(gzochi_client_common_session *session,
 void (*cb) (gzochi_client_common_session *, unsigned char *, short, void *),
 void *user_data)
{
  session->received_message_callback = cb;
  session->received_message_user_data = user_data;
}

gzochi_client_common_session *gzochi_client_common_session_new (void)
{
  gzochi_client_common_session *session = 
    calloc (1, sizeof (gzochi_client_common_session));

  return session;
}

int gzochi_client_common_session_is_dispatchable 
(gzochi_client_common_session *session)
{
  short message_len = 0;

  if (! session->connected && ! session->disconnect_acknowledged)
    return TRUE;
  if (session->buffer_length < 3)
    return FALSE;

  message_len = gzochi_common_io_read_short (session->buffer, 0);
  return session->buffer_length >= message_len + 3; 
}

void gzochi_client_common_session_free (gzochi_client_common_session *session)
{
  free (session);
}

void gzochi_client_common_session_disconnect 
(gzochi_client_common_session *session)
{
  session->connected = FALSE;

  if (session->disconnected_callback != NULL)
    session->disconnected_callback (session, session->disconnected_user_data);
}
