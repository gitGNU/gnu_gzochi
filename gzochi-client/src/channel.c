/* channel.c: Channel management routines for libgzochi
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
#include <stdlib.h>

#include "channel.h"
#include "protocol.h"
#include "session.h"

gzochi_client_channel *gzochi_client_channel_new 
(gzochi_client_session *session, char *name)
{
  gzochi_client_channel *channel = calloc (1, sizeof (gzochi_client_channel));

  channel->session = session;
  channel->name = name;

  return channel;
}

void gzochi_client_channel_free (gzochi_client_channel *channel)
{
  free (channel);
}

gzochi_client_session *gzochi_client_channel_session 
(gzochi_client_channel *channel)
{
  return channel->session;
}

char *gzochi_client_channel_name (gzochi_client_channel *channel)
{
  return channel->name;
}

void gzochi_client_channel_send 
(gzochi_client_channel *channel, unsigned char *data, short len)
{
  if (gzochi_protocol_send_channel_message 
      (channel->session, channel->name, data, len) < 0)
    assert (1 == 0);
}

void gzochi_client_channel_set_disconnected_callback
(gzochi_client_channel *channel, void (*callback) (gzochi_client_channel *))
{
  channel->disconnected_callback = callback;
}

void gzochi_client_channel_set_received_message_callback
(gzochi_client_channel *channel, 
 void (*callback) (gzochi_client_channel *, unsigned char *, short))
{
  channel->received_message_callback = callback;
}
