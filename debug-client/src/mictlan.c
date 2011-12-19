/* mictlan.c: Simple debugging client for gzochi
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
#include <libgzochi.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void *reader_loop (void *arg)
{
  gzochi_client_session *session = (gzochi_client_session *) arg;

  char line[USHRT_MAX + 1];
  unsigned short len = USHRT_MAX;

  while (fgets (line, len + 1, stdin) != NULL)
    gzochi_client_send (session, (unsigned char *) line, (short) strlen (line));

  return NULL;
}

static void received_message 
(gzochi_client_session *session, unsigned char *msg, short len)
{
  fprintf (stderr, "[Message] %.*s\n", len, (char *) msg);
}

static void left_channel (gzochi_client_channel *channel)
{
  char *name = gzochi_client_channel_name (channel);
  fprintf (stderr, "[Left channel \"%s\"]\n", name);
}

static void received_channel_message
(gzochi_client_channel *channel, unsigned char *msg, short len)
{
  char *name = gzochi_client_channel_name (channel);
  fprintf (stderr, "[Channel message/%s] %.*s\n", name, len, (char *) msg);
}

static void joined_channel (gzochi_client_channel *channel)
{
  char *name = gzochi_client_channel_name (channel);
  fprintf (stderr, "[Joined channel \"%s\"]\n", name);
  
  gzochi_client_channel_set_disconnected_callback (channel, left_channel);
  gzochi_client_channel_set_received_message_callback 
    (channel, received_channel_message);
}

int main (int argc, char *argv[])
{
  gzochi_client_session *session = NULL;
  char *hostname = NULL, *endpoint = NULL;
  int port = 0;

  assert (argc == 4);

  hostname = argv[1];
  port = atoi (argv[2]);
  endpoint = argv[3];

  session = gzochi_client_connect 
    (hostname, port, endpoint, (unsigned char *) "mictlan", 7);
  if (session != NULL)
    {
      pthread_t reader_thread;

      gzochi_client_session_set_received_message_callback 
	(session, received_message);
      gzochi_client_session_set_joined_channel_callback 
	(session, joined_channel);

      pthread_create (&reader_thread, NULL, reader_loop, session);
      gzochi_client_run (session);
    }
  else printf ("Failed to connect to %s:%d/%s.\n", hostname, port, endpoint);

  return 0;
}
