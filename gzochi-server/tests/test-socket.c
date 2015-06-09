/* test-socket.c: Test routines for socket.c in gzochid.
 * Copyright (C) 2015 Julian Graham
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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "context.h"
#include "game.h"
#include "log.h"
#include "protocol.h"
#include "socket.h"

gboolean gzochid_transaction_active () { return FALSE; }

static GMutex connect_mutex;
static GCond connect_cond;

static gzochid_client_socket *test_socket;

void
gzochid_protocol_client_disconnected (gzochid_protocol_client *client) { }

void
gzochid_protocol_client_dispatch (gzochid_protocol_client *client,
				  unsigned char *data, short len)
{
}

void
gzochid_protocol_client_free (gzochid_protocol_client *client) { }

gzochid_protocol_client *
gzochid_protocol_client_accept (gzochid_client_socket *sock)
{
  g_mutex_lock (&connect_mutex);

  test_socket = sock;
  
  g_cond_signal (&connect_cond);
  g_mutex_unlock (&connect_mutex);

  return NULL;
}

static void
test_socket_free ()
{
  GSource *client_read_source = NULL;
  gzochid_game_context *game_context =
    calloc (1, sizeof (gzochid_game_context));
  gzochid_socket_server_context *context = gzochid_socket_server_context_new ();
  int sock = socket (PF_INET, SOCK_STREAM, 0);
  
  gzochid_socket_server_context_init
    (context, (gzochid_context *) game_context, 0);

  g_mutex_lock (&connect_mutex);  
  assert (connect (sock, context->addr, context->addrlen) == 0);
  while (test_socket == NULL)
    g_cond_wait (&connect_cond, &connect_mutex);
  g_mutex_unlock (&connect_mutex);

  client_read_source = g_main_context_find_source_by_user_data
    (context->main_context, test_socket);
  
  g_source_ref (client_read_source);
  gzochid_client_socket_free (test_socket);
  g_assert (g_source_is_destroyed (client_read_source));
  g_source_unref (client_read_source);
  
  gzochid_socket_server_context_free (context);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);
  g_test_add_func ("/socket/free", test_socket_free);
  
  return g_test_run ();
}
