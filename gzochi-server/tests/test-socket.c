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
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include "context.h"
#include "log.h"
#include "protocol.h"
#include "socket.h"

struct _test_client_state
{
  int accept_called;
  int can_dispatch_called;
  int dispatch_called;
  int error_called;
  int free_called;
};

typedef struct _test_client_state test_client_state;

struct _test_socket_fixture
{
  gzochid_socket_context *socket_context;
  gzochid_server_socket *server_socket;
  gzochid_client_socket *client_socket;
  int client_socket_fd;
  
  test_client_state *state;
};

typedef struct _test_socket_fixture test_socket_fixture;

static gboolean
test_client_can_dispatch (const GByteArray *arr, gpointer user_data)
{
  test_client_state *state = user_data;

  state->can_dispatch_called++;

  return TRUE;
}

static unsigned int
test_client_dispatch (const GByteArray *arr, gpointer user_data)
{
  test_client_state *state = user_data;

  state->dispatch_called++;

  return arr->len;
}

static void
test_client_error (gpointer user_data)
{
  test_client_state *state = user_data;

  state->error_called++;
}

static void
test_client_free (gpointer user_data)
{
  test_client_state *state = user_data;

  state->free_called++;
}

static gzochid_client_protocol test_client_protocol =
  {
    test_client_can_dispatch,
    test_client_dispatch,
    test_client_error,
    test_client_free
  };

static gzochid_client_socket *
test_server_accept (GIOChannel *channel, const char *conn_desc,
		    gpointer user_data)
{
  test_socket_fixture *fixture = user_data;

  fixture->client_socket = gzochid_client_socket_new
    (channel, conn_desc, test_client_protocol, fixture->state);
  fixture->state->accept_called++;
  
  return fixture->client_socket;
}

static gzochid_server_protocol test_server_protocol = { test_server_accept };

static void
test_socket_fixture_set_up (test_socket_fixture *fixture,
			    gconstpointer user_data)
{
  struct sockaddr addr;
  size_t addrlen = sizeof (struct sockaddr);

  fixture->client_socket_fd = socket (AF_INET, SOCK_STREAM, 0);
  fixture->state = calloc (1, sizeof (test_client_state));
  fixture->socket_context = gzochid_socket_context_new ();
  fixture->server_socket = gzochid_server_socket_new
    (test_server_protocol, fixture);

  gzochid_server_socket_listen
    (fixture->socket_context, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname (fixture->server_socket, &addr, &addrlen);
  connect (fixture->client_socket_fd, &addr, addrlen);

  g_assert_true
    (g_main_context_iteration
     (fixture->socket_context->main_context, FALSE));

  g_assert_nonnull (fixture->client_socket);
}

static void
test_socket_fixture_tear_down (test_socket_fixture *fixture,
			       gconstpointer user_data)
{
  GSource *server_source = g_main_context_find_source_by_user_data
    (fixture->socket_context->main_context, fixture->server_socket);
  GSource *client_source = g_main_context_find_source_by_user_data
    (fixture->socket_context->main_context, fixture->client_socket);
  
  g_source_destroy (server_source);
  g_source_unref (server_source);

  if (client_source != NULL)
    {
      g_source_destroy (client_source);
      g_source_unref (client_source);
    }

  free (fixture->state);
  gzochid_socket_context_free (fixture->socket_context);
}

static void
test_socket_server_accept (test_socket_fixture *fixture,
			   gconstpointer user_data)
{
  g_assert_cmpint (fixture->state->accept_called, ==, 1);
}

static void
test_socket_client_dispatch (test_socket_fixture *fixture,
			     gconstpointer user_data)
{
  write (fixture->client_socket_fd, "\x01\x02\x03", 3);

  g_main_context_iteration (fixture->socket_context->main_context, FALSE);
  
  g_assert_cmpint (fixture->state->can_dispatch_called, ==, 1);
  g_assert_cmpint (fixture->state->dispatch_called, ==, 1);
  g_assert_cmpint (fixture->state->error_called, ==, 0);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/socket/server/accept", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_socket_server_accept,
     test_socket_fixture_tear_down);
  g_test_add
    ("/socket/client/dispatch", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_socket_client_dispatch,
     test_socket_fixture_tear_down);
    
  return g_test_run ();
}
