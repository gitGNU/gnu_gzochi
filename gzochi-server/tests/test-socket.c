/* test-socket.c: Test routines for socket.c in gzochid.
 * Copyright (C) 2016 Julian Graham
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
#include <fcntl.h>
#include <glib.h>
#include <glib-object.h>
#include <glib-unix.h>
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
  GzochidSocketServer *socket_server;
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
  fixture->socket_server = g_object_new (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->server_socket = gzochid_server_socket_new
    ("test", test_server_protocol, fixture);

  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname (fixture->server_socket, &addr, &addrlen);
  connect (fixture->client_socket_fd, &addr, addrlen);

  g_assert
    (g_main_context_iteration
     (fixture->socket_server->main_context, FALSE));

  g_assert (fixture->client_socket != NULL);
}

static void
test_socket_fixture_tear_down (test_socket_fixture *fixture,
			       gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  free (fixture->state);
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

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint (fixture->state->can_dispatch_called, ==, 1);
  g_assert_cmpint (fixture->state->dispatch_called, ==, 1);
  g_assert_cmpint (fixture->state->error_called, ==, 0);
}

static void
test_socket_client_listen ()
{
  GError *err = NULL;
  int pipe_flags = 0;
  gint pipe_fds[2] = { 0 };
  test_client_state state = { 0 };
  GzochidSocketServer *socket_server = g_object_new
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  gzochid_client_socket *client_socket = NULL;

  g_unix_open_pipe (pipe_fds, 0, &err);
  g_assert_no_error (err);

  pipe_flags = fcntl (pipe_fds[0], F_GETFL, 0);
  fcntl (pipe_fds[0], F_SETFL, pipe_flags | O_NONBLOCK);

  pipe_flags = fcntl (pipe_fds[1], F_GETFL, 0);
  fcntl (pipe_fds[1], F_SETFL, pipe_flags | O_NONBLOCK);

  client_socket = gzochid_client_socket_new
    (g_io_channel_unix_new (pipe_fds[0]), "test", test_client_protocol, &state);

  gzochid_client_socket_listen (socket_server, client_socket);

  write (pipe_fds[1], "\x01\x02\x03", 3);
  
  g_main_context_iteration (socket_server->main_context, FALSE);
  g_assert_cmpint (state.can_dispatch_called, ==, 1);

  g_object_unref (socket_server);
  gzochid_client_socket_unref (client_socket);
}

static void
test_reconnectable_socket_simple (test_socket_fixture *fixture,
				  gconstpointer user_data)
{
  gzochid_reconnectable_socket *rsock = gzochid_reconnectable_socket_new ();

  gzochid_reconnectable_socket_write (rsock, "foo", 4);
  g_assert (!g_main_context_iteration
	    (fixture->socket_server->main_context, FALSE));

  gzochid_reconnectable_socket_connect (rsock, fixture->client_socket);
  g_assert (g_main_context_iteration
	    (fixture->socket_server->main_context, FALSE));

  gzochid_reconnectable_socket_free (rsock);
}

struct _reconnectable_socket_callback_data
{
  gboolean connect_triggered;
  gboolean disconnect_triggered;
};

typedef struct _reconnectable_socket_callback_data
reconnectable_socket_callback_data;

static void
test_on_connect (gpointer user_data)
{
  reconnectable_socket_callback_data *callback_data = user_data;
  callback_data->connect_triggered = TRUE;
}

static void
test_on_disconnect (gpointer user_data)
{
  reconnectable_socket_callback_data *callback_data = user_data;
  callback_data->disconnect_triggered = TRUE;
}

static void
test_reconnectable_socket_on_connect (test_socket_fixture *fixture,
				      gconstpointer user_data)
{
  gzochid_reconnectable_socket *rsock = gzochid_reconnectable_socket_new ();
  reconnectable_socket_callback_data callback_data = { FALSE };
  
  gzochid_reconnectable_socket_listen
    (rsock, test_on_connect, &callback_data, test_on_disconnect,
     &callback_data);

  gzochid_reconnectable_socket_connect (rsock, fixture->client_socket);

  g_assert (callback_data.connect_triggered);
  g_assert (!callback_data.disconnect_triggered);
  
  gzochid_reconnectable_socket_free (rsock);
}

static void
test_reconnectable_socket_on_disconnect (test_socket_fixture *fixture,
					 gconstpointer user_data)
{
  gzochid_reconnectable_socket *rsock = gzochid_reconnectable_socket_new ();
  reconnectable_socket_callback_data callback_data = { FALSE };

  gzochid_reconnectable_socket_connect (rsock, fixture->client_socket);
  gzochid_reconnectable_socket_listen
    (rsock, test_on_connect, &callback_data, test_on_disconnect,
     &callback_data);

  gzochid_reconnectable_socket_disconnect (rsock);
  
  g_assert (!callback_data.connect_triggered);
  g_assert (callback_data.disconnect_triggered);

  gzochid_reconnectable_socket_free (rsock);
}

int
main (int argc, char *argv[])
{
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/socket/server/accept", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_socket_server_accept,
     test_socket_fixture_tear_down);
  g_test_add
    ("/socket/client/dispatch", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_socket_client_dispatch,
     test_socket_fixture_tear_down);
  g_test_add_func ("/socket/client/listen", test_socket_client_listen);

  g_test_add
    ("/socket/reconnectable/simple", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_reconnectable_socket_simple,
     test_socket_fixture_tear_down);
  g_test_add
    ("/socket/reconnectable/on-connect", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_reconnectable_socket_on_connect,
     test_socket_fixture_tear_down);
  g_test_add
    ("/socket/reconnectable/on-disconnect", test_socket_fixture, NULL,
     test_socket_fixture_set_up, test_reconnectable_socket_on_disconnect,
     test_socket_fixture_tear_down);
  
  return g_test_run ();
}
