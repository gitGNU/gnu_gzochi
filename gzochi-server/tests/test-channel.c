/* test-channel.c: Tests for channel.c in gzochid.
 * Copyright (C) 2017 Julian Graham
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
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>

#include "app.h"
#include "channel.h"
#include "game-protocol.h"
#include "socket.h"

struct _channel_fixture
{
  gzochid_application_context *app_context;
  gzochid_client_socket *client_socket;
  gzochid_game_client *game_client;

  GIOChannel *read_channel;
};

typedef struct _channel_fixture channel_fixture;

static gzochid_client_socket *
create_pipe_client (int *read_fd)
{
  int pipefd[2] = { 0 };
  int pipe_fd = pipe (pipefd);
  GIOChannel *write_channel = g_io_channel_unix_new (pipefd[1]);  
  gzochid_client_socket *client_socket = gzochid_game_server_protocol.accept
    (write_channel, "", NULL);

  g_io_channel_set_encoding (write_channel, NULL, NULL);

  if (read_fd != NULL)
    *read_fd = pipefd[0];

  return client_socket;
}

static void
channel_fixture_setup (channel_fixture *fixture, gconstpointer user_data)
{
  int read_fd = 0;
  
  fixture->app_context = gzochid_application_context_new ();
  fixture->client_socket = create_pipe_client (&read_fd);    
  fixture->game_client = _gzochid_client_socket_get_protocol_data
    (fixture->client_socket);

  fixture->read_channel = g_io_channel_unix_new (read_fd);

  g_io_channel_set_flags (fixture->read_channel, G_IO_FLAG_NONBLOCK, NULL);
}

static void
channel_fixture_teardown (channel_fixture *fixture, gconstpointer user_data)
{
  gzochid_client_socket_unref (fixture->client_socket);
  gzochid_application_context_free (fixture->app_context);

  g_io_channel_unref (fixture->read_channel);
}

static void
register_client (gzochid_application_context *app_context, guint64 oid,
		 gzochid_game_client *client)
{
  g_hash_table_insert
    (app_context->oids_to_clients, g_memdup (&oid, sizeof (guint64)), client);
  g_hash_table_insert
    (app_context->clients_to_oids, client, g_memdup (&oid, sizeof (guint64)));
}

static void
test_join_direct (channel_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  
  gzochid_channel_join_direct (fixture->app_context, 1, 2, &err);

  g_assert_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MAPPED);
  g_clear_error (&err);
  
  register_client (fixture->app_context, 2, fixture->game_client);
  gzochid_channel_join_direct (fixture->app_context, 1, 2, &err);
  g_assert_no_error (err);

  gzochid_channel_join_direct (fixture->app_context, 1, 2, &err);
  
  g_assert_error
    (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_ALREADY_MEMBER);
  g_clear_error (&err);
}

static void
test_leave_direct (channel_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;

  gzochid_channel_leave_direct (fixture->app_context, 1, 2, &err);

  g_assert_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MAPPED);
  g_clear_error (&err);

  register_client (fixture->app_context, 2, fixture->game_client);
  gzochid_channel_leave_direct (fixture->app_context, 1, 2, &err);

  g_assert_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MEMBER);
  g_clear_error (&err);

  gzochid_channel_join_direct (fixture->app_context, 1, 2, NULL);
  gzochid_channel_leave_direct (fixture->app_context, 1, 2, &err);

  g_assert_no_error (err);
}

static void
test_close_direct (channel_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;

  register_client (fixture->app_context, 2, fixture->game_client);
  gzochid_channel_join_direct (fixture->app_context, 1, 2, NULL);
  gzochid_channel_close_direct (fixture->app_context, 1);
  gzochid_channel_leave_direct (fixture->app_context, 1, 2, &err);

  g_assert_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MEMBER);
  g_clear_error (&err);
}

static void
test_message_direct (channel_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *msg = g_bytes_new_static ("foo\n", 5);
  GString *str = g_string_new ("");
  GzochidSocketServer *socket_server =
    g_object_new (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  
  register_client (fixture->app_context, 2, fixture->game_client);
  gzochid_client_socket_listen (socket_server, fixture->client_socket);

  gzochid_channel_join_direct (fixture->app_context, 1, 2, NULL);
  gzochid_channel_message_direct (fixture->app_context, 1, msg);

  g_main_context_iteration (socket_server->main_context, TRUE);

  g_io_channel_read_line_string (fixture->read_channel, str, NULL, &err);
  g_assert_cmpstr (str->str, ==, "\x00\x05\x31""foo\n");
  
  g_object_unref (socket_server);
  g_string_free (str, TRUE);
  g_bytes_unref (msg);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add ("/channel/join/direct", channel_fixture, NULL,
	      channel_fixture_setup, test_join_direct,
	      channel_fixture_teardown);
  g_test_add ("/channel/leave/direct", channel_fixture, NULL,
	      channel_fixture_setup, test_leave_direct,
	      channel_fixture_teardown);
  g_test_add ("/channel/close/direct", channel_fixture, NULL,
	      channel_fixture_setup, test_close_direct,
	      channel_fixture_teardown);
  g_test_add ("/channel/message/direct", channel_fixture, NULL,
	      channel_fixture_setup, test_message_direct,
	      channel_fixture_teardown);
  
  return g_test_run ();
}
