/* test-game-protocol.c: Test routines for game-protocol.c in gzochid.
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "config.h"
#include "game.h"
#include "game-protocol.h"
#include "resolver.h"
#include "socket.h"

struct _game_protocol_fixture
{
  GzochidResolutionContext *resolution_context;
  GzochidGameServer *game_server;
  GzochidSocketServer *socket_server;

  gzochid_client_socket *client_socket;
  gzochid_server_socket *server_socket;
};

typedef struct _game_protocol_fixture game_protocol_fixture;

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static gzochid_client_socket *
server_accept_wrapper (GIOChannel *channel, const char *desc, gpointer data)
{
  game_protocol_fixture *fixture = data;
  gzochid_client_socket *ret = gzochid_game_server_protocol.accept
    (channel, desc, fixture->game_server);

  fixture->client_socket = ret;
  return ret;
}

static gzochid_server_protocol game_server_wrapper_protocol =
  { server_accept_wrapper };

static void
game_protocol_fixture_set_up (game_protocol_fixture *fixture,
			      gconstpointer user_data)
{
  struct sockaddr addr;
  size_t addrlen = sizeof (struct sockaddr);
  int socket_fd = socket (AF_INET, SOCK_STREAM, 0);
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  
  g_test_log_set_fatal_handler (ignore_warnings, NULL);
  
  fixture->resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);

  gzochid_resolver_provide
    (fixture->resolution_context, G_OBJECT (configuration), NULL);
  
  fixture->game_server = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_GAME_SERVER, NULL);
  fixture->socket_server = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_SOCKET_SERVER, NULL);
  
  fixture->server_socket = gzochid_server_socket_new
    ("test", game_server_wrapper_protocol, fixture);
  
  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname (fixture->server_socket, &addr, &addrlen);
  connect (socket_fd, &addr, addrlen);

  g_assert (g_main_context_iteration
	    (fixture->socket_server->main_context, FALSE));

  g_assert (fixture->client_socket != NULL);

  g_key_file_unref (key_file);
  g_object_unref (configuration);
}

static void
game_protocol_fixture_tear_down (game_protocol_fixture *fixture,
				 gconstpointer user_data)
{
  GSource *client_source = g_main_context_find_source_by_user_data
    (fixture->socket_server->main_context, fixture->client_socket);

  if (client_source != NULL)
    g_source_unref (client_source);

  g_object_unref (fixture->game_server);
  g_object_unref (fixture->resolution_context);
  g_object_unref (fixture->socket_server);
}

static void
test_server_accept (game_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochid_client_protocol protocol;
  
  g_assert (fixture->client_socket != NULL);

  protocol = _gzochid_client_socket_get_protocol (fixture->client_socket);

  g_assert_cmpint
    (memcmp (&protocol, &gzochid_game_client_protocol,
	     sizeof (gzochid_client_protocol)), ==, 0);
}

static void
test_client_can_dispatch_true (game_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochid_game_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  GByteArray *byte_array = g_byte_array_new ();

  g_byte_array_append (byte_array, "\000\001\020\000", 4);

  g_assert
    (gzochid_game_client_protocol.can_dispatch (byte_array, client));

  g_byte_array_free (byte_array, TRUE);
}

static void
test_client_can_dispatch_false (game_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochid_game_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  GByteArray *byte_array = g_byte_array_new ();
  
  g_assert (! gzochid_game_client_protocol.can_dispatch (byte_array, client));

  g_byte_array_append (byte_array, "\000\001\001", 3);

  g_assert (! gzochid_game_client_protocol.can_dispatch (byte_array, client));

  g_byte_array_free (byte_array, TRUE);
}

static void
test_client_dispatch_one (game_protocol_fixture *fixture,
			  gconstpointer user_data)
{
  gzochid_game_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  unsigned int bytes_dispatched = 0;
  GByteArray *byte_array = g_byte_array_new ();

  g_byte_array_append (byte_array, "\x00\x03\x31\x01\x02\0x03", 6);
  
  bytes_dispatched = gzochid_game_client_protocol.dispatch (byte_array, client);
  
  g_assert_cmpint (bytes_dispatched, ==, 6);

  g_byte_array_free (byte_array, TRUE);
}

static void
test_client_dispatch_multiple (game_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochid_game_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  unsigned int bytes_dispatched = 0;
  GByteArray *byte_array = g_byte_array_new ();

  g_byte_array_append (byte_array, "\x00\x03\x31\x01\x02\0x03", 6);
  g_byte_array_append (byte_array, "\x00\x03\x31\x04\x05\0x06", 6);
  
  bytes_dispatched = gzochid_game_client_protocol.dispatch (byte_array, client);
  
  g_assert_cmpint (bytes_dispatched, ==, 12);

  g_byte_array_free (byte_array, TRUE);
}

static void
test_client_error (game_protocol_fixture *fixture, gconstpointer user_data)
{
  GSource *source = g_main_context_find_source_by_user_data
    (fixture->socket_server->main_context, fixture->client_socket);
  gzochid_game_client *client = _gzochid_client_socket_get_protocol_data
    (fixture->client_socket);

  g_source_ref (source);  

  gzochid_game_client_protocol.error (client);
  g_assert (_gzochid_game_client_disconnected (client));

  g_source_unref (source);
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

  g_test_add ("/server/accept", game_protocol_fixture, NULL,
	      game_protocol_fixture_set_up, test_server_accept,
	      game_protocol_fixture_tear_down);
  g_test_add ("/client/can_dispatch/true", game_protocol_fixture, NULL,
	      game_protocol_fixture_set_up, test_client_can_dispatch_true,
	      game_protocol_fixture_tear_down);

  g_test_add
    ("/client/can_dispatch/false", game_protocol_fixture, NULL,
     game_protocol_fixture_set_up, test_client_can_dispatch_false,
     game_protocol_fixture_tear_down);
  
  g_test_add ("/client/dispatch/one", game_protocol_fixture, NULL,
	      game_protocol_fixture_set_up, test_client_dispatch_one,
	      game_protocol_fixture_tear_down);
  g_test_add ("/client/dispatch/multiple", game_protocol_fixture, NULL,
	      game_protocol_fixture_set_up, test_client_dispatch_multiple,
	      game_protocol_fixture_tear_down);
  g_test_add ("/client/error", game_protocol_fixture, NULL,
	      game_protocol_fixture_set_up, test_client_error,
	      game_protocol_fixture_tear_down);
  
  return g_test_run ();
}
