/* test-channelserver.c: Tests for channelserver.c in gzochi-metad.
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
#include <stdlib.h>

#include "resolver.h"
#include "channelserver.h"
#include "socket.h"

struct _gzochid_client_socket
{
  GByteArray *bytes_written;
};

void
gzochid_client_socket_write (gzochid_client_socket *sock, const
			     unsigned char *data, size_t len)
{
  g_byte_array_append (sock->bytes_written, data, len);
}

static gzochid_client_socket *
gzochid_test_client_socket_new (void)
{
  gzochid_client_socket *socket = malloc (sizeof (gzochid_client_socket));

  socket->bytes_written = g_byte_array_new ();
  
  return socket;
}

static void
gzochid_test_client_socket_free (gzochid_client_socket *socket)
{
  g_byte_array_unref (socket->bytes_written);
  free (socket);
}

struct _channelserver_fixture
{
  GzochiMetadChannelServer *server;
  gzochid_client_socket *client_socket;
  gzochid_client_socket *other_client_1;
  gzochid_client_socket *other_client_2; 
};

typedef struct _channelserver_fixture channelserver_fixture;

static void
setup_channelserver (channelserver_fixture *fixture, gconstpointer user_data)
{
  fixture->server = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_CHANNEL_SERVER, NULL);
  fixture->client_socket = gzochid_test_client_socket_new ();
  fixture->other_client_1 = gzochid_test_client_socket_new ();
  fixture->other_client_2 = gzochid_test_client_socket_new ();
}

static void
teardown_channelserver (channelserver_fixture *fixture, gconstpointer user_data)
{
  g_object_unref (fixture->server);
  gzochid_test_client_socket_free (fixture->client_socket);
  gzochid_test_client_socket_free (fixture->other_client_1);
  gzochid_test_client_socket_free (fixture->other_client_2);
}

static void
test_server_connected (channelserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, &err);
  g_assert_no_error (err);
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, &err);
  g_assert_error (err, GZOCHI_METAD_CHANNELSERVER_ERROR,
		  GZOCHI_METAD_CHANNELSERVER_ERROR_ALREADY_CONNECTED);

  g_error_free (err);
}

static void
test_server_disconnected (channelserver_fixture *fixture,
			  gconstpointer user_data)
{
  GError *err = NULL;

  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, NULL);
  
  gzochi_metad_channelserver_server_disconnected (fixture->server, 1, &err);
  g_assert_no_error (err);

  gzochi_metad_channelserver_server_disconnected (fixture->server, 1, &err);
  g_assert_error (err, GZOCHI_METAD_CHANNELSERVER_ERROR,
		  GZOCHI_METAD_CHANNELSERVER_ERROR_NOT_CONNECTED);

  g_error_free (err);  
}

static inline GBytes *
byte_array_to_bytes (const GByteArray *byte_array)
{
  return g_bytes_new_static (byte_array->data, byte_array->len);
}

static void
test_relay_join (channelserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x15\x71test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);
  GBytes *actual_2 = NULL, *actual_3 = NULL;
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 2, fixture->other_client_1, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 3, fixture->other_client_2, NULL);

  gzochi_metad_channelserver_relay_join (fixture->server, 1, "test", 2, 3);

  g_assert_cmpint (fixture->client_socket->bytes_written->len, ==, 0);
  
  actual_2 = byte_array_to_bytes (fixture->other_client_1->bytes_written); 
  actual_3 = byte_array_to_bytes (fixture->other_client_2->bytes_written); 
  
  g_assert (g_bytes_equal (expected, actual_2));
  g_assert (g_bytes_equal (expected, actual_3));

  g_bytes_unref (expected);
  g_bytes_unref (actual_2);
  g_bytes_unref (actual_3);
}

static void
test_relay_leave (channelserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x15\x73test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);
  GBytes *actual_2 = NULL, *actual_3 = NULL;
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 2, fixture->other_client_1, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 3, fixture->other_client_2, NULL);

  gzochi_metad_channelserver_relay_leave (fixture->server, 1, "test", 2, 3);

  g_assert_cmpint (fixture->client_socket->bytes_written->len, ==, 0);
  
  actual_2 = byte_array_to_bytes (fixture->other_client_1->bytes_written); 
  actual_3 = byte_array_to_bytes (fixture->other_client_2->bytes_written); 
  
  g_assert (g_bytes_equal (expected, actual_2));
  g_assert (g_bytes_equal (expected, actual_3));

  g_bytes_unref (expected);
  g_bytes_unref (actual_2);
  g_bytes_unref (actual_3);
}

static void
test_relay_close (channelserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x0d\x77test\x00\x00\x00\x00\x00\x00\x00\x00\x02", 16);
  GBytes *actual_2 = NULL, *actual_3 = NULL;
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 2, fixture->other_client_1, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 3, fixture->other_client_2, NULL);

  gzochi_metad_channelserver_relay_close (fixture->server, 1, "test", 2);

  g_assert_cmpint (fixture->client_socket->bytes_written->len, ==, 0);
  
  actual_2 = byte_array_to_bytes (fixture->other_client_1->bytes_written); 
  actual_3 = byte_array_to_bytes (fixture->other_client_2->bytes_written); 
  
  g_assert (g_bytes_equal (expected, actual_2));
  g_assert (g_bytes_equal (expected, actual_3));

  g_bytes_unref (expected);
  g_bytes_unref (actual_2);
  g_bytes_unref (actual_3);
}

static void
test_relay_message (channelserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  GBytes *expected = g_bytes_new_static
    ("\x00\x13\x75test\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04""foo", 22);
  GBytes *actual_2 = NULL, *actual_3 = NULL;
  
  gzochi_metad_channelserver_server_connected
    (fixture->server, 1, fixture->client_socket, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 2, fixture->other_client_1, NULL);
  gzochi_metad_channelserver_server_connected
    (fixture->server, 3, fixture->other_client_2, NULL);

  gzochi_metad_channelserver_relay_message (fixture->server, 1, "test", 1, msg);

  actual_2 = byte_array_to_bytes (fixture->other_client_1->bytes_written);
  actual_3 = byte_array_to_bytes (fixture->other_client_2->bytes_written);
  
  g_assert (g_bytes_equal (expected, actual_2));
  g_assert (g_bytes_equal (expected, actual_3));

  g_bytes_unref (msg);
  g_bytes_unref (expected);
  g_bytes_unref (actual_2);
  g_bytes_unref (actual_3);
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

  g_test_add ("/channelserver/server-connected", channelserver_fixture, NULL,
	      setup_channelserver, test_server_connected,
	      teardown_channelserver);
  g_test_add ("/channelserver/server-disconnected", channelserver_fixture, NULL,
	      setup_channelserver, test_server_disconnected,
	      teardown_channelserver);
  g_test_add ("/channelserver/relay-join", channelserver_fixture, NULL,
	      setup_channelserver, test_relay_join,
	      teardown_channelserver);
  g_test_add ("/channelserver/relay-leave", channelserver_fixture, NULL,
	      setup_channelserver, test_relay_leave,
	      teardown_channelserver);
  g_test_add ("/channelserver/relay-close", channelserver_fixture, NULL,
	      setup_channelserver, test_relay_close,
	      teardown_channelserver);
  g_test_add ("/channelserver/relay-message", channelserver_fixture, NULL,
	      setup_channelserver, test_relay_message, teardown_channelserver);
  
  return g_test_run ();
}

