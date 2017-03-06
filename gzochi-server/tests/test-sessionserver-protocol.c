/* test-sessionserver-protocol.c: Test routines for sessionserver-protocol.c.
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
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>

#include "resolver.h"
#include "sessionserver-protocol.h"
#include "sessionserver.h"
#include "socket.h"

struct _GzochiMetadSessionServer
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochiMetadSessionServer, gzochi_metad_session_server,
	       G_TYPE_OBJECT);

static void
gzochi_metad_session_server_class_init (GzochiMetadSessionServerClass *klass)
{
}

static void
gzochi_metad_session_server_init (GzochiMetadSessionServer *self)
{
}

static GList *activity_log = NULL;

static void
clear_activity_log ()
{
  g_list_free_full (activity_log, g_free);
  activity_log = NULL;
}

void
gzochi_metad_sessionserver_server_disconnected
(GzochiMetadSessionServer *sessionserver, int node_id, GError **err)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf ("SERVER %d DISCONNECTED", node_id));
}

void
gzochi_metad_sessionserver_session_connected
(GzochiMetadSessionServer *sessionserver, int node_id, const char *app,
 guint64 session_id, GError **err)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("SESSION %s/%" G_GUINT64_FORMAT " CONNECTED TO SERVER %d", app,
      session_id, node_id));
}

void
gzochi_metad_sessionserver_session_disconnected
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
 GError **err)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("SESSION %s/%" G_GUINT64_FORMAT " DISCONNECTED", app, session_id));
}

void
gzochi_metad_sessionserver_relay_disconnect
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
 GError **err)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("DISCONNECT SESSION %s/%" G_GUINT64_FORMAT, app, session_id));
}

void
gzochi_metad_sessionserver_relay_message
(GzochiMetadSessionServer *sessionserver, const char *app, guint64 session_id,
 GBytes *msg, GError **err)
{
  size_t data_len = 0;
  const char *data = g_bytes_get_data (msg, &data_len);

  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("MESSAGE FOR SESSION %s/%" G_GUINT64_FORMAT ": %s", app, session_id,
      data));
}

struct _metaserver_wrapper_client
{
  gzochi_metad_sessionserver_client *sessionserver_client;
};

typedef struct _metaserver_wrapper_client metaserver_wrapper_client;

static gboolean
can_dispatch_wrapper (const GByteArray *bytes, gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  return gzochi_metad_sessionserver_client_protocol.can_dispatch
    (bytes, wrapper_client->sessionserver_client);
}

static unsigned int
dispatch_wrapper (const GByteArray *bytes, gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  return gzochi_metad_sessionserver_client_protocol.dispatch
    (bytes, wrapper_client->sessionserver_client);
}

static void
error_wrapper (gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  gzochi_metad_sessionserver_client_protocol.error
    (wrapper_client->sessionserver_client);
}

static void
free_wrapper (gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  gzochi_metad_sessionserver_client_protocol.free
    (wrapper_client->sessionserver_client);
  free (wrapper_client);
}

static gzochid_client_protocol wrapper_protocol =
  { can_dispatch_wrapper, dispatch_wrapper, error_wrapper, free_wrapper };

struct _sessionserver_protocol_fixture
{
  GzochidSocketServer *socket_server;
  GzochiMetadSessionServer *sessionserver;
  metaserver_wrapper_client *client;

  GIOChannel *read_channel;
  GIOChannel *write_channel;
};

typedef struct _sessionserver_protocol_fixture sessionserver_protocol_fixture;

static void
sessionserver_protocol_fixture_set_up (sessionserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  int pipefd[2] = { 0 };
  int pipe_fd = pipe (pipefd);
  GIOChannel *write_channel = g_io_channel_unix_new (pipefd[1]);
  metaserver_wrapper_client *wrapper_client =
    malloc (sizeof (metaserver_wrapper_client));
  gzochid_client_socket *client_socket = gzochid_client_socket_new
    (write_channel, "", wrapper_protocol, wrapper_client);
  
  fixture->socket_server = gzochid_resolver_require
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->sessionserver = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_SESSION_SERVER, NULL);

  fixture->read_channel = g_io_channel_unix_new (pipefd[0]);
  fixture->write_channel = write_channel;
  fixture->client = wrapper_client;

  wrapper_client->sessionserver_client = gzochi_metad_sessionserver_client_new
    (fixture->sessionserver, client_socket, 0);
  
  g_io_channel_set_flags (fixture->read_channel, G_IO_FLAG_NONBLOCK, NULL);  
  g_io_channel_set_encoding (write_channel, NULL, NULL);
  g_io_channel_set_flags (write_channel, G_IO_FLAG_NONBLOCK, NULL);
  
  gzochid_client_socket_listen (fixture->socket_server, client_socket);
  gzochid_client_socket_unref (client_socket);
}

static void
sessionserver_protocol_fixture_tear_down
(sessionserver_protocol_fixture *fixture, gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_object_unref (fixture->sessionserver);
  
  g_io_channel_unref (fixture->read_channel);
  
  clear_activity_log ();
}

static void
test_client_can_dispatch_true (sessionserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x60""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);

  g_assert (wrapper_protocol.can_dispatch (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (sessionserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0c\x60""foo", 7);

  g_assert (! wrapper_protocol.can_dispatch (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_dispatch_one_session_connected
(sessionserver_protocol_fixture *fixture, gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x60""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);
  
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);
  
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "SESSION foo/1 CONNECTED TO SERVER 0");
}

static void
test_client_dispatch_one_session_disconnected
(sessionserver_protocol_fixture *fixture, gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x61""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);
  
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);
  
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "SESSION foo/1 DISCONNECTED");
}

static void
test_client_dispatch_one_relay_disconnect
(sessionserver_protocol_fixture *fixture, gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x62""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);
  
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);
  
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "DISCONNECT SESSION foo/1");
}

static void
test_client_dispatch_one_relay_message (sessionserver_protocol_fixture *fixture,
					gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x12\x64""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04"
     "bar", 21);
  
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);
  
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "MESSAGE FOR SESSION foo/1: bar");
}

static void
test_client_dispatch_multiple (sessionserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x62""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);
  g_byte_array_append
    (bytes, "\x00\x0c\x61""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 2);
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 0), ==, "DISCONNECT SESSION foo/1");
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 1), ==, "SESSION foo/1 DISCONNECTED");
}

static void
test_client_error (sessionserver_protocol_fixture *fixture,
		   gconstpointer user_data)
{
  wrapper_protocol.error (fixture->client);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "SERVER 0 DISCONNECTED");
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
    ("/client/can_dispatch/true", sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up, test_client_can_dispatch_true,
     sessionserver_protocol_fixture_tear_down);
  
  g_test_add
    ("/client/can_dispatch/false", sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/session-connected", sessionserver_protocol_fixture,
     NULL, sessionserver_protocol_fixture_set_up,
     test_client_dispatch_one_session_connected,
     sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/session-disconnected",
     sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up,
     test_client_dispatch_one_session_disconnected,
     sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/relay-disconnect", sessionserver_protocol_fixture,
     NULL, sessionserver_protocol_fixture_set_up,
     test_client_dispatch_one_relay_disconnect,
     sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/relay-message", sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up,
     test_client_dispatch_one_relay_message,
     sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/multiple", sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up,
     test_client_dispatch_multiple, sessionserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/error", sessionserver_protocol_fixture, NULL,
     sessionserver_protocol_fixture_set_up, test_client_error,
     sessionserver_protocol_fixture_tear_down);
  
  return g_test_run ();
}
