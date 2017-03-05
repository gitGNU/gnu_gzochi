/* test-channelserver-protocol.c: Test routines for channelserver-protocol.c.
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

#include "channelserver-protocol.h"
#include "channelserver.h"
#include "resolver.h"
#include "socket.h"

struct _GzochiMetadChannelServer
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochiMetadChannelServer, gzochi_metad_channel_server,
	       G_TYPE_OBJECT);

static void
gzochi_metad_channel_server_class_init (GzochiMetadChannelServerClass *klass)
{
}

static void
gzochi_metad_channel_server_init (GzochiMetadChannelServer *self)
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
gzochi_metad_channelserver_server_disconnected
(GzochiMetadChannelServer *channelserver, int node_id, GError **err)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf ("SERVER %d DISCONNECTED", node_id));
}

void
gzochi_metad_channelserver_relay_join (GzochiMetadChannelServer *channelserver,
				       int from_node_id, const char *app,
				       guint64 channel_oid, guint64 session_oid)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("%d: JOIN SESSION %s/%" G_GUINT64_FORMAT " TO CHANNEL %s/%"
      G_GUINT64_FORMAT, from_node_id, app, session_oid, app, channel_oid));
}

void
gzochi_metad_channelserver_relay_leave (GzochiMetadChannelServer *channelserver,
					int from_node_id, const char *app,
					guint64 channel_oid,
					guint64 session_oid)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("%d: REMOVE SESSION %s/%" G_GUINT64_FORMAT " FROM CHANNEL %s/%"
      G_GUINT64_FORMAT, from_node_id, app, session_oid, app, channel_oid));
}

void
gzochi_metad_channelserver_relay_message
(GzochiMetadChannelServer *channelserver, int from_node_id, const char *app,
 guint64 channel_oid, GBytes *msg)
{
  size_t data_len = 0;
  const char *data = g_bytes_get_data (msg, &data_len);

  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("%d: MESSAGE FOR CHANNEL %s/%" G_GUINT64_FORMAT ": %s", from_node_id, app,
      channel_oid, data));
}

void
gzochi_metad_channelserver_relay_close (GzochiMetadChannelServer *channelserver,
					int from_node_id, const char *app,
					guint64 channel_oid)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("%d: CLOSE CHANNEL %s/%" G_GUINT64_FORMAT, from_node_id, app,
      channel_oid));
}

struct _metaserver_wrapper_client
{
  gzochi_metad_channelserver_client *channelserver_client;
};

typedef struct _metaserver_wrapper_client metaserver_wrapper_client;

static gboolean
can_dispatch_wrapper (const GByteArray *bytes, gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  return gzochi_metad_channelserver_client_protocol.can_dispatch
    (bytes, wrapper_client->channelserver_client);
}

static unsigned int
dispatch_wrapper (const GByteArray *bytes, gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  return gzochi_metad_channelserver_client_protocol.dispatch
    (bytes, wrapper_client->channelserver_client);
}

static void
error_wrapper (gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  gzochi_metad_channelserver_client_protocol.error
    (wrapper_client->channelserver_client);
}

static void
free_wrapper (gpointer data)
{
  metaserver_wrapper_client *wrapper_client = data;

  gzochi_metad_channelserver_client_protocol.free
    (wrapper_client->channelserver_client);
  free (wrapper_client);
}

static gzochid_client_protocol wrapper_protocol =
  { can_dispatch_wrapper, dispatch_wrapper, error_wrapper, free_wrapper };

struct _channelserver_protocol_fixture
{
  GzochidSocketServer *socket_server;
  GzochiMetadChannelServer *channelserver;
  metaserver_wrapper_client *client;

  GIOChannel *read_channel;
  GIOChannel *write_channel;
};

typedef struct _channelserver_protocol_fixture channelserver_protocol_fixture;

static void
channelserver_protocol_fixture_set_up (channelserver_protocol_fixture *fixture,
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
  fixture->channelserver = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_CHANNEL_SERVER, NULL);

  fixture->read_channel = g_io_channel_unix_new (pipefd[0]);
  fixture->write_channel = write_channel;
  fixture->client = wrapper_client;

  wrapper_client->channelserver_client = gzochi_metad_channelserver_client_new
    (fixture->channelserver, client_socket, 1);
  
  g_io_channel_set_flags (fixture->read_channel, G_IO_FLAG_NONBLOCK, NULL);  
  g_io_channel_set_encoding (write_channel, NULL, NULL);
  g_io_channel_set_flags (write_channel, G_IO_FLAG_NONBLOCK, NULL);
  
  gzochid_client_socket_listen (fixture->socket_server, client_socket);
}

static void
channelserver_protocol_fixture_tear_down
(channelserver_protocol_fixture *fixture, gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_object_unref (fixture->channelserver);
  
  g_io_channel_unref (fixture->read_channel);
  
  clear_activity_log ();
}

static void
test_client_can_dispatch_true (channelserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0c\x76""foo\x00\x00\x00\x00\x00\x00\x00\x00\x01", 15);

  g_assert (wrapper_protocol.can_dispatch (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (channelserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0c\x76""foo", 7);

  g_assert (! wrapper_protocol.can_dispatch (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_dispatch_one_join (channelserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x15\x70test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);

  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==,
     "1: JOIN SESSION test/3 TO CHANNEL test/2");
}

static void
test_client_dispatch_one_leave (channelserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x15\x72test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);

  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==,
     "1: REMOVE SESSION test/3 FROM CHANNEL test/2");
}

static void
test_client_dispatch_one_relay_message (channelserver_protocol_fixture *fixture,
					gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x12\x74""foo\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04"
     "bar", 21);
  
  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);
  
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "1: MESSAGE FOR CHANNEL foo/2: bar");
}

static void
test_client_dispatch_one_close (channelserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0d\x76test\x00\x00\x00\x00\x00\x00\x00\x00\x02", 16);

  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "1: CLOSE CHANNEL test/2");
}

static void
test_client_dispatch_multiple (channelserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x15\x70test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);
  g_byte_array_append
    (bytes, "\x00\x15\x72test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
     "\x00\x00\x00\x00\x00\x00\x00\x03", 24);

  wrapper_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 2);
  g_assert_cmpstr
    ((char *) activity_log->data, ==,
     "1: JOIN SESSION test/3 TO CHANNEL test/2");
  g_assert_cmpstr
    ((char *) activity_log->next->data, ==,
     "1: REMOVE SESSION test/3 FROM CHANNEL test/2");
}

static void
test_client_error (channelserver_protocol_fixture *fixture,
		   gconstpointer user_data)
{
  wrapper_protocol.error (fixture->client);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "SERVER 1 DISCONNECTED");
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
    ("/client/can_dispatch/true", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_can_dispatch_true,
     channelserver_protocol_fixture_tear_down);
  
  g_test_add
    ("/client/can_dispatch/false", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/join", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_dispatch_one_join,
     channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/leave", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_dispatch_one_leave,
     channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/relay-message", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up,
     test_client_dispatch_one_relay_message,
     channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/close", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_dispatch_one_close,
     channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/multiple", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up,
     test_client_dispatch_multiple, channelserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/error", channelserver_protocol_fixture, NULL,
     channelserver_protocol_fixture_set_up, test_client_error,
     channelserver_protocol_fixture_tear_down);
  
  return g_test_run ();
}
