/* test-dataserver-protocol.c: Test routines for test-dataserver-protocol.c.
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "data-protocol.h"
#include "dataserver.h"
#include "dataserver-protocol.h"
#include "event.h"
#include "event-meta.h"
#include "protocol.h"
#include "resolver.h"
#include "socket.h"

struct _GzochiMetadDataServer
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochiMetadDataServer, gzochi_metad_data_server, G_TYPE_OBJECT);

static void
gzochi_metad_data_server_class_init (GzochiMetadDataServerClass *klass)
{
}

static void
gzochi_metad_data_server_init (GzochiMetadDataServer *self)
{
}

static GList *activity_log = NULL;

static void
clear_activity_log ()
{
  g_list_free_full (activity_log, g_free);
  activity_log = NULL;
}

gzochid_data_reserve_oids_response *
gzochi_metad_dataserver_reserve_oids (GzochiMetadDataServer *dataserver,
				      guint node_id, const char *app)
{
  gzochid_data_oids_block block;
  gzochid_data_reserve_oids_response *response = NULL;

  block.block_start = 1;
  block.block_size = 100;

  response = gzochid_data_reserve_oids_response_new (app, &block);

  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_value (GzochiMetadDataServer *dataserver,
				       guint node_id, const char *app,
				       const char *store, GBytes *key,
				       gboolean for_write, GError **err)
{
  GBytes *data = g_bytes_new_static ("foo", 4);
  gzochid_data_response *response =
    gzochid_data_response_new (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_next_key (GzochiMetadDataServer *dataserver,
					  guint node_id, const char *app,
					  const char *store, GBytes *key,
					  GError **err)
{
  GBytes *data = g_bytes_new_static ("foo1", 5);
  gzochid_data_response *response = gzochid_data_response_new
    (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

void
gzochi_metad_dataserver_release_key (GzochiMetadDataServer *dataserver,
				     guint node_id, const char *app,
				     const char *store, GBytes *key)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("RELEASE KEY/%s/%s: %s", app, store,
      (char *) g_bytes_get_data (key, NULL)));
}

void
gzochi_metad_dataserver_release_range (GzochiMetadDataServer *dataserver,
				       guint node_id, const char *app,
				       const char *store, GBytes *from,
				       GBytes *to)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("RELEASE RANGE/%s/%s: %s -> %s", app, store,
      (char *) g_bytes_get_data (from, NULL),
      (char *) g_bytes_get_data (to, NULL)));
}

void
gzochi_metad_dataserver_release_all (GzochiMetadDataServer *dataserver,
				     guint node_id)
{
  activity_log = g_list_append (activity_log, g_strdup ("RELEASE ALL"));
}

void
gzochi_metad_dataserver_process_changeset (GzochiMetadDataServer *dataserver,
					   guint node_id,
					   gzochid_data_changeset *changeset,
					   GError **err)
{
  int i = 0;

  for (; i < changeset->changes->len; i++)
    {
      gzochid_data_change *change = &g_array_index
	(changeset->changes, gzochid_data_change, i);

      if (change->delete)
	activity_log = g_list_append
	  (activity_log, g_strdup_printf
	   ("PROCESS CHANGESET/%s/%s: DELETE KEY %s", changeset->app,
	    change->store, (char *) g_bytes_get_data (change->key, NULL)));
      else activity_log = g_list_append
	     (activity_log, g_strdup_printf
	      ("PROCESS CHANGESET/%s/%s: SET KEY %s -> %s", changeset->app,
	       change->store, (char *) g_bytes_get_data (change->key, NULL),
	       (char *) g_bytes_get_data (change->data, NULL)));
    }
}

struct _dataserver_protocol_fixture
{
  GzochidSocketServer *socket_server;
  GzochiMetadDataServer *dataserver;
  gzochi_metad_dataserver_client *client;

  GIOChannel *read_channel;
  GIOChannel *write_channel;
};

typedef struct _dataserver_protocol_fixture dataserver_protocol_fixture;

static void
dataserver_protocol_fixture_set_up (dataserver_protocol_fixture *fixture,
				    gconstpointer user_data)
{
  int pipefd[2] = { 0 };
  int pipe_fd = pipe (pipefd);
  GIOChannel *write_channel = g_io_channel_unix_new (pipefd[1]);
  gzochid_client_socket *client_socket = gzochid_client_socket_new
    (write_channel, "", gzochi_metad_dataserver_client_protocol, NULL);
  
  fixture->socket_server = gzochid_resolver_require
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->dataserver = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_DATA_SERVER, NULL);

  fixture->read_channel = g_io_channel_unix_new (pipefd[0]);
  fixture->write_channel = write_channel;
  fixture->client = gzochi_metad_dataserver_client_new
    (fixture->dataserver, client_socket, 0);
  
  g_io_channel_set_flags (fixture->read_channel, G_IO_FLAG_NONBLOCK, NULL);  
  g_io_channel_set_encoding (write_channel, NULL, NULL);
  g_io_channel_set_flags (write_channel, G_IO_FLAG_NONBLOCK, NULL);
  
  gzochid_client_socket_listen (fixture->socket_server, client_socket);
}

static void
dataserver_protocol_fixture_tear_down (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_object_unref (fixture->dataserver);
  
  g_io_channel_unref (fixture->read_channel);
  gzochi_metad_dataserver_client_protocol.free (fixture->client);
  
  clear_activity_log ();
}

static void
test_client_can_dispatch_true (dataserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http://localhost:8080/", 27);

  g_assert
    (gzochi_metad_dataserver_client_protocol.can_dispatch
     (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (dataserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http", 9);

  g_assert
    (! gzochi_metad_dataserver_client_protocol.can_dispatch
     (bytes, fixture->client));

  g_byte_array_unref (bytes);
}

static void
test_client_dispatch_one_reserve_oids (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  char buf[18];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x05\x20test", 8);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  g_io_channel_flush (fixture->write_channel, NULL);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->read_channel, buf, 18, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert(memcmp (buf,
		   "\x00\x0f\x50test\x00"
		   "\x00\x00\x00\x00\x00\x00\x00\x01\x00\x64", 18) == 0);
}

static void
test_client_dispatch_one_request_value (dataserver_protocol_fixture *fixture,
					gconstpointer user_data)
{
  char buf[20];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0f\x21test\x00oids\x00\x00\x00\x02""1", 18);
  
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  g_io_channel_flush (fixture->write_channel, NULL);

  g_assert_cmpint
    (g_io_channel_read_chars (fixture->read_channel, buf, 20, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x11\x51test\x00oids\x00\x01\x00\x04""foo\x00", 20) ==
     0);
}

static void
test_client_dispatch_one_request_next_key
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  char buf[22];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x11\x22test\x00names\x00\x00\x04""foo", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  g_io_channel_flush (fixture->write_channel, NULL);

  g_assert_cmpint
    (g_io_channel_read_chars (fixture->read_channel, buf, 22, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x13\x52test\x00names\x00\x01\x00\x05""foo1", 22) == 0);

}

static void
test_client_dispatch_one_release_key (dataserver_protocol_fixture *fixture,
				      gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0e\x40test\x00oids\x00\x00\x02""1", 17);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE KEY/test/oids: 1");
}

static void
test_client_dispatch_one_release_range
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x19\x42test\x00names\x00\x00\x05""foo1\x00\x00\x05""foo2\x00",
     28);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "RELEASE RANGE/test/names: foo1 -> foo2");
}

static void
test_client_dispatch_one_process_changeset
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x3f\x30test", 8);
  g_byte_array_append (bytes, "\x00\x04", 2);
  g_byte_array_append (bytes, "oids\x00\x00\x02""1\x00\x00\x00", 11);
  g_byte_array_append (bytes, "oids\x00\x00\x02""2\x00\x00\x04""foo", 15);
  g_byte_array_append (bytes, "names\x00\x00\x04""foo\x00\x00\x00", 14);
  g_byte_array_append (bytes, "names\x00\x00\x04""bar\x00\x00\x02""3", 16);

  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 4);
  g_assert_cmpstr (g_list_nth_data (activity_log, 0), ==,
		   "PROCESS CHANGESET/test/oids: DELETE KEY 1");
  g_assert_cmpstr (g_list_nth_data (activity_log, 1), ==,
		   "PROCESS CHANGESET/test/oids: SET KEY 2 -> foo");
  g_assert_cmpstr (g_list_nth_data (activity_log, 2), ==,
		   "PROCESS CHANGESET/test/names: DELETE KEY foo");
  g_assert_cmpstr (g_list_nth_data (activity_log, 3), ==,
		   "PROCESS CHANGESET/test/names: SET KEY bar -> 3");
}

static void
test_client_dispatch_multiple (dataserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""foo", 20);
  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""bar", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, fixture->client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 2);
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 0), ==, "RELEASE KEY/test/names: foo");
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 1), ==, "RELEASE KEY/test/names: bar");
}

static void
test_client_error (dataserver_protocol_fixture *fixture,
		   gconstpointer user_data)
{
  gzochi_metad_dataserver_client_protocol.error (fixture->client);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE ALL");
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

  g_test_add ("/client/can_dispatch/true", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_can_dispatch_true,
	      dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/can_dispatch/false", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/reserve-oids", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_reserve_oids,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/request-value", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_request_value,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/request-next-key", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_request_next_key,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/release-key", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_release_key,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/release-range", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_release_range,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/process-changeset", dataserver_protocol_fixture,
     NULL, dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_process_changeset,
     dataserver_protocol_fixture_tear_down);

  g_test_add ("/client/dispatch/multiple", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_dispatch_multiple,
	      dataserver_protocol_fixture_tear_down);

  g_test_add ("/client/error", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_error,
	      dataserver_protocol_fixture_tear_down);
  
  return g_test_run ();
}
