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
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "data-protocol.h"
#include "dataserver.h"
#include "dataserver-protocol.h"
#include "protocol.h"
#include "resolver.h"
#include "socket.h"

static GList *activity_log = NULL;

static void
clear_activity_log ()
{
  g_list_free_full (activity_log, g_free);
  activity_log = NULL;
}

gzochid_data_reserve_oids_response *
gzochi_metad_dataserver_reserve_oids (GzochiMetadDataServer *dataserver,
				      guint node_id, char *app)
{
  gzochid_data_oids_block block;
  gzochid_data_reserve_oids_response *response = NULL;
  
  mpz_init_set_str (block.block_start, "1", 16);
  block.block_size = 100;

  response = gzochid_data_reserve_oids_response_new (app, &block);

  mpz_clear (block.block_start);
  
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_value (GzochiMetadDataServer *dataserver,
				       guint node_id, char *app, char *store,
				       GBytes *key, gboolean for_write,
				       GError **err)
{
  GBytes *data = g_bytes_new_static ("foo", 4);
  gzochid_data_response *response =
    gzochid_data_response_new (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_next_key (GzochiMetadDataServer *dataserver,
					  guint node_id, char *app, char *store,
					  GBytes *key, GError **err)
{
  GBytes *data = g_bytes_new_static ("foo1", 5);
  gzochid_data_response *response = gzochid_data_response_new
    (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

void
gzochi_metad_dataserver_release_key (GzochiMetadDataServer *dataserver,
				     guint node_id, char *app, char *store,
				     GBytes *key)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("RELEASE KEY/%s/%s: %s", app, store,
      (char *) g_bytes_get_data (key, NULL)));
}

void
gzochi_metad_dataserver_release_range (GzochiMetadDataServer *dataserver,
				       guint node_id, char *app, char *store,
				       GBytes *from, GBytes *to)
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
  gzochid_client_socket *client_socket;
  gzochid_server_socket *server_socket;

  GIOChannel *socket_channel;
};

typedef struct _dataserver_protocol_fixture dataserver_protocol_fixture;

static gzochid_client_socket *
server_accept_wrapper (GIOChannel *channel, const char *desc, gpointer data)
{
  dataserver_protocol_fixture *fixture = data;
  gzochid_client_socket *ret = gzochi_metad_dataserver_server_protocol.accept
    (channel, desc, NULL);

  fixture->client_socket = ret;
  return ret;
}

static gzochid_server_protocol dataserver_server_wrapper_protocol =
  { server_accept_wrapper };

static void
dataserver_protocol_fixture_set_up (dataserver_protocol_fixture *fixture,
				    gconstpointer user_data)
{
  struct sockaddr addr;
  size_t addrlen = sizeof (struct sockaddr);
  int socket_fd = socket (AF_INET, SOCK_STREAM, 0);

  fixture->socket_server = gzochid_resolver_require
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  
  fixture->server_socket = gzochid_server_socket_new
    ("test", dataserver_server_wrapper_protocol, fixture);
  
  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname (fixture->server_socket, &addr, &addrlen);
  connect (socket_fd, &addr, addrlen);

  g_assert_true
    (g_main_context_iteration
     (fixture->socket_server->main_context, TRUE));

  g_assert_nonnull (fixture->client_socket);

  fixture->socket_channel = g_io_channel_unix_new (socket_fd);
  g_io_channel_set_flags (fixture->socket_channel, G_IO_FLAG_NONBLOCK, NULL);
}

static void
dataserver_protocol_fixture_tear_down (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_io_channel_unref (fixture->socket_channel);

  clear_activity_log ();
}

static void
test_server_accept (dataserver_protocol_fixture *fixture,
		    gconstpointer user_data)
{
  gzochid_client_protocol protocol =
    _gzochid_client_socket_get_protocol (fixture->client_socket);

  g_assert (memcmp (&protocol, &gzochi_metad_dataserver_client_protocol,
		    sizeof (gzochid_client_protocol)) == 0);
}

static void
test_client_can_dispatch_true (dataserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x01http://localhost:8080/", 27);

  g_assert_true
    (gzochi_metad_dataserver_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (dataserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x01http", 9);

  g_assert_false
    (gzochi_metad_dataserver_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
}

static void
test_client_dispatch_one_login (dataserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x01http://localhost:8080/", 27);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpstr
    (gzochi_metad_dataserver_client_get_admin_server_base_url (client), ==,
     "http://localhost:8080/");
}

static void
test_client_dispatch_one_reserve_oids (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[16];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x05\x20test", 8);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 12, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert (memcmp (buf, "\x00\x09\x50test\x00""1\x00\x00\x64", 12) == 0);
}

static void
test_client_dispatch_one_request_value (dataserver_protocol_fixture *fixture,
					gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[20];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0f\x21test\x00oids\x00\x00\x00\x02""1", 18);
  
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 20, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x11\x51test\x00oids\x00\x01\x00\x04""foo\x00", 20) ==
     0);
}

static void
test_client_dispatch_one_request_next_key
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[22];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x11\x22test\x00names\x00\x00\x04""foo", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 22, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x13\x52test\x00names\x00\x01\x00\x05""foo1", 22) == 0);

}

static void
test_client_dispatch_one_release_key (dataserver_protocol_fixture *fixture,
				      gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0e\x40test\x00oids\x00\x00\x02""1", 17);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE KEY/test/oids: 1");
}

static void
test_client_dispatch_one_release_range
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x19\x42test\x00names\x00\x00\x05""foo1\x00\x00\x05""foo2\x00",
     28);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "RELEASE RANGE/test/names: foo1 -> foo2");
}

static void
test_client_dispatch_one_process_changeset
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x3f\x30test", 8);
  g_byte_array_append (bytes, "\x00\x04", 2);
  g_byte_array_append (bytes, "oids\x00\x00\x02""1\x00\x00\x00", 11);
  g_byte_array_append (bytes, "oids\x00\x00\x02""2\x00\x00\x04""foo", 15);
  g_byte_array_append (bytes, "names\x00\x00\x04""foo\x00\x00\x00", 14);
  g_byte_array_append (bytes, "names\x00\x00\x04""bar\x00\x00\x02""3", 16);

  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
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
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""foo", 20);
  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""bar", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
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
  GSource *source = g_main_context_find_source_by_user_data
    (fixture->socket_server->main_context, fixture->client_socket);
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  gzochi_metad_dataserver_client_protocol.error (client);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE ALL");

  g_source_unref (source);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add ("/server/accept", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_server_accept,
	      dataserver_protocol_fixture_tear_down);
  g_test_add ("/client/can_dispatch/true", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_can_dispatch_true,
	      dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/can_dispatch/false", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/login", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_login,
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
