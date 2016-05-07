/* test-dataclient.c: Tests for dataclient.c in gzochid.
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

#include <arpa/inet.h>
#include <glib.h>
#include <netinet/in.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "config.h"
#include "dataclient.h"
#include "oids.h"
#include "resolver.h"
#include "socket.h"

struct _dataclient_fixture
{
  GzochidResolutionContext *resolution_context;
  GzochidDataClient *dataclient;

  GzochidSocketServer *socket_server;
  gzochid_server_socket *server_socket;

  GByteArray *bytes_received;
};

typedef struct _dataclient_fixture dataclient_fixture;

struct _dataclient_oids_callback_data
{
  gzochid_data_oids_block block;
};

typedef struct _dataclient_oids_callback_data dataclient_oids_callback_data;

struct _dataclient_data_callback_data
{
  GBytes *value;
  struct timeval timeout;
};

typedef struct _dataclient_data_callback_data dataclient_data_callback_data;

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static gboolean
exit_loop (gpointer user_data)
{
  dataclient_fixture *fixture = user_data;
  g_main_loop_quit (fixture->socket_server->main_loop);
  return FALSE;
}

static gboolean
can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return TRUE;
}

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  dataclient_fixture *fixture = user_data;

  g_byte_array_append (fixture->bytes_received, buffer->data, buffer->len);
  g_main_loop_quit (fixture->socket_server->main_loop);
  return buffer->len;
}

static void
client_error (gpointer user_data)
{
}

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol test_client_protocol =
  { can_dispatch, client_dispatch, client_error, client_free };

static gzochid_client_socket *
server_accept (GIOChannel *channel, const char *desc, gpointer data)
{
  return gzochid_client_socket_new (channel, desc, test_client_protocol, data);
}

gzochid_server_protocol test_server_protocol = { server_accept };

static void
oids_callback (gzochid_data_oids_block block, gpointer user_data)
{
  dataclient_oids_callback_data *callback_data = user_data;

  mpz_init_set (callback_data->block.block_start, block.block_start);
  callback_data->block.block_size = block.block_size;
}

static void
success_callback (GBytes *value, gpointer user_data)
{
  dataclient_data_callback_data *callback_data = user_data;
  callback_data->value = g_bytes_ref (value);
}

static void
failure_callback (struct timeval wait_time, gpointer user_data)
{
  dataclient_data_callback_data *callback_data = user_data;
  callback_data->timeout = wait_time;
}

static void
dataclient_fixture_setup_inner (dataclient_fixture *fixture, GKeyFile *key_file)
{
  GError *err = NULL;
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);

  gzochid_resolver_provide
    (fixture->resolution_context, G_OBJECT (configuration), NULL);

  g_object_unref (configuration);
  
  fixture->dataclient = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_DATA_CLIENT, &err);

  g_assert_no_error (err);

  fixture->bytes_received = g_byte_array_new ();
}

static void
dataclient_fixture_setup (dataclient_fixture *fixture, gconstpointer user_data)
{
  GKeyFile *key_file = g_key_file_new ();
  fixture->resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);

  dataclient_fixture_setup_inner (fixture, key_file);
  g_key_file_unref (key_file);
}

static void
dataclient_connected_fixture_setup (dataclient_fixture *fixture,
				    gconstpointer user_data)
{
  struct sockaddr_in addr;
  size_t addrlen = sizeof (struct sockaddr_in);
  char *server_address = NULL;
  GKeyFile *key_file = g_key_file_new ();

  fixture->resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);

  fixture->socket_server = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->server_socket = gzochid_server_socket_new
    ("test", test_server_protocol, fixture);

  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname
    (fixture->server_socket, (struct sockaddr *) &addr, &addrlen);

  server_address = g_strdup_printf 
    ("%d.%d.%d.%d:%d",
     addr.sin_addr.s_addr & 0xff,
     (addr.sin_addr.s_addr & 0xff00) >> 8,
     (addr.sin_addr.s_addr & 0xff0000) >> 16,
     (addr.sin_addr.s_addr & 0xff000000) >> 24, htons (addr.sin_port));

  g_key_file_set_value
    (key_file, "metaserver", "server.address", server_address);

  g_free (server_address);
  
  dataclient_fixture_setup_inner (fixture, key_file);
  g_key_file_unref (key_file);
  
  gzochid_dataclient_start (fixture->dataclient, NULL);
}

static void
dataclient_fixture_teardown (dataclient_fixture *fixture,
			     gconstpointer user_data)
{
  g_object_unref (fixture->dataclient);
  g_object_unref (fixture->resolution_context);  
  g_byte_array_unref (fixture->bytes_received);
}

static void
dataclient_connected_fixture_teardown (dataclient_fixture *fixture,
			     gconstpointer user_data)
{
  gzochid_dataclient_stop (fixture->dataclient);

  dataclient_fixture_teardown (fixture, user_data);
}

static void
test_reserve_oids_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  gzochid_dataclient_reserve_oids
    (fixture->dataclient, "test", oids_callback, NULL);

  g_timeout_add (2000, exit_loop, fixture);
  g_main_loop_run (fixture->socket_server->main_loop);
  
  g_assert_cmpint (fixture->bytes_received->len, ==, 8);  
  g_assert_true
    (memcmp (fixture->bytes_received->data, "\x00\x05\x20test\x00", 8) == 0);
}

static void
test_received_oids_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  dataclient_oids_callback_data callback_data;
  gzochid_data_oids_block block;
  gzochid_data_reserve_oids_response *response = NULL;

  mpz_init_set_ui (block.block_start, 1);
  block.block_size = 100;
  
  response = gzochid_data_reserve_oids_response_new ("test", &block);

  gzochid_dataclient_reserve_oids
    (fixture->dataclient, "test", oids_callback, &callback_data);
  gzochid_dataclient_received_oids (fixture->dataclient, response);

  g_assert_true (mpz_cmp_ui (callback_data.block.block_start, 1) == 0);
  g_assert_cmpint (callback_data.block.block_size, ==, 100);
  
  gzochid_data_reserve_oids_response_free (response);

  mpz_clear (block.block_start);
  mpz_clear (callback_data.block.block_start);
}

static void
test_request_value_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  
  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, TRUE,
     success_callback, NULL, failure_callback, NULL);

  g_timeout_add (2000, exit_loop, fixture);
  g_main_loop_run (fixture->socket_server->main_loop);
  
  g_assert_cmpint (fixture->bytes_received->len, ==, 20);  
  g_assert_true
    (memcmp (fixture->bytes_received->data,
	     "\x00\x11\x21test\x00oids\x00\x01\x00\x04""foo\x00", 20) == 0);

  g_bytes_unref (key);
}

static void
test_received_value_success (dataclient_fixture *fixture,
			     gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  response.app = "test";
  response.store = "oids";
  response.success = TRUE;
  response.data = g_bytes_new_static ("bar", 4);
  
  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, FALSE,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_value (fixture->dataclient, &response);

  g_assert_true (g_bytes_equal (response.data, callback_data.value));
  
  g_bytes_unref (response.data);
  g_bytes_unref (callback_data.value);
  g_bytes_unref (key);
}

static void
test_received_value_failure (dataclient_fixture *fixture,
			     gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  response.app = "test";
  response.store = "oids";
  response.success = FALSE;
  
  response.timeout.tv_sec = 123;
  response.timeout.tv_usec = 456;

  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, FALSE,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_value (fixture->dataclient, &response);

  g_assert_true (timercmp (&response.timeout, &callback_data.timeout, ==));
  
  g_bytes_unref (key);
}

static void
test_received_value_unexpected (dataclient_fixture *fixture,
				gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  response.app = "test";
  response.store = "oids";
  response.success = TRUE;
  response.data = g_bytes_new_static ("bar", 4);

  gzochid_dataclient_request_next_key
    (fixture->dataclient, "test", "names", key,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_value (fixture->dataclient, &response);

  g_assert_null (callback_data.value);  

  g_bytes_unref (key);
  g_bytes_unref (response.data);
}

static void
test_request_next_key_simple (dataclient_fixture *fixture,
			      gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  
  gzochid_dataclient_request_next_key
    (fixture->dataclient, "test", "names", key,
     success_callback, NULL, failure_callback, NULL);

  g_timeout_add (2000, exit_loop, fixture);
  g_main_loop_run (fixture->socket_server->main_loop);
  
  g_assert_cmpint (fixture->bytes_received->len, ==, 20);  
  g_assert_true
    (memcmp (fixture->bytes_received->data,
	     "\x00\x11\x22test\x00names\x00\x00\x04""foo\x00", 20) == 0);

  g_bytes_unref (key);
}

static void
test_received_next_key_success (dataclient_fixture *fixture,
				gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  response.app = "test";
  response.store = "names";
  response.success = TRUE;
  response.data = g_bytes_new_static ("bar", 4);
  
  gzochid_dataclient_request_next_key
    (fixture->dataclient, "test", "names", key,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);

  g_assert_true (g_bytes_equal (response.data, callback_data.value));
  
  g_bytes_unref (response.data);
  g_bytes_unref (callback_data.value);
  g_bytes_unref (key);
}

static void
test_received_next_key_failure (dataclient_fixture *fixture,
				gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  response.app = "test";
  response.store = "names";
  response.success = FALSE;
  
  response.timeout.tv_sec = 123;
  response.timeout.tv_usec = 456;

  gzochid_dataclient_request_next_key
    (fixture->dataclient, "test", "names", key,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);

  g_assert_true (timercmp (&response.timeout, &callback_data.timeout, ==));
  
  g_bytes_unref (key);
}

static void
test_received_next_key_unexpected (dataclient_fixture *fixture,
				   gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  dataclient_data_callback_data callback_data = { 0 };
  gzochid_data_response response;

  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  response.app = "test";
  response.store = "names";
  response.success = TRUE;
  response.data = g_bytes_new_static ("bar", 4);

  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, TRUE,
     success_callback, &callback_data, failure_callback, &callback_data);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);

  g_assert_null (callback_data.value);  

  g_bytes_unref (key);
  g_bytes_unref (response.data);
}

static void
test_submit_changeset_simple (dataclient_fixture *fixture,
			      gconstpointer user_data)
{
  gzochid_data_change obj_change1;
  gzochid_data_change obj_change2;
  gzochid_data_change binding_change1;
  gzochid_data_change binding_change2;

  GArray *changes = g_array_new (FALSE, FALSE, sizeof (gzochid_data_change));
  GByteArray *expected_outbound_message_array = g_byte_array_new ();
  GBytes *expected_outbound_message = NULL, *actual_outbound_message = NULL;
  gzochid_data_changeset *changeset = NULL;
  
  obj_change1.store = strdup ("oids");
  obj_change1.key = g_bytes_new_static ("1", 2);
  obj_change1.delete = FALSE;
  obj_change1.data = g_bytes_new_static ("foo", 4);

  obj_change2.store = strdup ("oids");
  obj_change2.key = g_bytes_new_static ("2", 2);
  obj_change2.delete = TRUE;
  obj_change2.data = NULL;

  binding_change1.store = strdup ("names");
  binding_change1.key = g_bytes_new_static ("foo", 4);
  binding_change1.delete = FALSE;
  binding_change1.data = g_bytes_new_static ("1", 2);

  binding_change2.store = strdup ("names");
  binding_change2.key = g_bytes_new_static ("bar", 4);
  binding_change2.delete = TRUE;
  binding_change2.data = NULL;

  g_array_append_val (changes, obj_change1);
  g_array_append_val (changes, obj_change2);
  g_array_append_val (changes, binding_change1);
  g_array_append_val (changes, binding_change2);

  changeset = gzochid_data_changeset_new ("test", changes);
  g_byte_array_append (expected_outbound_message_array, "\x00\x3f\x30", 3);
  gzochid_data_protocol_changeset_write
    (changeset, expected_outbound_message_array);
  gzochid_data_changeset_free (changeset);
  
  gzochid_dataclient_submit_changeset (fixture->dataclient, "test", changes);
  
  g_timeout_add (2000, exit_loop, fixture);
  g_main_loop_run (fixture->socket_server->main_loop);

  expected_outbound_message = g_byte_array_free_to_bytes
    (expected_outbound_message_array);
  actual_outbound_message = g_bytes_new
    (fixture->bytes_received->data, fixture->bytes_received->len);
  
  g_assert_true
    (g_bytes_equal (expected_outbound_message, actual_outbound_message));

  g_bytes_unref (expected_outbound_message);
  g_bytes_unref (actual_outbound_message);

  g_array_unref (changes);
  
  free (obj_change1.store);
  g_bytes_unref (obj_change1.key);
  g_bytes_unref (obj_change1.data);
  free (obj_change2.store);
  g_bytes_unref (obj_change2.key);
  free (binding_change1.store);
  g_bytes_unref (binding_change1.key);
  g_bytes_unref (binding_change1.data);
  free (binding_change2.store);
  g_bytes_unref (binding_change2.key);  
}

int
main (int argc, char *argv[])
{  
  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/dataclient/reserve-oids/simple", dataclient_fixture, NULL,
     dataclient_connected_fixture_setup, test_reserve_oids_simple,
     dataclient_connected_fixture_teardown);
  g_test_add
    ("/dataclient/received-oids/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_oids_simple,
     dataclient_fixture_teardown);
  
  g_test_add
    ("/dataclient/request-value/simple", dataclient_fixture, NULL,
     dataclient_connected_fixture_setup, test_request_value_simple,
     dataclient_connected_fixture_teardown);
  
  g_test_add
    ("/dataclient/received-value/success", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_value_success,
     dataclient_fixture_teardown);
  g_test_add
    ("/dataclient/received-value/failure", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_value_failure,
     dataclient_fixture_teardown);
  g_test_add
    ("/dataclient/received-value/unexpected", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_value_unexpected,
     dataclient_fixture_teardown);
  
  g_test_add
    ("/dataclient/request-next-key/simple", dataclient_fixture, NULL,
     dataclient_connected_fixture_setup, test_request_next_key_simple,
     dataclient_connected_fixture_teardown);
  
  g_test_add
    ("/dataclient/received-next-key/success", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_next_key_success,
     dataclient_fixture_teardown);
  g_test_add
    ("/dataclient/received-next-key/failure", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_next_key_failure,
     dataclient_fixture_teardown);
  g_test_add
    ("/dataclient/received-next-key/unexpected", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_next_key_unexpected,
     dataclient_fixture_teardown);

  g_test_add
    ("/dataclient/submit-changeset/simple", dataclient_fixture, NULL,
     dataclient_connected_fixture_setup, test_submit_changeset_simple,
     dataclient_connected_fixture_teardown);

  return g_test_run ();
}
