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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "config.h"
#include "dataclient.h"
#include "oids.h"
#include "socket.h"

struct _dataclient_fixture
{
  gzochid_reconnectable_socket *socket;
  GMainLoop *main_loop;
  GMainContext *main_context;
  GzochidDataClient *dataclient;

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

  dataclient_fixture *fixture;
  gboolean released;
};

typedef struct _dataclient_data_callback_data dataclient_data_callback_data;

struct _gzochid_reconnectable_socket
{
  dataclient_fixture *fixture;
};

gzochid_reconnectable_socket *
gzochid_reconnectable_socket_new ()
{
  return calloc (1, sizeof (gzochid_reconnectable_socket));
}

void
gzochid_reconnectable_socket_free (gzochid_reconnectable_socket *sock)
{
  free (sock);
}

void
gzochid_reconnectable_socket_write (gzochid_reconnectable_socket *sock,
				    unsigned char *buf, size_t len)
{
  g_byte_array_append (sock->fixture->bytes_received, buf, len);
}

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static void
oids_callback (gzochid_data_oids_block block, gpointer user_data)
{
  dataclient_oids_callback_data *callback_data = user_data;

  callback_data->block.block_start = block.block_start;
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
release_callback (gpointer user_data)
{
  dataclient_data_callback_data *callback_data = user_data;
  g_main_loop_quit (callback_data->fixture->main_loop);
  callback_data->released = TRUE;
}

static void
dataclient_fixture_setup (dataclient_fixture *fixture, gconstpointer user_data)
{
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);

  g_key_file_set_value (key_file, "metaserver", "lock.release.msec", "10");
  g_key_file_set_value (key_file, "metaserver", "rangelock.release.msec", "10");

  fixture->socket = gzochid_reconnectable_socket_new ();
  fixture->socket->fixture = fixture;

  fixture->main_context = g_main_context_new ();
  fixture->main_loop = g_main_loop_new (fixture->main_context, FALSE);
  
  fixture->dataclient = g_object_new
    (GZOCHID_TYPE_DATA_CLIENT,
     "configuration", configuration,
     "main-context", fixture->main_context,
     "reconnectable-socket", fixture->socket,
     NULL);

  fixture->bytes_received = g_byte_array_new ();

  g_object_unref (configuration);  
  g_key_file_unref (key_file);
}

static void
dataclient_fixture_teardown (dataclient_fixture *fixture,
			     gconstpointer user_data)
{
  gzochid_reconnectable_socket_free (fixture->socket);

  g_main_context_unref (fixture->main_context);
  g_main_loop_unref (fixture->main_loop);

  g_object_unref (fixture->dataclient);
  g_byte_array_unref (fixture->bytes_received);
}

static void
test_reserve_oids_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  gzochid_dataclient_reserve_oids
    (fixture->dataclient, "test", oids_callback, NULL);

  g_assert_cmpint (fixture->bytes_received->len, ==, 8);  
  g_assert
    (memcmp (fixture->bytes_received->data, "\x00\x05\x20test\x00", 8) == 0);
}

static void
test_received_oids_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  dataclient_oids_callback_data callback_data;
  gzochid_data_oids_block block;
  gzochid_data_reserve_oids_response *response = NULL;

  block.block_start = 1;
  block.block_size = 100;
  
  response = gzochid_data_reserve_oids_response_new ("test", &block);

  gzochid_dataclient_reserve_oids
    (fixture->dataclient, "test", oids_callback, &callback_data);
  gzochid_dataclient_received_oids (fixture->dataclient, response);

  g_assert_cmpint (callback_data.block.block_start, ==, 1);
  g_assert_cmpint (callback_data.block.block_size, ==, 100);
  
  gzochid_data_reserve_oids_response_free (response);
}

static void
test_request_value_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  
  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, TRUE,
     success_callback, NULL, failure_callback, NULL, release_callback, NULL);

  g_assert_cmpint (fixture->bytes_received->len, ==, 20);  
  g_assert
    (memcmp (fixture->bytes_received->data,
	     "\x00\x11\x21test\x00oids\x00\x01\x00\x04""foo\x00", 20) == 0);

  g_bytes_unref (key);
}

static gboolean
exit_loop (gpointer user_data)
{
  g_main_loop_quit (user_data);
  return FALSE;
}

static void
set_timeout (GMainLoop *main_loop, GMainContext *main_context)
{
  GSource *timeout = g_timeout_source_new (1000);

  g_source_set_callback (timeout, exit_loop, main_loop, NULL);
  g_source_attach (timeout, main_context);
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

  callback_data.fixture = fixture;
  
  gzochid_dataclient_request_value
    (fixture->dataclient, "test", "oids", key, FALSE,
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, &callback_data);

  gzochid_dataclient_received_value (fixture->dataclient, &response);
  set_timeout (fixture->main_loop, fixture->main_context);
  g_main_loop_run (fixture->main_loop);

  g_assert (g_bytes_equal (response.data, callback_data.value));
  g_assert (callback_data.released);
  
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
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, NULL);

  gzochid_dataclient_received_value (fixture->dataclient, &response);

  g_assert (timercmp (&response.timeout, &callback_data.timeout, ==));
  
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
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, NULL);

  gzochid_dataclient_received_value (fixture->dataclient, &response);

  g_assert (callback_data.value == NULL);  

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
     success_callback, NULL, failure_callback, NULL, release_callback, NULL);

  g_assert_cmpint (fixture->bytes_received->len, ==, 20);  
  g_assert (memcmp (fixture->bytes_received->data,
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
  
  callback_data.fixture = fixture;

  gzochid_dataclient_request_next_key
    (fixture->dataclient, "test", "names", key,
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, &callback_data);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);
  set_timeout (fixture->main_loop, fixture->main_context);
  g_main_loop_run (fixture->main_loop);

  g_assert (g_bytes_equal (response.data, callback_data.value));  
  g_assert (callback_data.released);
  
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
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, NULL);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);

  g_assert (timercmp (&response.timeout, &callback_data.timeout, ==));
  
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
     success_callback, &callback_data, failure_callback, &callback_data,
     release_callback, NULL);

  gzochid_dataclient_received_next_key (fixture->dataclient, &response);

  g_assert (callback_data.value == NULL);  

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
  
  expected_outbound_message = g_byte_array_free_to_bytes
    (expected_outbound_message_array);
  actual_outbound_message = g_bytes_new
    (fixture->bytes_received->data, fixture->bytes_received->len);
  
  g_assert (g_bytes_equal (expected_outbound_message, actual_outbound_message));

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

static void
test_release_key_simple (dataclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_dataclient_release_key (fixture->dataclient, "test", "oids", key);
  
  g_assert_cmpint (fixture->bytes_received->len, ==, 19);
  g_assert (memcmp (fixture->bytes_received->data,
		    "\x00\x10\x40test\x00oids\x00\x00\x04""foo\x00", 19) == 0);

  g_bytes_unref (key);
}

static void
test_release_key_range_simple (dataclient_fixture *fixture,
			       gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("bar", 4);
  GBytes *to = g_bytes_new_static ("foo", 4);

  gzochid_dataclient_release_key_range
    (fixture->dataclient, "test", "oids", from, to);

  g_assert_cmpint (fixture->bytes_received->len, ==, 25);
  g_assert (memcmp (fixture->bytes_received->data,
		    "\x00\x16\x42test\x00oids\x00\x00\x04""bar\x00\x00\x04"
		    "foo\x00", 25) == 0);

  g_bytes_unref (from);
  g_bytes_unref (to);
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
    ("/dataclient/reserve-oids/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_reserve_oids_simple,
     dataclient_fixture_teardown);
  g_test_add
    ("/dataclient/received-oids/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_received_oids_simple,
     dataclient_fixture_teardown);
  
  g_test_add
    ("/dataclient/request-value/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_request_value_simple,
     dataclient_fixture_teardown);
  
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
     dataclient_fixture_setup, test_request_next_key_simple,
     dataclient_fixture_teardown);
  
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
     dataclient_fixture_setup, test_submit_changeset_simple,
     dataclient_fixture_teardown);

  g_test_add
    ("/dataclient/release-key/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_release_key_simple,
     dataclient_fixture_teardown);

  g_test_add
    ("/dataclient/release-key-range/simple", dataclient_fixture, NULL,
     dataclient_fixture_setup, test_release_key_range_simple,
     dataclient_fixture_teardown);
  
  return g_test_run ();
}
