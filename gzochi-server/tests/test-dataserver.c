/* test-dataserver.c: Tests for dataserver.c in gzochi-metad.
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
#include <glib.h>
#include <glib-object.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "data-protocol.h"
#include "dataserver.h"
#include "gzochid-storage.h"
#include "resolver.h"
#include "storage-mem.h"

static gzochid_storage_context *test_context = NULL;

static gzochid_storage_store *oids = NULL;
static gzochid_storage_store *names = NULL;
static gzochid_storage_store *meta = NULL;

static gzochid_storage_context *(*mem_initialize) (char *);
static void (*mem_close_context) (gzochid_storage_context *);
static gzochid_storage_store *(*mem_open)
  (gzochid_storage_context *, char *, unsigned int);
static void (*mem_close_store) (gzochid_storage_store *);

static gzochid_storage_context *
test_storage_initialize (char *dir)
{
  if (test_context == NULL)
    test_context = mem_initialize (dir);
  return test_context;
}

static void
test_storage_close_context (gzochid_storage_context *context)
{
  assert (test_context == context);
  mem_close_context (context);
  test_context = NULL;  
}

static gzochid_storage_store *
test_storage_open (gzochid_storage_context *context, char *name,
		   unsigned int flags)
{
  if (strcmp (name, "oids") == 0)
    {
      if (oids == NULL)
	oids = mem_open (context, name, flags);
      return oids;
    }
  else if (strcmp (name, "names") == 0)
    {
      if (names == NULL)
	names = mem_open (context, name, flags);
      return names;
    }
  else if (strcmp (name, "meta") == 0)
    {
      if (meta == NULL)
	meta = mem_open (context, name, flags);
      return meta;
    }
  else assert (1 == 0);
  return NULL;
}

static void
test_storage_close_store (gzochid_storage_store *store)
{
  if (oids == store)
    oids = NULL;
  else if (names == store)
    names = NULL;
  else if (meta == store)
    meta = NULL;

  mem_close_store (store);
}

struct _dataserver_fixture
{
  GzochiMetadDataServer *server;
};

typedef struct _dataserver_fixture dataserver_fixture;

static void
put (gzochid_storage_store *store, char *key, size_t key_len, char *value,
     size_t value_len)
{
  gzochid_storage_engine_interface *iface =
    &gzochid_storage_engine_interface_mem;
  gzochid_storage_transaction *tx = iface->transaction_begin (test_context);

  iface->transaction_put (tx, store, key, key_len, value, value_len);
  assert (!tx->rollback);  
  iface->transaction_prepare (tx);
  assert (!tx->rollback);
  iface->transaction_commit (tx);
}

static void
setup_dataserver (dataserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  GzochidResolutionContext *resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);

  g_key_file_set_value (key_file, "data", "server.port", "0");
  
  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);

  fixture->server = gzochid_resolver_require_full
    (resolution_context, GZOCHI_METAD_TYPE_DATA_SERVER, &err);
  
  g_assert_no_error (err);
  
  g_key_file_unref (key_file);
  g_object_unref (resolution_context);
  g_object_unref (configuration);
  
  gzochi_metad_dataserver_start (fixture->server);
}

static void
teardown_dataserver (dataserver_fixture *fixture, gconstpointer user_data)
{
  g_object_unref (fixture->server);
}

static void
test_reserve_oids (dataserver_fixture *fixture, gconstpointer user_data)
{
  gzochid_data_reserve_oids_response *response =
    gzochi_metad_dataserver_reserve_oids (fixture->server, 1, "test");

  g_assert_cmpint (response->block.block_size, ==, 100);

  gzochid_data_reserve_oids_response_free (response);
}

static void
test_request_value (dataserver_fixture *fixture, gconstpointer user_data)
{
  gzochid_data_response *response = NULL;
  GBytes *key = g_bytes_new_static ("1", 2);
  GBytes *expected = g_bytes_new_static ("foo", 4);

  test_storage_initialize (NULL);
  test_storage_open (test_context, "oids", GZOCHID_STORAGE_CREATE);
  
  put (oids, "1", 2, "foo", 4);
  
  response = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "oids", key, FALSE, NULL);

  g_assert_true (response->success);
  g_assert_true (g_bytes_equal (response->data, expected));

  g_bytes_unref (expected);
  g_bytes_unref (key);
  
  gzochid_data_response_free (response);
}

static void
test_request_value_not_found (dataserver_fixture *fixture,
			      gconstpointer user_data)
{
  gzochid_data_response *response = NULL;
  GBytes *key = g_bytes_new_static ("1", 2);
  
  response = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "oids", key, FALSE, NULL);

  g_assert_true (response->success);
  g_assert_null (response->data);

  gzochid_data_response_free (response);
  g_bytes_unref (key);
}

static void
test_request_value_failure (dataserver_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("1", 2);
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;
  
  response1 = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "oids", key, TRUE, NULL);
  response2 = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "oids", key, FALSE, NULL);

  g_assert_false (response2->success);

  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);
  g_bytes_unref (key);
}

static void
test_request_next_key (dataserver_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("a", 2);
  GBytes *middle_key = g_bytes_new_static ("a1", 2);
  GBytes *next_key = g_bytes_new_static ("b", 2);
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);
  put (names, "b", 2, "2", 2);

  response1 = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", key, NULL);

  g_assert_true (response1->success);
  g_assert_nonnull (response1->data);
  g_assert_true (g_bytes_equal (next_key, response1->data));
  
  response2 = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "names", middle_key, TRUE, NULL);

  g_assert_false (response2->success);

  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (key);
  g_bytes_unref (middle_key);
  g_bytes_unref (next_key);
}

static void
test_request_next_key_null (dataserver_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *expected = g_bytes_new_static ("a", 2);
  gzochid_data_response *response = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);

  response = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", NULL, NULL);

  g_assert_true (response->success);
  g_assert_nonnull (response->data);
  g_assert_true (g_bytes_equal (expected, response->data));

  gzochid_data_response_free (response);
  g_bytes_unref (expected);
}

static void
test_request_next_key_not_found (dataserver_fixture *fixture,
				     gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("a", 2);
  gzochid_data_response *response = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);

  response = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", key, NULL);

  g_assert_true (response->success);
  g_assert_null (response->data);

  gzochid_data_response_free (response);
  g_bytes_unref (key);
}

static void
test_request_next_key_failure (dataserver_fixture *fixture,
				   gconstpointer user_data)
{
  GBytes *key1 = g_bytes_new_static ("a", 2);
  GBytes *key2 = g_bytes_new_static ("b", 2);
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);
  put (names, "b", 2, "2", 2);
  put (names, "c", 2, "3", 2);

  response1 = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", key1, NULL);
  response2 = gzochi_metad_dataserver_request_next_key
    (fixture->server, 2, "test", "names", key2, NULL);

  g_assert_false (response2->success);
  
  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
}

static void
test_release_key (dataserver_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("1", 2);
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  response1 = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "oids", key, TRUE, NULL);
  gzochi_metad_dataserver_release_key
    (fixture->server, 1, "test", "oids", key);
  response2 = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "oids", key, TRUE, NULL);

  g_assert_true (response2->success);

  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (key);
}

static void
test_release_key_range (dataserver_fixture *fixture,
			gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("test-binding", 13); 
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  response1 = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", NULL, NULL);
  gzochi_metad_dataserver_release_range
    (fixture->server, 1, "test", "names", NULL, NULL);
  response2 = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "names", key, TRUE, NULL);

  g_assert_true (response2->success);

  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (key);
}

static void
test_release_all (dataserver_fixture *fixture, gconstpointer user_data)
{
  GBytes *key1 = g_bytes_new_static ("test-binding", 13);
  GBytes *key2 = g_bytes_new_static ("1", 2);
  
  gzochid_data_response *response1a = gzochi_metad_dataserver_request_next_key
    (fixture->server, 1, "test", "names", NULL, NULL);
  gzochid_data_response *response2a = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "names", key1, TRUE, NULL);
  gzochid_data_response *response3a = NULL;

  gzochid_data_response *response1b = NULL;
  gzochid_data_response *response2b = NULL;
  gzochid_data_response *response3b = NULL;
  
  response3a = gzochi_metad_dataserver_request_value
    (fixture->server, 1, "test", "oids", key2, TRUE, NULL);

  gzochi_metad_dataserver_release_all (fixture->server, 1);

  response1b = gzochi_metad_dataserver_request_next_key
    (fixture->server, 2, "test", "names", NULL, NULL);

  g_assert_true (response1b->success);
  
  response2b = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "names", key1, TRUE, NULL);

  g_assert_true (response2b->success);

  response3b = gzochi_metad_dataserver_request_value
    (fixture->server, 2, "test", "oids", key2, TRUE, NULL);
  
  g_assert_true (response3b->success);

  gzochid_data_response_free (response1a);
  gzochid_data_response_free (response1b);

  gzochid_data_response_free (response2a);
  gzochid_data_response_free (response2b);

  gzochid_data_response_free (response3a);
  gzochid_data_response_free (response3b);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
}

static void
test_process_changeset (dataserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GArray *changes = g_array_sized_new
    (FALSE, FALSE, sizeof (gzochid_data_change), 6);
  char *data = NULL;
  
  gzochid_storage_engine_interface *iface =
    &gzochid_storage_engine_interface_mem;

  gzochid_data_change object_change1;
  gzochid_data_change object_change2;
  gzochid_data_change object_change3;

  gzochid_data_change binding_change1;
  gzochid_data_change binding_change2;
  gzochid_data_change binding_change3;

  gzochid_data_changeset *changeset = NULL;
  gzochid_storage_transaction *transaction = NULL;

  object_change1.store = strdup ("oids");
  object_change1.delete = FALSE;
  object_change1.key = g_bytes_new_static ("1", 2);
  object_change1.data = g_bytes_new_static ("bar", 4);

  object_change2.store = strdup ("oids");
  object_change2.delete = TRUE;
  object_change2.key = g_bytes_new_static ("2", 2);
  object_change2.data = NULL;

  object_change3.store = strdup ("oids");
  object_change3.delete = FALSE;
  object_change3.key = g_bytes_new_static ("3", 2);
  object_change3.data = g_bytes_new_static ("baz", 4);
  
  g_array_insert_val (changes, 0, object_change1);
  g_array_insert_val (changes, 1, object_change2);
  g_array_insert_val (changes, 2, object_change3);

  binding_change1.store = strdup ("names");
  binding_change1.delete = FALSE;
  binding_change1.key = g_bytes_new_static ("binding-1", 10);
  binding_change1.data = g_bytes_new_static ("2", 2);

  binding_change2.store = strdup ("names");
  binding_change2.delete = TRUE;
  binding_change2.key = g_bytes_new_static ("binding-2", 10);

  binding_change3.store = strdup ("names");
  binding_change3.delete = FALSE;
  binding_change3.key = g_bytes_new_static ("binding-3", 10);
  binding_change3.data = g_bytes_new_static ("3", 2);
  
  g_array_insert_val (changes, 3, binding_change1);
  g_array_insert_val (changes, 4, binding_change2);
  g_array_insert_val (changes, 5, binding_change3);

  changeset = gzochid_data_changeset_new ("test", changes);
  
  test_storage_initialize (NULL);
  test_storage_open (test_context, "oids", GZOCHID_STORAGE_CREATE);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);

  put (oids, "1", 2, "foo", 4);
  put (oids, "2", 2, "bar", 4);

  put (names, "binding-1", 10, "1", 2);
  put (names, "binding-2", 10, "2", 2);
  
  gzochi_metad_dataserver_process_changeset
    (fixture->server, 1, changeset, &err);

  g_assert_no_error (err);

  transaction = iface->transaction_begin (test_context);
  data = iface->transaction_get (transaction, oids, "1", 2, NULL);

  g_assert_nonnull (data);
  g_assert_cmpstr (data, ==, "bar");
  free (data);

  data = iface->transaction_get (transaction, oids, "2", 2, NULL);
  g_assert_null (data);

  data = iface->transaction_get (transaction, oids, "3", 2, NULL);

  g_assert_nonnull (data);
  g_assert_cmpstr (data, ==, "baz");
  free (data);

  data = iface->transaction_get (transaction, names, "binding-1", 10, NULL);

  g_assert_nonnull (data);
  g_assert_cmpstr (data, ==, "2");
  free (data);
  
  data = iface->transaction_get (transaction, names, "binding-2", 10, NULL);
  g_assert_null (data);
  
  data = iface->transaction_get (transaction, names, "binding-3", 10, NULL);

  g_assert_nonnull (data);  
  g_assert_cmpstr (data, ==, "3");
  free (data);
  
  iface->transaction_rollback (transaction);

  free (object_change1.store);
  g_bytes_unref (object_change1.key);
  g_bytes_unref (object_change1.data);
  
  free (object_change2.store);
  g_bytes_unref (object_change2.key);

  free (object_change3.store);
  g_bytes_unref (object_change3.key);
  g_bytes_unref (object_change3.data);

  free (binding_change1.store);
  g_bytes_unref (binding_change1.key);
  g_bytes_unref (binding_change1.data);

  free (binding_change2.store);
  g_bytes_unref (binding_change2.key);

  free (binding_change3.store);
  g_bytes_unref (binding_change3.key);
  g_bytes_unref (binding_change3.data);
  
  gzochid_data_changeset_free (changeset);
  g_array_unref (changes);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  mem_initialize = gzochid_storage_engine_interface_mem.initialize;
  mem_close_context = gzochid_storage_engine_interface_mem.close_context;
  mem_open = gzochid_storage_engine_interface_mem.open;
  mem_close_store = gzochid_storage_engine_interface_mem.close_store;

  gzochid_storage_engine_interface_mem.initialize = test_storage_initialize;
  gzochid_storage_engine_interface_mem.close_context =
    test_storage_close_context;
  gzochid_storage_engine_interface_mem.open = test_storage_open;
  gzochid_storage_engine_interface_mem.close_store = test_storage_close_store;
  
  g_test_add ("/dataserver/reserve-oids", dataserver_fixture, NULL,
	      setup_dataserver, test_reserve_oids, teardown_dataserver);
  g_test_add ("/dataserver/request-value", dataserver_fixture, NULL,
	      setup_dataserver, test_request_value, teardown_dataserver);
  g_test_add ("/dataserver/request-value/not-found", dataserver_fixture, NULL,
	      setup_dataserver, test_request_value_not_found,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-value/failure", dataserver_fixture, NULL,
	      setup_dataserver, test_request_value_failure,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-key", dataserver_fixture, NULL,
	      setup_dataserver, test_request_next_key, teardown_dataserver);
  g_test_add ("/dataserver/request-next-key/null", dataserver_fixture, NULL,
	      setup_dataserver, test_request_next_key_null,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-key/not-found", dataserver_fixture,
	      NULL, setup_dataserver, test_request_next_key_not_found,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-key/failure", dataserver_fixture,
	      NULL, setup_dataserver, test_request_next_key_failure,
	      teardown_dataserver);
  g_test_add ("/dataserver/release-key", dataserver_fixture, NULL,
	      setup_dataserver, test_release_key, teardown_dataserver);
  g_test_add ("/dataserver/release-key-range", dataserver_fixture, NULL,
	      setup_dataserver, test_release_key_range, teardown_dataserver);
  g_test_add ("/dataserver/release-all", dataserver_fixture, NULL,
	      setup_dataserver, test_release_all, teardown_dataserver);
  g_test_add ("/dataserver/process-changeset", dataserver_fixture, NULL,
	      setup_dataserver, test_process_changeset, teardown_dataserver);
  
  return g_test_run ();
}
