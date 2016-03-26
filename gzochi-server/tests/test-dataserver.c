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
test_request_object (dataserver_fixture *fixture, gconstpointer user_data)
{
  mpz_t test_oid;
  gzochid_data_object_response *response = NULL;
  GBytes *expected = g_bytes_new_static ("foo", 4);

  test_storage_initialize (NULL);
  test_storage_open (test_context, "oids", GZOCHID_STORAGE_CREATE);
  
  put (oids, "1", 2, "foo", 4);
  
  mpz_init (test_oid);
  mpz_set_ui (test_oid, 1);

  response = gzochi_metad_dataserver_request_object
    (fixture->server, 1, "test", test_oid, FALSE);

  g_assert_true (response->success);
  g_assert_true (g_bytes_equal (response->data, expected));

  g_bytes_unref (expected);
  gzochid_data_object_response_free (response);
  mpz_clear (test_oid);
}

static void
test_request_object_not_found (dataserver_fixture *fixture,
			       gconstpointer user_data)
{
  mpz_t test_oid;
  gzochid_data_object_response *response = NULL;
  
  mpz_init (test_oid);
  mpz_set_ui (test_oid, 1);
  
  response = gzochi_metad_dataserver_request_object
    (fixture->server, 1, "test", test_oid, FALSE);

  g_assert_true (response->success);
  g_assert_null (response->data);

  gzochid_data_object_response_free (response);
  mpz_clear (test_oid);
}

static void
test_request_object_failure (dataserver_fixture *fixture,
			     gconstpointer user_data)
{
  mpz_t test_oid;
  gzochid_data_object_response *response1 = NULL;
  gzochid_data_object_response *response2 = NULL;
  
  mpz_init (test_oid);
  mpz_set_ui (test_oid, 1);
  
  response1 = gzochi_metad_dataserver_request_object
    (fixture->server, 1, "test", test_oid, TRUE);
  response2 = gzochi_metad_dataserver_request_object
    (fixture->server, 2, "test", test_oid, FALSE);

  g_assert_false (response2->success);

  gzochid_data_object_response_free (response1);
  gzochid_data_object_response_free (response2);
  mpz_clear (test_oid);
}

static void
test_request_binding (dataserver_fixture *fixture, gconstpointer user_data)
{
  mpz_t expected;
  gzochid_data_binding_response *response = NULL;

  mpz_init (expected);
  mpz_set_ui (expected, 1);
  
  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "test-binding", 13, "1", 2);

  response = gzochi_metad_dataserver_request_binding
    (fixture->server, 1, "test", "test-binding", FALSE);

  g_assert_true (response->success);
  g_assert_true (response->present);
  g_assert (mpz_cmp (response->oid, expected) == 0);
  
  mpz_clear (expected);
  gzochid_data_binding_response_free (response);
}

static void
test_request_binding_not_found (dataserver_fixture *fixture,
				gconstpointer user_data)
{
  gzochid_data_binding_response *response =
    gzochi_metad_dataserver_request_binding
    (fixture->server, 1, "test", "test-binding", FALSE);

  g_assert_true (response->success);
  g_assert_false (response->present);

  gzochid_data_binding_response_free (response);  
}

static void
test_request_binding_failure (dataserver_fixture *fixture,
			      gconstpointer user_data)
{
  gzochid_data_binding_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;
  
  response1 = gzochi_metad_dataserver_request_binding
    (fixture->server, 1, "test", "test-binding", TRUE);
  response2 = gzochi_metad_dataserver_request_binding
    (fixture->server, 2, "test", "test-binding", FALSE);

  g_assert_false (response2->success);

  gzochid_data_binding_response_free (response1);
  gzochid_data_binding_response_free (response2);
}

static void
test_request_next_binding (dataserver_fixture *fixture, gconstpointer user_data)
{
  gzochid_data_binding_key_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);
  put (names, "b", 2, "2", 2);

  response1 = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", "a");

  g_assert_true (response1->success);
  g_assert_nonnull (response1->name);
  g_assert_cmpstr (response1->name, ==, "b");
  
  response2 = gzochi_metad_dataserver_request_binding
    (fixture->server, 2, "test", "a1", TRUE);

  g_assert_false (response2->success);

  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_response_free (response2);
}

static void
test_request_next_binding_null (dataserver_fixture *fixture,
				gconstpointer user_data)
{
  gzochid_data_binding_key_response *response = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);

  response = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", NULL);

  g_assert_true (response->success);
  g_assert_nonnull (response->name);
  g_assert_cmpstr (response->name, ==, "a");

  gzochid_data_binding_key_response_free (response);
}

static void
test_request_next_binding_not_found (dataserver_fixture *fixture,
				     gconstpointer user_data)
{
  gzochid_data_binding_key_response *response = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);

  response = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", "a");

  g_assert_true (response->success);
  g_assert_null (response->name);

  gzochid_data_binding_key_response_free (response);
}

static void
test_request_next_binding_failure (dataserver_fixture *fixture,
				   gconstpointer user_data)
{
  gzochid_data_binding_key_response *response1 = NULL;
  gzochid_data_binding_key_response *response2 = NULL;

  test_storage_initialize (NULL);
  test_storage_open (test_context, "names", GZOCHID_STORAGE_CREATE);
  
  put (names, "a", 2, "1", 2);
  put (names, "b", 2, "2", 2);
  put (names, "c", 2, "3", 2);

  response1 = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", "a");
  response2 = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 2, "test", "b");

  g_assert_false (response2->success);
  
  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_key_response_free (response2);  
}

static void
test_release_object (dataserver_fixture *fixture, gconstpointer user_data)
{
  mpz_t test_oid;
  gzochid_data_object_response *response1 = NULL;
  gzochid_data_object_response *response2 = NULL;

  mpz_init (test_oid);
  mpz_set_ui (test_oid, 1);

  response1 = gzochi_metad_dataserver_request_object
    (fixture->server, 1, "test", test_oid, TRUE);
  gzochi_metad_dataserver_release_object (fixture->server, 1, "test", test_oid);
  response2 = gzochi_metad_dataserver_request_object
    (fixture->server, 2, "test", test_oid, TRUE);

  g_assert_true (response2->success);

  mpz_clear (test_oid);
  
  gzochid_data_object_response_free (response1);
  gzochid_data_object_response_free (response2);
}

static void
test_release_binding (dataserver_fixture *fixture, gconstpointer user_data)
{
  gzochid_data_binding_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  response1 = gzochi_metad_dataserver_request_binding
    (fixture->server, 1, "test", "test-binding", TRUE);
  gzochi_metad_dataserver_release_binding
    (fixture->server, 1, "test", "test-binding");
  response2 = gzochi_metad_dataserver_request_binding
    (fixture->server, 2, "test", "test-binding", TRUE);

  g_assert_true (response2->success);

  gzochid_data_binding_response_free (response1);
  gzochid_data_binding_response_free (response2);
}

static void
test_release_binding_range (dataserver_fixture *fixture,
			    gconstpointer user_data)
{
  gzochid_data_binding_key_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  response1 = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", NULL);
  gzochi_metad_dataserver_release_binding_range
    (fixture->server, 1, "test", NULL, NULL);
  response2 = gzochi_metad_dataserver_request_binding
    (fixture->server, 2, "test", "test-binding", TRUE);

  g_assert_true (response2->success);

  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_response_free (response2);
}

static void
test_release_all (dataserver_fixture *fixture, gconstpointer user_data)
{
  mpz_t oid;
  gzochid_data_binding_key_response *response1a =
    gzochi_metad_dataserver_request_next_binding
    (fixture->server, 1, "test", NULL);
  gzochid_data_binding_response *response2a =
    gzochi_metad_dataserver_request_binding
    (fixture->server, 1, "test", "test-binding", TRUE);
  gzochid_data_object_response *response3a = NULL;

  gzochid_data_binding_key_response *response1b = NULL;
  gzochid_data_binding_response *response2b = NULL;
  gzochid_data_object_response *response3b = NULL;
  
  mpz_init (oid);
  mpz_set_ui (oid, 1);  

  response3a = gzochi_metad_dataserver_request_object
    (fixture->server, 1, "test", oid, TRUE);

  gzochi_metad_dataserver_release_all (fixture->server, 1, "test");

  response1b = gzochi_metad_dataserver_request_next_binding
    (fixture->server, 2, "test", NULL);

  g_assert_true (response1b->success);
  
  response2b = gzochi_metad_dataserver_request_binding
    (fixture->server, 2, "test", "test-binding", TRUE);

  g_assert_true (response2b->success);

  response3b = gzochi_metad_dataserver_request_object
    (fixture->server, 2, "test", oid, TRUE);
  
  g_assert_true (response3b->success);

  mpz_clear (oid);
  
  gzochid_data_binding_key_response_free (response1a);
  gzochid_data_binding_key_response_free (response1b);

  gzochid_data_binding_response_free (response2a);
  gzochid_data_binding_response_free (response2b);

  gzochid_data_object_response_free (response3a);
  gzochid_data_object_response_free (response3b);
}

static void
test_process_changeset (dataserver_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  GArray *object_changes = g_array_sized_new
    (FALSE, FALSE, sizeof (gzochid_data_object_change), 3);
  GArray *binding_changes = g_array_sized_new
    (FALSE, FALSE, sizeof (gzochid_data_binding_change), 3);
  char *data = NULL;
  
  gzochid_storage_engine_interface *iface =
    &gzochid_storage_engine_interface_mem;

  gzochid_data_object_change object_change1;
  gzochid_data_object_change object_change2;
  gzochid_data_object_change object_change3;

  gzochid_data_binding_change binding_change1;
  gzochid_data_binding_change binding_change2;
  gzochid_data_binding_change binding_change3;

  gzochid_data_changeset *changeset = NULL;
  gzochid_storage_transaction *transaction = NULL;

  object_change1.delete = FALSE;
  object_change1.data = g_bytes_new_static ("bar", 4);
  mpz_init (object_change1.oid);
  mpz_set_ui (object_change1.oid, 1);

  object_change2.delete = TRUE;
  object_change2.data = NULL;
  mpz_init (object_change2.oid);
  mpz_set_ui (object_change2.oid, 2);

  object_change3.delete = FALSE;
  object_change3.data = g_bytes_new_static ("baz", 4);
  mpz_init (object_change3.oid);
  mpz_set_ui (object_change3.oid, 3);
  
  g_array_insert_val (object_changes, 0, object_change1);
  g_array_insert_val (object_changes, 1, object_change2);
  g_array_insert_val (object_changes, 2, object_change3);

  binding_change1.delete = FALSE;
  binding_change1.name = strdup ("binding-1");
  mpz_init (binding_change1.oid);
  mpz_set_ui (binding_change1.oid, 2);

  binding_change2.delete = TRUE;
  binding_change2.name = strdup ("binding-2");

  binding_change3.delete = FALSE;
  binding_change3.name = strdup ("binding-3");
  mpz_init (binding_change3.oid);
  mpz_set_ui (binding_change3.oid, 3);
  
  g_array_insert_val (binding_changes, 0, binding_change1);
  g_array_insert_val (binding_changes, 1, binding_change2);
  g_array_insert_val (binding_changes, 2, binding_change3);

  changeset = gzochid_data_changeset_new
    ("test", object_changes, binding_changes);
  
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

  gzochid_data_changeset_free (changeset);
  g_array_unref (object_changes);
  g_array_unref (binding_changes);
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
  g_test_add ("/dataserver/request-object", dataserver_fixture, NULL,
	      setup_dataserver, test_request_object, teardown_dataserver);
  g_test_add ("/dataserver/request-object/not-found", dataserver_fixture, NULL,
	      setup_dataserver, test_request_object_not_found,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-object/failure", dataserver_fixture, NULL,
	      setup_dataserver, test_request_object_failure,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-binding", dataserver_fixture, NULL,
	      setup_dataserver, test_request_binding, teardown_dataserver);
  g_test_add ("/dataserver/request-binding/not-found", dataserver_fixture, NULL,
	      setup_dataserver, test_request_binding_not_found,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-binding/failure", dataserver_fixture, NULL,
	      setup_dataserver, test_request_binding_failure,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-binding", dataserver_fixture, NULL,
	      setup_dataserver, test_request_next_binding, teardown_dataserver);
  g_test_add ("/dataserver/request-next-binding/null", dataserver_fixture, NULL,
	      setup_dataserver, test_request_next_binding_null,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-binding/not-found", dataserver_fixture,
	      NULL, setup_dataserver, test_request_next_binding_not_found,
	      teardown_dataserver);
  g_test_add ("/dataserver/request-next-binding/failure", dataserver_fixture,
	      NULL, setup_dataserver, test_request_next_binding_failure,
	      teardown_dataserver);
  g_test_add ("/dataserver/release-object", dataserver_fixture, NULL,
	      setup_dataserver, test_release_object, teardown_dataserver);
  g_test_add ("/dataserver/release-binding", dataserver_fixture, NULL,
	      setup_dataserver, test_release_binding, teardown_dataserver);
  g_test_add ("/dataserver/release-binding-range", dataserver_fixture, NULL,
	      setup_dataserver, test_release_binding_range,
	      teardown_dataserver);
  g_test_add ("/dataserver/release-all", dataserver_fixture, NULL,
	      setup_dataserver, test_release_all, teardown_dataserver);
  g_test_add ("/dataserver/process-changeset", dataserver_fixture, NULL,
	      setup_dataserver, test_process_changeset, teardown_dataserver);
  
  return g_test_run ();
}
