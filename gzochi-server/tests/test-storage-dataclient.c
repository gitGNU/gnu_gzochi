/* test-storage-dataclient.c: Tests for storage-dataclient.c in gzochid.
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

#include "dataclient.h"
#include "storage-dataclient.h"

struct _dataclient_storage_response
{  
  gboolean success;

  union
  {
    GBytes *data;
    struct timeval timeout;
  };
};

typedef struct _dataclient_storage_response dataclient_storage_response;

struct _dataclient_storage_response_closure
{
  GzochidDataClient *client;
  dataclient_storage_response *response;
  
  gzochid_dataclient_success_callback success_callback;
  gpointer success_data;

  gzochid_dataclient_failure_callback failure_callback;
  gpointer failure_data;
};

typedef struct _dataclient_storage_response_closure
dataclient_storage_response_closure;

struct _GzochidDataClient
{
  GObject parent_instance;

  GList *responses;
  GList *requested_keys;
  GList *requested_values;
  GList *changesets;
  GList *deferred_response_closures;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  self->responses = NULL;
  self->requested_keys = NULL;
  self->requested_values = NULL;
  self->changesets = NULL;
  self->deferred_response_closures = NULL;
}

static gpointer
process_response_async (gpointer data)
{
  dataclient_storage_response_closure *closure = data;

  if (closure->response->success)
    closure->success_callback (closure->response->data, closure->success_data);
  else closure->failure_callback
	 (closure->response->timeout, closure->failure_data);

  free (closure);
  return NULL;
}

static void
execute_deferred_closure (gpointer data)
{
  dataclient_storage_response_closure *closure = data;
  dataclient_storage_response *response = closure->response;
  
  process_response_async (closure);  
  free (response);
}

static dataclient_storage_response_closure *
create_closure
(GzochidDataClient *client, dataclient_storage_response *response,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data)
{
  dataclient_storage_response_closure *closure =
    malloc (sizeof (dataclient_storage_response_closure));

  closure->client = client;
  closure->response = response;

  closure->success_callback = success_callback;
  closure->success_data = success_data;
  
  closure->failure_callback = failure_callback;
  closure->failure_data = failure_data;

  return closure;
}

static dataclient_storage_response *
create_success_response (GBytes *data)
{
  dataclient_storage_response *response =
    malloc (sizeof (dataclient_storage_response));

  response->success = TRUE;
  response->data = data == NULL ? NULL : g_bytes_ref (data);
  
  return response;
}

static dataclient_storage_response *
create_failure_response (struct timeval timeout)
{
  dataclient_storage_response *response =
    malloc (sizeof (dataclient_storage_response));

  response->success = FALSE;
  response->timeout = timeout;

  return response;
}

static void
free_response (dataclient_storage_response *response)
{
  if (response->success && response->data != NULL)
    g_bytes_unref (response->data);
    
  free (response);
}

static void
process_response
(GzochidDataClient *client,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data)
{
  dataclient_storage_response_closure *closure = NULL;
  dataclient_storage_response *response = NULL;

  if (client->responses != NULL)
    {
      response = client->responses->data;
      client->responses = g_list_delete_link
	(client->responses, client->responses);
      
      closure = create_closure
	(client, response, success_callback, success_data, failure_callback,
	 failure_data);

      g_thread_new ("test-response", process_response_async, closure);
    }
  else client->deferred_response_closures = g_list_append
	 (client->deferred_response_closures,
	  create_closure
	  (client, create_failure_response ((struct timeval) { 0, 0 }),
	   success_callback, success_data, failure_callback, failure_data));
}

void
gzochid_dataclient_request_value
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gboolean for_write, gzochid_dataclient_success_callback success_callback,
 gpointer success_data, gzochid_dataclient_failure_callback failure_callback,
 gpointer failure_data, gzochid_dataclient_release_callback release_callback,
 gpointer release_data)
{
  client->requested_values =
    g_list_append (client->requested_values, g_bytes_ref (key));
  
  process_response
    (client, success_callback, success_data, failure_callback, failure_data);
}

void
gzochid_dataclient_request_next_key
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  client->requested_keys = g_list_append
    (client->requested_keys, key == NULL ? NULL : g_bytes_ref (key));
  
  process_response
      (client, success_callback, success_data, failure_callback, failure_data);
}

void
gzochid_dataclient_submit_changeset (GzochidDataClient *client, char *app,
				     GArray *changes)
{
  client->changesets = g_list_append
    (client->changesets, g_array_ref (changes));
}

void
gzochid_dataclient_release_key (GzochidDataClient *client, char *app,
				char *store, GBytes *key)
{
}

void
gzochid_dataclient_release_key_range (GzochidDataClient *client, char *app,
				      char *store, GBytes *from, GBytes *to)
{
}

struct _dataclient_storage_fixture
{
  GzochidDataClient *dataclient;
  gzochid_storage_engine_interface *iface;
  gzochid_storage_context *storage_context;
  gzochid_storage_store *store;
};

typedef struct _dataclient_storage_fixture dataclient_storage_fixture;

static void
dataclient_storage_fixture_setup (dataclient_storage_fixture *fixture,
				  gconstpointer user_data)
{
  fixture->dataclient = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  fixture->iface = &gzochid_storage_engine_interface_dataclient;
  fixture->storage_context = fixture->iface->initialize ("test");

  gzochid_dataclient_storage_context_set_dataclient
    (fixture->storage_context, fixture->dataclient);
  
  fixture->store = fixture->iface->open
    (fixture->storage_context, "test", GZOCHID_STORAGE_CREATE);
}

static void
maybe_bytes_unref (gpointer data)
{
  if (data != NULL)
    g_bytes_unref (data);
}

static void
clear_changeset (gpointer data)
{
  GArray *changes = data;
  int i = 0;

  for (; i < changes->len; i++)
    {
      gzochid_data_change *change = &g_array_index
	(changes, gzochid_data_change, i);

      free (change->store);

      g_bytes_unref (change->key);

      if (change->data != NULL)
	g_bytes_unref (change->data);
    }
  
  
  g_array_unref (changes);
}

static void
dataclient_storage_fixture_teardown (dataclient_storage_fixture *fixture,
				     gconstpointer user_data)
{
  g_list_free_full
    (fixture->dataclient->deferred_response_closures, execute_deferred_closure);

  g_list_free_full (fixture->dataclient->requested_keys, maybe_bytes_unref);
  g_list_free_full
    (fixture->dataclient->requested_values, (GDestroyNotify) g_bytes_unref);
  g_list_free_full (fixture->dataclient->changesets, clear_changeset);
  
  g_object_unref (fixture->dataclient);

  fixture->iface->close_store (fixture->store);
  fixture->iface->close_context (fixture->storage_context);
}

static void
test_get_cached (dataclient_storage_fixture *fixture, gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);
  free (value);

  fixture->iface->transaction_rollback (tx);
  tx = fixture->iface->transaction_begin (fixture->storage_context);
  value = fixture->iface->transaction_get
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  free_response (response);
  g_bytes_unref (success_bytes);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_get_uncached_success (dataclient_storage_fixture *fixture,
			   gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  
  g_bytes_unref (success_bytes);
  free_response (response);
  
  fixture->iface->transaction_rollback (tx);
}

static void
test_get_uncached_failure_success (dataclient_storage_fixture *fixture,
				   gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  value = fixture->iface->transaction_get
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  free_response (response1);
  
  g_bytes_unref (success_bytes);
  free_response (response2);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_get_uncached_timeout (dataclient_storage_fixture *fixture,
			   gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });

  value = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);

  g_assert (value == NULL);
  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_get_for_update_cached (dataclient_storage_fixture *fixture,
			    gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, NULL);

  free (value);

  fixture->iface->transaction_rollback (tx);
  tx = fixture->iface->transaction_begin (fixture->storage_context);
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  free_response (response);
  g_bytes_unref (success_bytes);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_get_for_update_cached_upgrade (dataclient_storage_fixture *fixture,
				    gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response1 =
    create_success_response (success_bytes);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  value = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);
  free (value);

  fixture->iface->transaction_rollback (tx);
  tx = fixture->iface->transaction_begin (fixture->storage_context);
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  free_response (response1);
  free_response (response2);
  g_bytes_unref (success_bytes);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_get_for_update_uncached_success (dataclient_storage_fixture *fixture,
				      gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  
  g_bytes_unref (success_bytes);
  free_response (response);
  
  fixture->iface->transaction_rollback (tx);
}

static void
test_get_for_update_uncached_failure_success
(dataclient_storage_fixture *fixture, gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert (value != NULL);
  g_assert (memcmp (value, "bar", MIN (4, value_len)) == 0);

  free (value);
  free_response (response1);
  
  g_bytes_unref (success_bytes);
  free_response (response2);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_get_for_update_uncached_timeout (dataclient_storage_fixture *fixture,
				      gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });

  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, NULL);

  g_assert (value == NULL);
  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);

}

static void
test_put_cached (dataclient_storage_fixture *fixture, gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, NULL);
  free (value);

  fixture->iface->transaction_put (tx, fixture->store, "foo", 4, "baz", 4);

  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);
  
  g_bytes_unref (success_bytes);
  free_response (response);

  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_put_cached_upgrade (dataclient_storage_fixture *fixture,
			 gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response1 =
    create_success_response (success_bytes);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  value = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);
  free (value);

  fixture->iface->transaction_put (tx, fixture->store, "foo", 4, "baz", 4);

  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);
  
  g_bytes_unref (success_bytes);
  free_response (response1);
  free_response (response2);

  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_put_uncached_success (dataclient_storage_fixture *fixture,
			   gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);
  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  fixture->iface->transaction_put (tx, fixture->store, "foo", 4, "baz", 4);

  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);  

  g_bytes_unref (success_bytes);
  free_response (response);
  
  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_put_uncached_failure_success (dataclient_storage_fixture *fixture,
				  gconstpointer user_data)
{
  size_t value_len = 0;  
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  fixture->iface->transaction_put (tx, fixture->store, "foo", 4, "baz", 4);

  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);  

  g_bytes_unref (success_bytes);
  free_response (response1);
  free_response (response2);
  
  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_put_uncached_timeout (dataclient_storage_fixture *fixture,
			   gconstpointer user_data)
{
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });

  fixture->iface->transaction_put (tx, fixture->store, "foo", 4, "bar", 4);

  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_delete_cached (dataclient_storage_fixture *fixture,
		    gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  int ret = 0;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  value = fixture->iface->transaction_get_for_update
    (tx, fixture->store, "foo", 4, NULL);
  free (value);

  ret = fixture->iface->transaction_delete (tx, fixture->store, "foo", 4);

  g_assert_cmpint (ret, ==, 0);
  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);
  
  g_bytes_unref (success_bytes);
  free_response (response);

  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_delete_cached_upgrade (dataclient_storage_fixture *fixture,
			    gconstpointer user_data)
{
  unsigned char *value = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response1 =
    create_success_response (success_bytes);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  int ret = 0;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  value = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);
  free (value);

  ret = fixture->iface->transaction_delete (tx, fixture->store, "foo", 4);

  g_assert_cmpint (ret, ==, 0);
  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);
  
  g_bytes_unref (success_bytes);
  free_response (response1);
  free_response (response2);

  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_delete_uncached_success (dataclient_storage_fixture *fixture,
			      gconstpointer user_data)
{
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);
  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);

  int ret = 0;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  ret = fixture->iface->transaction_delete (tx, fixture->store, "foo", 4);

  g_assert_cmpint (ret, ==, 0);
  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);  

  g_bytes_unref (success_bytes);
  free_response (response);
  
  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_delete_uncached_failure_success (dataclient_storage_fixture *fixture,
				      gconstpointer user_data)
{
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  int ret = 0;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  ret = fixture->iface->transaction_delete (tx, fixture->store, "foo", 4);

  g_assert_cmpint (ret, ==, 0);  
  g_assert (! tx->rollback);
  
  fixture->iface->transaction_prepare (tx);
  fixture->iface->transaction_commit (tx);  

  g_bytes_unref (success_bytes);
  free_response (response1);
  free_response (response2);
  
  g_assert_cmpint (g_list_length (fixture->dataclient->changesets), ==, 1);
  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 2);
}

static void
test_delete_uncached_timeout (dataclient_storage_fixture *fixture,
			      gconstpointer user_data)
{
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  
  int ret = fixture->iface->transaction_delete (tx, fixture->store, "foo", 4);

  g_assert_cmpint (ret, ==, GZOCHID_STORAGE_ETXFAILURE);  
  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint
    (g_list_length (fixture->dataclient->requested_values), ==, 1);
}

static void
test_first_key_cached (dataclient_storage_fixture *fixture,
		       gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  key = fixture->iface->transaction_first_key (tx, fixture->store, NULL);
  free (key);

  fixture->iface->transaction_rollback (tx);
  tx = fixture->iface->transaction_begin (fixture->storage_context);
  key = fixture->iface->transaction_first_key (tx, fixture->store, &key_len);

  g_assert (key != NULL);
  g_assert (memcmp (key, "bar", MIN (4, key_len)) == 0);

  free (key);
  free_response (response);
  g_bytes_unref (success_bytes);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
}

static void
test_first_key_uncached_success (dataclient_storage_fixture *fixture,
				 gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  key = fixture->iface->transaction_first_key (tx, fixture->store, NULL);

  g_assert (key != NULL);
  g_assert (memcmp (key, "bar", MIN (4, key_len)) == 0);

  fixture->iface->transaction_rollback (tx);

  free (key);
  free_response (response);
  g_bytes_unref (success_bytes);
}

static void
test_first_key_uncached_failure_success (dataclient_storage_fixture *fixture,
					 gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  key = fixture->iface->transaction_first_key (tx, fixture->store, &key_len);

  g_assert (key != NULL);
  g_assert (memcmp (key, "bar", MIN (4, key_len)) == 0);

  free (key);
  free_response (response1);
  
  g_bytes_unref (success_bytes);
  free_response (response2);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
}

static void
test_first_key_uncached_timeout (dataclient_storage_fixture *fixture,
				 gconstpointer user_data)
{
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });

  key = fixture->iface->transaction_first_key (tx, fixture->store, NULL);

  g_assert (key == NULL);
  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
}

static void
test_next_key_cached (dataclient_storage_fixture *fixture,
		      gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("foo", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  key = fixture->iface->transaction_next_key
    (tx, fixture->store, "bar", 4, NULL);
  free (key);

  fixture->iface->transaction_rollback (tx);
  tx = fixture->iface->transaction_begin (fixture->storage_context);
  key = fixture->iface->transaction_next_key
    (tx, fixture->store, "bar", 4, &key_len);

  g_assert (key != NULL);
  g_assert (memcmp (key, "foo", MIN (4, key_len)) == 0);

  free (key);
  free_response (response);
  g_bytes_unref (success_bytes);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
}

static void
test_next_key_uncached_success (dataclient_storage_fixture *fixture,
				gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);

  GBytes *success_bytes = g_bytes_new_static ("foo", 4);
  dataclient_storage_response *response =
    create_success_response (success_bytes);
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);
  
  key = fixture->iface->transaction_next_key
    (tx, fixture->store, "bar", 4, NULL);

  g_assert (key != NULL);
  g_assert (memcmp (key, "foo", MIN (4, key_len)) == 0);

  fixture->iface->transaction_rollback (tx);

  free (key);
  free_response (response);
  g_bytes_unref (success_bytes);
}

static void
test_next_key_uncached_failure_success (dataclient_storage_fixture *fixture,
					gconstpointer user_data)
{
  size_t key_len = 0;  
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });
  dataclient_storage_response *response1 = create_failure_response
    ((struct timeval) { 0, 5000 });

  GBytes *success_bytes = g_bytes_new_static ("foo", 4);
  dataclient_storage_response *response2 =
    create_success_response (success_bytes);

  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response1);
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response2);
  
  key = fixture->iface->transaction_next_key
    (tx, fixture->store, "bar", 4, &key_len);

  g_assert (key != NULL);
  g_assert (memcmp (key, "foo", MIN (4, key_len)) == 0);

  free (key);
  free_response (response1);
  
  g_bytes_unref (success_bytes);
  free_response (response2);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
}

static void
test_next_key_uncached_timeout (dataclient_storage_fixture *fixture,
				gconstpointer user_data)
{
  unsigned char *key = NULL;
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin_timed
    (fixture->storage_context, (struct timeval) { 0, 100000 });

  key = fixture->iface->transaction_next_key
    (tx, fixture->store, "bar", 4, NULL);

  g_assert (key == NULL);
  g_assert (tx->rollback);
  g_assert (tx->should_retry);
  
  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
    ("/storage-dataclient/get/cached", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_get_cached,
     dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/get/uncached/success", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_get_uncached_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_get_uncached_failure_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get/uncached/timeout", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_get_uncached_timeout,
     dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/get-for-update/cached", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_get_for_update_cached,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get-for-update/cached/upgrade",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_get_for_update_cached, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get-for-update/uncached/success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_get_for_update_uncached_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get-for-update/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_get_for_update_uncached_failure_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/get-for-update/uncached/timeout",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_get_for_update_uncached_timeout, dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/put/cached", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_put_cached,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/put/cached/upgrade", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_put_cached_upgrade,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/put/uncached/success", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_put_uncached_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/put/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_put_uncached_failure_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/put/uncached/timeout", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_put_uncached_timeout,
     dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/delete/cached", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_delete_cached,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/delete/cached/upgrade", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_delete_cached_upgrade,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/delete/uncached/success", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_delete_uncached_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/delete/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_delete_uncached_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/delete/uncached/timeout", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_delete_uncached_timeout,
     dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/first-key/cached", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_first_key_cached,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/first-key/uncached/success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_first_key_uncached_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/first-key/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_first_key_uncached_failure_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/first-key/uncached/timeout",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_first_key_uncached_timeout, dataclient_storage_fixture_teardown);

  g_test_add
    ("/storage-dataclient/next-key/cached", dataclient_storage_fixture, NULL,
     dataclient_storage_fixture_setup, test_next_key_cached,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/next-key/uncached/success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_next_key_uncached_success, dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/next-key/uncached/failure-success",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_next_key_uncached_failure_success,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/next-key/uncached/timeout",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_next_key_uncached_timeout, dataclient_storage_fixture_teardown);

  return g_test_run ();
}
