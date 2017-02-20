/* test-storage-dataclient.c: Tests for storage-dataclient.c in gzochid.
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

#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
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

  gchar *qualified_key;
  gzochid_dataclient_release_callback release_callback;
  gpointer release_data;
};

typedef struct _dataclient_storage_response_closure
dataclient_storage_response_closure;

struct _dataclient_release_closure
{
  gchar *qualified_key;
  gzochid_dataclient_release_callback release_callback;
  gpointer release_data;
};

typedef struct _dataclient_release_closure dataclient_release_closure;

struct _GzochidDataClient
{
  GObject parent_instance;

  GMutex process_response_mutex;
  GCond process_response_cond;
  
  GList *responses;
  GList *requested_keys;
  GList *released_keys;
  GList *changesets;
  GList *deferred_response_closures;
  GList *release_closures;
  GThread *processing_thread;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

static void
gzochid_data_client_finalize (GObject *object)
{
  GzochidDataClient *self = GZOCHID_DATA_CLIENT (object);

  g_mutex_clear (&self->process_response_mutex);
  g_cond_clear (&self->process_response_cond);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_data_client_finalize;
}

static void
execute_release_closure (gpointer data)
{
  dataclient_release_closure *closure = data;
  closure->release_callback (closure->release_data);

  g_free (closure->qualified_key);
  free (closure);
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  g_mutex_init (&self->process_response_mutex);
  g_cond_init (&self->process_response_cond);
  
  self->responses = NULL;
  self->requested_keys = NULL;
  self->released_keys = NULL;
  self->changesets = NULL;
  self->deferred_response_closures = NULL;
  self->release_closures = NULL;
}

static dataclient_release_closure *
create_release_closure (gchar *qualified_key,
			gzochid_dataclient_release_callback release_callback,
			gpointer release_data)
{
  dataclient_release_closure *closure =
    malloc (sizeof (dataclient_release_closure));

  closure->qualified_key = g_strdup (qualified_key);
  closure->release_callback = release_callback;
  closure->release_data = release_data;
  
  return closure;
}

static gpointer
process_response_async (gpointer data)
{
  dataclient_storage_response_closure *closure = data;
  GzochidDataClient *client = closure->client;
  
  if (closure->response->success)
    {
      closure->success_callback
	(closure->response->data, closure->success_data);

      client->release_closures =
	g_list_append (client->release_closures, 
		       create_release_closure (closure->qualified_key,
					       closure->release_callback,
					       closure->release_data));      
    }
  else closure->failure_callback
	 (closure->response->timeout, closure->failure_data);
      
  g_free (closure->qualified_key);
  free (closure);

  g_mutex_lock (&client->process_response_mutex);  
  g_cond_signal (&client->process_response_cond);
  g_mutex_unlock (&client->process_response_mutex);
  
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
create_response_closure
(GzochidDataClient *client, gchar *qualified_key,
 dataclient_storage_response *response,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  dataclient_storage_response_closure *closure =
    malloc (sizeof (dataclient_storage_response_closure));

  closure->client = client;
  closure->response = response;

  closure->success_callback = success_callback;
  closure->success_data = success_data;
  
  closure->failure_callback = failure_callback;
  closure->failure_data = failure_data;

  closure->qualified_key = g_strdup (qualified_key);
  closure->release_callback = release_callback;
  closure->release_data = release_data;
  
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
(GzochidDataClient *client, gchar *qualified_key,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  dataclient_storage_response_closure *closure = NULL;
  dataclient_storage_response *response = NULL;

  if (client->responses != NULL)
    {
      response = client->responses->data;
      client->responses = g_list_delete_link
	(client->responses, client->responses);
      
      closure = create_response_closure
	(client, qualified_key, response, success_callback, success_data,
	 failure_callback, failure_data, release_callback, release_data);

      client->processing_thread =
	g_thread_new ("test-response", process_response_async, closure);
    }
  else client->deferred_response_closures = g_list_append
	 (client->deferred_response_closures,
	  create_response_closure
	  (client, qualified_key,
	   create_failure_response ((struct timeval) { 0, 0 }),
	   success_callback, success_data, failure_callback, failure_data,
	   release_callback, release_data));
}

static const gchar *
key_text (GBytes *key)
{
  size_t size = 0;
  const char *ret = g_bytes_get_data (key, &size);
  assert (index (ret, '/') == NULL);
  return ret;
}

static gchar *
create_qualified_key (char *app, char *store, GBytes *key)
{
  if (key == NULL)
    return g_strdup_printf ("/%s/%s/", app, store);
  else return g_strdup_printf ("/%s/%s/%s", app, store, key_text (key));
}

static gchar *
create_qualified_key_range (char *app, char *store, GBytes *from, GBytes *to)
{
  if (from == NULL)
    {
      if (to == NULL)
	return g_strdup_printf ("/%s/%s//", app, store);
      else return g_strdup_printf ("/%s/%s//%s", app, store, key_text (to));
    }
  else if (to == NULL)
    return g_strdup_printf ("/%s/%s/%s//", app, store, key_text (from));
  else return g_strdup_printf
	 ("/%s/%s/%s/%s", app, store, key_text (from), key_text (to));
}

static gint
find_release_closure (gconstpointer a, gconstpointer b)
{
  const dataclient_release_closure *closure = a;
  const char *qualified_key = b;

  return strcmp (closure->qualified_key, qualified_key);
}

static void
release_key (GzochidDataClient *client, char *app, char *store, GBytes *key)
{
  gchar *release_closure_key = create_qualified_key (app, store, key);
  GList *release_closure_ptr = g_list_find_custom
    (client->release_closures, release_closure_key, find_release_closure);

  if (release_closure_ptr != NULL)
    {
      execute_release_closure (release_closure_ptr->data);
      client->release_closures = g_list_delete_link
	(client->release_closures, release_closure_ptr);
    }
  
  g_free (release_closure_key);
}

static void
release_key_range (GzochidDataClient *client, char *app, char *store,
		   GBytes *from, GBytes *to)
{
  gchar *release_closure_key =
    create_qualified_key_range (app, store, from, to);
  GList *release_closure_ptr = g_list_find_custom
    (client->release_closures, release_closure_key, find_release_closure);

  if (release_closure_ptr != NULL)
    {
      execute_release_closure (release_closure_ptr->data);
      client->release_closures = g_list_delete_link
	(client->release_closures, release_closure_ptr);
    }

  g_free (release_closure_key);
}

void
gzochid_dataclient_request_value
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gboolean for_write, gzochid_dataclient_success_callback success_callback,
 gpointer success_data, gzochid_dataclient_failure_callback failure_callback,
 gpointer failure_data, gzochid_dataclient_release_callback release_callback,
 gpointer release_data)
{
  gchar *qualified_key = create_qualified_key (app, store, key);

  client->requested_keys =
    g_list_append (client->requested_keys, qualified_key);

  process_response
    (client, qualified_key, success_callback, success_data, failure_callback,
     failure_data, release_callback, release_data);
}

void
gzochid_dataclient_request_next_key
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  gchar *qualified_key_range =
    create_qualified_key_range (app, store, key, NULL);
  
  client->requested_keys = g_list_append
    (client->requested_keys, qualified_key_range);
  
  process_response
    (client, qualified_key_range, success_callback, success_data,
     failure_callback, failure_data, release_callback, release_data);
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
  client->released_keys = g_list_append
    (client->released_keys, create_qualified_key (app, store, key));
}

void
gzochid_dataclient_release_key_range (GzochidDataClient *client, char *app,
				      char *store, GBytes *from, GBytes *to)
{
  client->released_keys = g_list_append
    (client->released_keys, create_qualified_key_range (app, store, from, to));
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
  if (fixture->dataclient->processing_thread != NULL)
    g_thread_join (fixture->dataclient->processing_thread);

  g_list_free_full
    (fixture->dataclient->deferred_response_closures, execute_deferred_closure);
  g_list_free_full (fixture->dataclient->requested_keys, g_free);
  g_list_free_full (fixture->dataclient->changesets, clear_changeset);
  g_list_free_full
    (fixture->dataclient->release_closures, execute_release_closure);
  g_list_free_full (fixture->dataclient->released_keys, g_free);

  fixture->iface->close_store (fixture->store);
  fixture->iface->close_context (fixture->storage_context);

  g_object_unref (fixture->dataclient);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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
  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 2);
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

  g_assert_cmpint (g_list_length (fixture->dataclient->requested_keys), ==, 1);
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

static void
test_lock_release_eviction (dataclient_storage_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *key_bytes = g_bytes_new_static ("foo", 4);
  GBytes *value_bytes = g_bytes_new_static ("bar", 4);
  dataclient_storage_response *response = create_success_response (value_bytes);
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);
  char *val = NULL;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);

  g_mutex_lock (&fixture->dataclient->process_response_mutex);
  
  val = fixture->iface->transaction_get (tx, fixture->store, "foo", 4, NULL);  

  g_cond_wait (&fixture->dataclient->process_response_cond,
	       &fixture->dataclient->process_response_mutex);

  g_mutex_unlock (&fixture->dataclient->process_response_mutex);

  release_key (fixture->dataclient, "test", "test", key_bytes);
  
  g_assert_cmpint (g_list_length (fixture->dataclient->released_keys), ==, 0);

  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->released_keys), ==, 1);

  free (val);
  
  free_response (response);
  g_bytes_unref (key_bytes);
  g_bytes_unref (value_bytes);
}

static void
test_range_lock_release_eviction (dataclient_storage_fixture *fixture,
				  gconstpointer user_data)
{
  GBytes *key_bytes = g_bytes_new_static ("foo", 4);
  dataclient_storage_response *response = create_success_response (key_bytes);
  gzochid_storage_transaction *tx = fixture->iface->transaction_begin
    (fixture->storage_context);
  char *key = NULL;
  
  fixture->dataclient->responses = g_list_append
    (fixture->dataclient->responses, response);

  g_mutex_lock (&fixture->dataclient->process_response_mutex);

  key = fixture->iface->transaction_first_key (tx, fixture->store, NULL);  
  
  g_cond_wait (&fixture->dataclient->process_response_cond,
	       &fixture->dataclient->process_response_mutex);

  g_mutex_unlock (&fixture->dataclient->process_response_mutex);
  
  release_key_range (fixture->dataclient, "test", "test", NULL, NULL);

  g_assert_cmpint (g_list_length (fixture->dataclient->released_keys), ==, 0);

  fixture->iface->transaction_rollback (tx);

  g_assert_cmpint (g_list_length (fixture->dataclient->released_keys), ==, 1);

  free (key);
  
  free_response (response);
  g_bytes_unref (key_bytes);
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

  g_test_add
    ("/storage-dataclient/lock-release/eviction", dataclient_storage_fixture,
     NULL, dataclient_storage_fixture_setup, test_lock_release_eviction,
     dataclient_storage_fixture_teardown);
  g_test_add
    ("/storage-dataclient/range-lock-release/eviction",
     dataclient_storage_fixture, NULL, dataclient_storage_fixture_setup,
     test_range_lock_release_eviction, dataclient_storage_fixture_teardown);
  
  return g_test_run ();
}
