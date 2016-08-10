/* test-durable-task.c: Test routines for durable-task.c in gzochid.
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
#include <stdlib.h>

#include "app.h"
#include "app-task.h"
#include "data.h"
#include "durable-task.h"
#include "game.h"
#include "gzochid-auth.h"
#include "oids.h"
#include "oids-storage.h"
#include "schedule.h"
#include "storage-mem.h"

static GCond test_worker_cond;
static GMutex test_worker_mutex;
static int test_worker_counter;

static void
test_counter_worker (gzochid_application_context *context, 
		     gzochid_auth_identity *identity, gpointer data)
{
  g_mutex_lock (&test_worker_mutex);

  test_worker_counter++;

  g_cond_signal (&test_worker_cond);
  g_mutex_unlock (&test_worker_mutex);
}

static void 
test_counter_worker_serializer (gzochid_application_context *context, 
				gzochid_application_worker worker,
				GByteArray *out)
{
}

static gzochid_application_worker 
test_counter_worker_deserializer (gzochid_application_context *context,
				  GByteArray *in)
{
  return test_counter_worker;
}

static gzochid_application_worker_serialization counter_worker_serialization = 
  { test_counter_worker_serializer, test_counter_worker_deserializer };

static void 
serialize_test_worker_counter (gzochid_application_context *context,
			       gpointer data, GByteArray *out, GError **error)
{
}

static gpointer 
deserialize_test_worker_counter (gzochid_application_context *context,
				 GByteArray *in, GError **error)
{
  return "";
}

static void 
finalize_test_worker_counter (gzochid_application_context *context, 
			      gpointer data)
{
}

static gzochid_io_serialization test_worker_counter_serialization =
  {
    serialize_test_worker_counter,
    deserialize_test_worker_counter,
    finalize_test_worker_counter
  };

static gzochid_application_task_serialization task_serialization = 
  { "test", &counter_worker_serialization, &test_worker_counter_serialization };

struct _test_context
{
  gzochid_application_context *app_context;
  gzochid_auth_identity *identity;
  guint64 handle_oid;
};

typedef struct _test_context test_context;

static void 
test_periodic_cancel_inner0 (gpointer data)
{
  test_context *context = data;
  struct timeval immediate = { 0, 0 };

  gzochid_application_task *task = gzochid_application_task_new
    (context->app_context, context->identity, test_counter_worker, "");
  gzochid_periodic_task_handle *handle = gzochid_schedule_periodic_durable_task
    (context->app_context, context->identity, task, 
     &task_serialization, immediate, immediate);
  gzochid_data_managed_reference *handle_ref = gzochid_data_create_reference 
    (context->app_context, 
     &gzochid_durable_application_task_handle_serialization, handle, NULL);

  context->handle_oid = handle_ref->oid;

  gzochid_application_task_unref (task);
}

static void 
test_periodic_cancel_inner1 (gpointer data)
{
  GError *err = NULL;
  test_context *context = data;
  gzochid_data_managed_reference *handle_ref = 
    gzochid_data_create_reference_to_oid 
    (context->app_context, 
     &gzochid_durable_application_task_handle_serialization,
     context->handle_oid);

  gzochid_data_dereference (handle_ref, &err);

  if (err == NULL)
    gzochid_cancel_periodic_task (context->app_context, handle_ref->obj);

  g_clear_error (&err);
}

static void 
application_context_init (gzochid_application_context *context)
{
  gzochid_context *base = (gzochid_context *) context;
  gzochid_game_context *game_context = gzochid_game_context_new (NULL);
  base->parent = (gzochid_context *) game_context;

  game_context->storage_engine = malloc (sizeof (gzochid_storage_engine));
  game_context->storage_engine->interface = 
    &gzochid_storage_engine_interface_mem;

  context->storage_context = 
    gzochid_storage_engine_interface_mem.initialize ("/dev/null");
  context->meta = gzochid_storage_engine_interface_mem.open
    (context->storage_context, "/dev/null", 0);
  context->oids = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);
  context->names = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);

  context->oid_strategy = gzochid_storage_oid_strategy_new
    (game_context->storage_engine->interface, context->storage_context,
     context->meta);

  context->identity_cache = gzochid_auth_identity_cache_new ();
}

static void
application_context_clear (gzochid_application_context *context)
{
  gzochid_oid_allocation_strategy_free (context->oid_strategy);
  
  gzochid_auth_identity_cache_destroy (context->identity_cache);
}

static void 
test_periodic_cancel ()
{
  test_context context;
  gboolean submitted = FALSE, done = FALSE;

  gzochid_game_context *game_context = NULL;
  gzochid_application_context *app_context = 
    gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  GThreadPool *pool = NULL;

  application_context_init (app_context);

  game_context = (gzochid_game_context *) 
    ((gzochid_context *) app_context)->parent;

  pool = gzochid_thread_pool_new (game_context, 1, TRUE, NULL);

  game_context->task_queue = gzochid_schedule_task_queue_new (pool);
  game_context->tx_timeout = (struct timeval) { INT_MAX, INT_MAX };
  gzochid_schedule_task_queue_start (game_context->task_queue);  

  context.app_context = app_context;
  context.identity = identity;

  while (TRUE)
    {
      g_mutex_lock (&test_worker_mutex);

      if (!submitted)
	{
	  gzochid_transaction_execute (test_periodic_cancel_inner0, &context);
	  submitted = TRUE;
	}

      g_cond_wait (&test_worker_cond, &test_worker_mutex);
      if (test_worker_counter >= 3)
	{
	  gzochid_transaction_execute (test_periodic_cancel_inner1, &context);
	  done = TRUE;
	}

      g_mutex_unlock (&test_worker_mutex);
      if (done)
	break;
    }

  g_assert_cmpint (test_worker_counter, >=, 3);

  gzochid_schedule_task_queue_stop (game_context->task_queue);
  g_thread_pool_free (pool, TRUE, TRUE);

  gzochid_auth_identity_unref (identity);  
  application_context_clear (app_context);
}

static GString *test_worker_string;

static void
serialize_test_worker_string (gzochid_application_context *context,
			      gpointer data, GByteArray *out, GError **error)
{
}

static gpointer
deserialize_test_worker_string (gzochid_application_context *context,
				GByteArray *in, GError **error)
{
  return test_worker_string;
}

static void
finalize_test_worker_string (gzochid_application_context *context,
			     gpointer data)
{
}

static gzochid_io_serialization test_worker_string_serialization =
  {
    serialize_test_worker_string,
    deserialize_test_worker_string,
    finalize_test_worker_string
  };

static void
serialize_task_worker_noop (gzochid_application_context *context,
			    gzochid_application_worker worker, GByteArray *out)
{
}

static void
test_string_worker_a (gzochid_application_context *context,
		      gzochid_auth_identity *identity, gpointer data)
{
  GString *str = data;
  g_string_append (str, "a");
}

static gzochid_application_worker
deserialize_task_worker_a (gzochid_application_context *context, GByteArray *in)
{
  return test_string_worker_a;
}

static gzochid_application_worker_serialization test_worker_serialization_a =
  { serialize_task_worker_noop, deserialize_task_worker_a };

static gzochid_application_task_serialization string_task_serialization_a =
  {
    "string-task-serialization-a",
    &test_worker_serialization_a,
    &test_worker_string_serialization
  };

static void
test_string_worker_b (gzochid_application_context *context,
		      gzochid_auth_identity *identity, gpointer data)
{
  GString *str = data;
  g_string_append (str, "b");
}

static gzochid_application_worker
deserialize_task_worker_b (gzochid_application_context *context, GByteArray *in)
{
  return test_string_worker_b;
}

static gzochid_application_worker_serialization test_worker_serialization_b =
  { serialize_task_worker_noop, deserialize_task_worker_b };

static gzochid_application_task_serialization string_task_serialization_b =
  {
    "string-task-serialization-b",
    &test_worker_serialization_b,
    &test_worker_string_serialization
  };

static void
test_string_worker_c (gzochid_application_context *context,
		      gzochid_auth_identity *identity, gpointer data)
{
  GString *str = data;
  g_string_append (str, "c");

  g_mutex_lock (&test_worker_mutex);
  g_cond_signal (&test_worker_cond);
  g_mutex_unlock (&test_worker_mutex);
}

static gzochid_application_worker
deserialize_task_worker_c (gzochid_application_context *context, GByteArray *in)
{
  return test_string_worker_c;
}

static gzochid_application_worker_serialization test_worker_serialization_c =
  { serialize_task_worker_noop, deserialize_task_worker_c };

static gzochid_application_task_serialization string_task_serialization_c =
  {
    "string-task-serialization-c",
    &test_worker_serialization_c,
    &test_worker_string_serialization
  };

static void 
test_task_chain_inner0 (gpointer data)
{
  test_context *context = data;
  struct timeval immediate = { 0, 0 };

  test_worker_string = g_string_new ("");
  
  gzochid_application_task *task_a = gzochid_application_task_new
    (context->app_context, context->identity, test_string_worker_a,
     test_worker_string);
  gzochid_application_task *task_b = gzochid_application_task_new
    (context->app_context, context->identity, test_string_worker_b,
     test_worker_string);
  gzochid_application_task *task_c = gzochid_application_task_new
    (context->app_context, context->identity, test_string_worker_c,
     test_worker_string);

  gzochid_durable_application_task_handle *handle_a =
    gzochid_create_durable_application_task_handle
    (task_a, &string_task_serialization_a, immediate, NULL, NULL);
  gzochid_durable_application_task_handle *handle_b =
    gzochid_create_durable_application_task_handle
    (task_b, &string_task_serialization_b, immediate, NULL, NULL);
  gzochid_durable_application_task_handle *handle_c =
    gzochid_create_durable_application_task_handle
    (task_c, &string_task_serialization_c, immediate, NULL, NULL);
  
  gzochid_durable_queue *queue =
    gzochid_durable_queue_new (context->app_context);

  gzochid_durable_queue_offer
    (queue, &gzochid_durable_application_task_handle_serialization, handle_a,
     NULL);
  gzochid_durable_queue_offer
    (queue, &gzochid_durable_application_task_handle_serialization, handle_b,
     NULL);
  gzochid_durable_queue_offer
    (queue, &gzochid_durable_application_task_handle_serialization, handle_c,
     NULL);

  gzochid_schedule_durable_task_chain
    (context->app_context, context->identity, queue, NULL);

  gzochid_application_task_unref (task_a);
  gzochid_application_task_unref (task_b);
  gzochid_application_task_unref (task_c);
}

static void 
test_task_chain_simple ()
{
  test_context context;
  gboolean submitted = FALSE, done = FALSE;

  gzochid_game_context *game_context = NULL;
  gzochid_application_context *app_context = 
    gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  GThreadPool *pool = NULL;

  application_context_init (app_context);

  game_context = (gzochid_game_context *) 
    ((gzochid_context *) app_context)->parent;

  pool = gzochid_thread_pool_new (game_context, 1, TRUE, NULL);

  game_context->task_queue = gzochid_schedule_task_queue_new (pool);
  game_context->tx_timeout = (struct timeval) { INT_MAX, INT_MAX };
  gzochid_schedule_task_queue_start (game_context->task_queue);  

  context.app_context = app_context;
  context.identity = identity;
  context.handle_oid = 0;

  g_mutex_lock (&test_worker_mutex);
  
  gzochid_transaction_execute (test_task_chain_inner0, &context);
  
  g_cond_wait (&test_worker_cond, &test_worker_mutex);
  g_mutex_unlock (&test_worker_mutex);

  g_assert_cmpstr (test_worker_string->str, ==, "abc");  

  gzochid_schedule_task_queue_stop (game_context->task_queue);
  g_thread_pool_free (pool, TRUE, TRUE);
}

int
main (int argc, char *argv[])
{
  g_cond_init (&test_worker_cond);
  g_mutex_init (&test_worker_mutex);

  gzochid_task_initialize_serialization_registry ();
  gzochid_task_register_serialization (&task_serialization);
  gzochid_task_register_serialization (&string_task_serialization_a);
  gzochid_task_register_serialization (&string_task_serialization_b);
  gzochid_task_register_serialization (&string_task_serialization_c);

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/durable-task/periodic/cancel", test_periodic_cancel);
  g_test_add_func ("/dutable-task/chain/simple", test_task_chain_simple);

  return g_test_run ();
}
