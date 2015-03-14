/* test-task.c: Test routines for task.c in gzochid.
 * Copyright (C) 2015 Julian Graham
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
#include <gmp.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>

#include "app.h"
#include "app-task.h"
#include "game.h"
#include "gzochid-auth.h"
#include "io.h"
#include "schedule.h"
#include "task.h"
#include "threads.h"
#include "tx.h"

static GCond test_worker_cond;
static GMutex test_worker_mutex;
static int test_worker_counter;

static void 
test_worker (gzochid_application_context *context, 
	     gzochid_auth_identity *identity, gpointer data)
{
  g_mutex_lock (&test_worker_mutex);

  test_worker_counter++;

  g_cond_signal (&test_worker_cond);
  g_mutex_unlock (&test_worker_mutex);
}

static void 
test_worker_serializer (gzochid_application_context *context, 
			gzochid_application_worker worker, GString *out)
{
}

static gzochid_application_worker 
test_worker_deserializer (gzochid_application_context *context, GString *in)
{
  return test_worker;
}

static gzochid_application_worker_serialization worker_serialization = 
  { test_worker_serializer, test_worker_deserializer };

static void 
serialize_test_worker_counter (gzochid_application_context *context,
			       gpointer data, GString *out, GError **error)
{
}

static gpointer 
deserialize_test_worker_counter (gzochid_application_context *context,
				 GString *in, GError **error)
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
  { "test", &worker_serialization, &test_worker_counter_serialization };

typedef struct test_context
{
  gzochid_application_context *app_context;
  gzochid_auth_identity *identity;
  mpz_t handle_oid;
} test_context;

static void 
test_periodic_cancel_inner0 (gpointer data)
{
  test_context *context = (test_context *) data;
  struct timeval immediate = { 0, 0 };

  gzochid_application_task *task = gzochid_application_task_new
    (context->app_context, context->identity, test_worker, "");
  gzochid_periodic_task_handle *handle = gzochid_schedule_periodic_durable_task
    (context->app_context, context->identity, task, 
     &task_serialization, immediate, immediate);
  gzochid_data_managed_reference *handle_ref = gzochid_data_create_reference 
    (context->app_context, 
     &gzochid_durable_application_task_handle_serialization, handle);

  mpz_set (context->handle_oid, handle_ref->oid);
}

static void 
test_periodic_cancel_inner1 (gpointer data)
{
  test_context *context = (test_context *) data;
  gzochid_data_managed_reference *handle_ref = 
    gzochid_data_create_reference_to_oid 
    (context->app_context, 
     &gzochid_durable_application_task_handle_serialization, 
     context->handle_oid);

  gzochid_data_dereference (handle_ref, NULL);

  gzochid_cancel_periodic_task 
    (context->app_context, 
     (gzochid_durable_application_task_handle *) handle_ref->obj);
}

static void 
application_context_init (gzochid_application_context *context)
{
  context->storage_context = gzochid_storage_initialize ("/dev/null");
  context->meta = gzochid_storage_open
    (context->storage_context, "/dev/null", 0);
  context->oids = gzochid_storage_open 
    (context->storage_context, "/dev/null", 0);
  context->names = gzochid_storage_open 
    (context->storage_context, "/dev/null", 0);
}

static void 
test_periodic_cancel ()
{
  test_context context;
  gboolean submitted = FALSE, done = FALSE;
  gzochid_periodic_task_handle handle;

  gzochid_game_context *game_context = 
    calloc (1, sizeof (gzochid_game_context));
  gzochid_application_context *app_context = 
    gzochid_application_context_new ();
  gzochid_auth_identity *identity = malloc (sizeof (gzochid_auth_identity));

  application_context_init (app_context);
  identity->name = "test";

  ((gzochid_context *) app_context)->parent = (gzochid_context *) game_context;
  game_context->task_queue = gzochid_schedule_task_queue_new 
    (gzochid_thread_pool_new (game_context, 1, TRUE, NULL));
  game_context->tx_timeout = (struct timeval) { INT_MAX, INT_MAX };
  gzochid_schedule_task_queue_start (game_context->task_queue);  

  context.app_context = app_context;
  context.identity = identity;
  mpz_init (context.handle_oid);

  while (TRUE)
    {
      g_mutex_lock (&test_worker_mutex);

      if (!submitted)
	{
	  gzochid_transaction_execute (test_periodic_cancel_inner0, &context);
	  submitted = TRUE;
	}

      g_cond_wait (&test_worker_cond, &test_worker_mutex);
      if (test_worker_counter == 3)
	{
	  gzochid_transaction_execute (test_periodic_cancel_inner1, &context);
	  done = TRUE;
	}

      g_mutex_unlock (&test_worker_mutex);
      if (done)
	break;
    }

  g_assert_cmpint (test_worker_counter, <=, 4);
  mpz_clear (context.handle_oid);
}

int
main (int argc, char *argv[])
{
  g_cond_init (&test_worker_cond);
  g_mutex_init (&test_worker_mutex);

  gzochid_task_initialize_serialization_registry ();
  gzochid_task_register_serialization (&task_serialization);

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/task/periodic/cancel", test_periodic_cancel);

  return g_test_run ();
}
