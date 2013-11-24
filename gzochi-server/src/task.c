/* task.c: Application task management routines for gzochid
 * Copyright (C) 2013 Julian Graham
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
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "auth.h"
#include "auth_int.h"
#include "data.h"
#include "game.h"
#include "guile.h"
#include "io.h"
#include "schedule.h"
#include "task.h"
#include "tx.h"
#include "txlog.h"
#include "util.h"

#define PENDING_TASK_PREFIX "s.pendingTask."

GHashTable *serialization_registry = NULL;

typedef struct _gzochid_durable_application_task
{
  mpz_t handle_oid;
} gzochid_durable_application_task;

gzochid_application_task_serialization *
gzochid_lookup_task_serialization (char *name)
{
  return (gzochid_application_task_serialization *) 
    g_hash_table_lookup (serialization_registry, name);
}

void gzochid_task_register_serialization 
(gzochid_application_task_serialization *serialization)
{
  g_hash_table_insert 
    (serialization_registry, serialization->name, serialization);
}

void gzochid_task_initialize_serialization_registry (void)
{
  serialization_registry = g_hash_table_new (g_str_hash, g_str_equal);
}

static int task_prepare (gpointer data)
{
  return TRUE;
}

static gzochid_task_transaction_context *create_transaction_context 
(gzochid_application_context *app_context)
{
  gzochid_task_transaction_context *tx_context = 
    calloc (1, sizeof (gzochid_task_transaction_context));

  tx_context->context = app_context;

  return tx_context;
}

static void cleanup_transaction (gzochid_task_transaction_context *tx_context)
{
  g_list_free (tx_context->scheduled_tasks);
  free (tx_context);
}

gzochid_application_task *gzochid_application_task_new 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_application_worker worker, gpointer data)
{
  gzochid_application_task *task = malloc (sizeof (gzochid_application_task));

  task->context = context;
  task->identity = identity;
  task->worker = worker;
  task->data = data;

  return task;
}

gzochid_task *gzochid_task_make_transactional_application_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_worker worker, gpointer data, 
 struct timeval target_execution_time)
{
  gzochid_application_task *transactional_task = 
    gzochid_application_task_new (context, identity, worker, data);
  gzochid_transactional_application_task_execution *execution = 
    gzochid_transactional_application_task_execution_new (transactional_task);
  gzochid_application_task *application_task = 
    gzochid_application_task_new 
    (context, identity, 
     gzochid_application_resubmitting_transactional_task_worker, execution);
  
  return gzochid_task_new 
    (gzochid_application_task_thread_worker, application_task, 
     target_execution_time);
}

gzochid_durable_application_task *gzochid_durable_application_task_new 
(mpz_t handle_oid)
{
  gzochid_durable_application_task *task = 
    malloc (sizeof (gzochid_durable_application_task));

  mpz_init (task->handle_oid);
  mpz_set (task->handle_oid, handle_oid);

  return task;
}

gzochid_durable_application_task_handle *create_durable_task_handle
(gzochid_data_managed_reference *task_data_reference, 
 gzochid_application_task_serialization *serialization,
 gzochid_auth_identity *identity, struct timeval target_execution_time)
{
  gzochid_durable_application_task_handle *durable_task_handle = 
    malloc (sizeof (gzochid_durable_application_task_handle));
  struct timeval immediate = { 0, 0 };

  durable_task_handle->task_data_reference = task_data_reference;
  durable_task_handle->serialization = serialization;
  durable_task_handle->identity = gzochid_auth_identity_clone (identity);
  durable_task_handle->repeats = FALSE;
  durable_task_handle->period = immediate;
  durable_task_handle->target_execution_time = target_execution_time;

  return durable_task_handle;
}

gzochid_durable_application_task_handle *create_durable_periodic_task_handle
(gzochid_data_managed_reference *task_data_reference, 
 gzochid_application_task_serialization *serialization, 
 gzochid_auth_identity *identity, struct timeval target_execution_time, 
 struct timeval period)
{
  gzochid_durable_application_task_handle *durable_task_handle = 
    create_durable_task_handle 
    (task_data_reference, serialization, identity, target_execution_time);

  durable_task_handle->repeats = TRUE;
  durable_task_handle->period = period;
  
  return durable_task_handle;
}

void gzochid_durable_application_task_free 
(gzochid_durable_application_task *task)
{
  free (task);
}

gpointer deserialize_durable_task_handle
(gzochid_application_context *context, GString *in)
{
  gzochid_durable_application_task_handle *handle = 
    malloc (sizeof (gzochid_durable_application_task_handle));
  char *serialization_name = NULL;

  mpz_t oid;

  mpz_init (oid);
  gzochid_util_deserialize_mpz (in, oid);
  
  serialization_name = gzochid_util_deserialize_string (in);
  handle->serialization = gzochid_lookup_task_serialization 
    (serialization_name);

  assert (handle->serialization != NULL);

  handle->task_worker = 
    handle->serialization->worker_serialization->deserializer (context, in);
  handle->task_data_reference = gzochid_data_create_reference_to_oid 
    (context, handle->serialization->data_serialization, oid);

  mpz_clear (oid);
 
  handle->identity = gzochid_auth_identity_deserializer (context, in);
  handle->repeats = gzochid_util_deserialize_boolean (in);

  if (handle->repeats)
    handle->period = gzochid_util_deserialize_timeval (in);

  handle->target_execution_time = gzochid_util_deserialize_timeval (in);
  
  free (serialization_name);
  return handle;
}

void serialize_durable_task_handle
(gzochid_application_context *context, gpointer data, GString *out)
{
  gzochid_durable_application_task_handle *handle = 
    (gzochid_durable_application_task_handle *) data;

  gzochid_util_serialize_mpz (handle->task_data_reference->oid, out);
  gzochid_util_serialize_string (handle->serialization->name, out);
  handle->serialization->worker_serialization->serializer 
    (context, handle->task_worker, out);

  gzochid_auth_identity_serializer (context, handle->identity, out);

  gzochid_util_serialize_boolean (handle->repeats, out);
  if (handle->repeats)
    gzochid_util_serialize_timeval (handle->period, out);

  gzochid_util_serialize_timeval (handle->target_execution_time, out);
}

static void finalize_durable_task_handle
(gzochid_application_context *context, gpointer data)
{
  gzochid_durable_application_task_handle *handle = 
    (gzochid_durable_application_task_handle *) data;

  gzochid_auth_identity_finalizer (context, handle->identity);
  free (handle);
}

gzochid_io_serialization 
gzochid_durable_application_task_handle_serialization = 
  { 
    serialize_durable_task_handle, 
    deserialize_durable_task_handle, 
    finalize_durable_task_handle
  };

gzochid_task *gzochid_task_new 
(gzochid_thread_worker worker, gpointer data, 
 struct timeval target_execution_time)
{
  gzochid_task *task = malloc (sizeof (gzochid_task));
  
  task->worker = worker;
  task->data = data;
  task->target_execution_time = target_execution_time;

  return task;
}

gzochid_task *gzochid_task_immediate_new 
(gzochid_thread_worker worker, gpointer data)
{
  struct timeval now;
  gettimeofday (&now, NULL);

  return gzochid_task_new (worker, data, now);
}

static void durable_task_application_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

gzochid_task *wrap_durable_task 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_durable_application_task *durable_task,
 gzochid_durable_application_task_handle *durable_task_handle)
{
  gzochid_game_context *game_context = (gzochid_game_context *) 
    ((gzochid_context *) context)->parent;
  gzochid_auth_identity *cloned_identity = 
    gzochid_auth_identity_clone (identity);
  gzochid_application_task *transactional_task = gzochid_application_task_new
    (context, cloned_identity, durable_task_application_worker, durable_task);
  gzochid_transactional_application_task_execution *execution = 
    gzochid_transactional_application_task_timed_execution_new 
    (transactional_task, game_context->tx_timeout);
  gzochid_application_task *application_task = gzochid_application_task_new
    (context, cloned_identity,
     gzochid_application_resubmitting_transactional_task_worker, execution);

  return gzochid_task_new 
    (gzochid_application_task_thread_worker, application_task, 
     durable_task_handle->target_execution_time);
}

static gzochid_task *rebuild_durable_task
(gzochid_application_context *context, mpz_t oid)
{
  gzochid_durable_application_task *task = NULL;
  gzochid_durable_application_task_handle *handle = NULL;
  gzochid_data_managed_reference *handle_reference = 
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_durable_application_task_handle_serialization, oid);

  assert (gzochid_data_dereference (handle_reference) == 0);
  handle = (gzochid_durable_application_task_handle *) handle_reference->obj;
  task = gzochid_durable_application_task_new (oid);

  return wrap_durable_task
    (context, handle->identity, task, handle);
}

static void commit_scheduled_task (gpointer data, gpointer user_data)
{
  gzochid_task_transaction_context *tx_context = 
    (gzochid_task_transaction_context *) user_data;
  gzochid_application_context *context = tx_context->context;
  gzochid_game_context *game_context = (gzochid_game_context *)
    ((gzochid_context *) context)->parent;

  gzochid_task *task = (gzochid_task *) data;
  
  gzochid_schedule_submit_task (game_context->task_queue, task);
}

static void task_commit (gpointer data)
{
  gzochid_task_transaction_context *tx_context = 
    (gzochid_task_transaction_context *) data;

  g_list_foreach (tx_context->scheduled_tasks, commit_scheduled_task, data);

  cleanup_transaction (tx_context);
}

static void task_rollback (gpointer data)
{ 
  gzochid_task_transaction_context *tx_context = 
    (gzochid_task_transaction_context *) data;
  cleanup_transaction (tx_context);
}

static gzochid_transaction_participant task_participant = 
  { "task", task_prepare, task_commit, task_rollback };

static gzochid_task_transaction_context *join_transaction 
(gzochid_application_context *context)
{
  gzochid_task_transaction_context *tx_context = NULL;

  if (!gzochid_transaction_active ()
      || (tx_context = gzochid_transaction_context (&task_participant)) == NULL)
    {
      tx_context = create_transaction_context (context);
      gzochid_transaction_join (&task_participant, tx_context);
    }

  return tx_context;
}

void gzochid_restart_tasks (gzochid_application_context *context)
{
  mpz_t oid;
  char *next_binding = NULL;
  int prefix_len = strlen (PENDING_TASK_PREFIX);
  gzochid_task_transaction_context *tx_context = NULL;
  int num_tasks = 0;

  mpz_init (oid);
  gzochid_tx_info (context, "Resubmitting durable tasks.");
  next_binding = gzochid_data_next_binding_oid 
    (context, PENDING_TASK_PREFIX, oid);

  tx_context = join_transaction (context);

  while (next_binding != NULL 
	 && strncmp (PENDING_TASK_PREFIX, next_binding, prefix_len) == 0)
    {
      char *next_next_binding = 
	gzochid_data_next_binding_oid (context, next_binding, oid);
      gzochid_task *task = rebuild_durable_task (context, oid);
 
      tx_context->scheduled_tasks = g_list_append 
	(tx_context->scheduled_tasks, task);

      free (next_binding);
      next_binding = next_next_binding;
      num_tasks++;
    }

  if (num_tasks == 1)
    gzochid_tx_info (context, "Resubmitted 1 task.", num_tasks);
  else if (num_tasks > 1)
    gzochid_tx_info (context, "Resubmitted %d tasks.", num_tasks);
  else gzochid_tx_info (context, "No tasks found to resubmit.");

  mpz_clear (oid);  
}

gzochid_durable_application_task_handle *build_durable_task_handle
(gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization, 
 struct timeval delay, struct timeval *period)
{
  struct timeval target;

  gzochid_data_managed_reference *task_data_reference = NULL;
  gzochid_durable_application_task_handle *durable_task_handle = NULL;
  gzochid_data_managed_reference *handle_reference = NULL;

  char *oid_str = NULL;
  int oid_strlen, prefix_len;
  char *binding_name;

  gettimeofday (&target, NULL);

  target.tv_sec += delay.tv_sec;
  target.tv_usec += delay.tv_usec;

  task_data_reference = gzochid_data_create_reference 
    (task->context, serialization->data_serialization, task->data);

  if (period != NULL)
    durable_task_handle = create_durable_periodic_task_handle
      (task_data_reference, serialization, task->identity, target, *period);
  else durable_task_handle = create_durable_task_handle
	 (task_data_reference, serialization, task->identity, target);

  handle_reference = gzochid_data_create_reference 
    (task->context, &gzochid_durable_application_task_handle_serialization, 
     durable_task_handle);
    
  oid_str = mpz_get_str (NULL, 16, handle_reference->oid);
  oid_strlen = strlen (oid_str);
  prefix_len = strlen (PENDING_TASK_PREFIX);
  binding_name = calloc (oid_strlen + prefix_len + 1, sizeof (char));

  binding_name = strncat (binding_name, PENDING_TASK_PREFIX, prefix_len);
  binding_name = strncat (binding_name, oid_str, oid_strlen);
  
  free (oid_str);

  if (gzochid_data_set_binding_to_oid 
      (task->context, binding_name, handle_reference->oid) != 0)
    {
      free (binding_name);
      return NULL;
    }
  else
    {
      free (binding_name);
      return durable_task_handle;
    }
}

static void durable_task_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_durable_application_task *task = 
    (gzochid_durable_application_task *) data;
  gzochid_task_transaction_context *tx_context = NULL;
  gzochid_data_managed_reference *handle_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_durable_application_task_handle_serialization, 
     task->handle_oid);   
  gzochid_durable_application_task_handle *handle = NULL;
  gzochid_application_task *inner_task = NULL;

  if (gzochid_data_dereference (handle_reference) != 0)

    /* Not safe to proceed if the dereference fails. */

    return;

  handle = (gzochid_durable_application_task_handle *) handle_reference->obj;

  if (gzochid_data_dereference (handle->task_data_reference) != 0)

    /* ...nor if it fails here. */

    return;

  inner_task = gzochid_application_task_new 
    (context, identity, handle->task_worker, handle->task_data_reference->obj);

  inner_task->worker (context, identity, inner_task->data);
  free (inner_task);

  tx_context = join_transaction (context);
  if (gzochid_transaction_rollback_only ())

    /* The body of the task worker may have done something to cause the
       transaction to be rolled back. If that's the case, we should not bother
       to attempt any logic that involves the data manager. */

    return;

  if (handle->repeats)
    {
      gzochid_task *wrapped_task = NULL;
      struct timeval now;

      gettimeofday (&now, NULL);
      timeradd (&handle->period, 
		&handle->target_execution_time, 
		&handle->target_execution_time);

      if (gzochid_data_mark 
	  (context, &gzochid_durable_application_task_handle_serialization, 
	   handle) == 0)
	{
	  wrapped_task = wrap_durable_task (context, identity, task, handle);
	  
	  tx_context->scheduled_tasks = g_list_append 
	    (tx_context->scheduled_tasks, wrapped_task);
	}
    }
  else
    { 
      char *oid_str = mpz_get_str (NULL, 16, task->handle_oid);
      GString *binding = g_string_new (PENDING_TASK_PREFIX);
      
      g_string_append (binding, oid_str);
      
      if (gzochid_data_remove_binding (context, binding->str) != 0
	  || gzochid_data_remove_object (handle_reference) != 0)
	
	/* Nothing to be done here. */
      
	;

      g_string_free (binding, FALSE);
      free (oid_str);
    }
}

void gzochid_task_free (gzochid_task *task)
{
  free (task);
}

void gzochid_schedule_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization)
{
  struct timeval immediate = { 0, 0 };
  gzochid_schedule_delayed_durable_task 
    (context, identity, task, serialization, immediate);
}

static void schedule_durable_task 
(gzochid_application_context *context, gzochid_task *task)
{
  gzochid_task_transaction_context *tx_context = join_transaction (context);

  tx_context->scheduled_tasks = g_list_append 
    (tx_context->scheduled_tasks, task);
}

void gzochid_schedule_delayed_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization, 
 struct timeval delay)
{
  gzochid_durable_application_task_handle *handle = 
    build_durable_task_handle (task, serialization, delay, NULL);

  if (handle != NULL)
    {
      gzochid_data_managed_reference *handle_reference = 
	gzochid_data_create_reference 
	(context, &gzochid_durable_application_task_handle_serialization, 
	 handle);
      gzochid_task *wrapped_task = wrap_durable_task
	(context, identity,
	 gzochid_durable_application_task_new (handle_reference->oid), handle);

      schedule_durable_task (context, wrapped_task);     
    }
}

gzochid_periodic_task_handle *gzochid_schedule_periodic_durable_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_application_task *task,
 gzochid_application_task_serialization *serialization, 
 struct timeval delay, struct timeval period)
{
  gzochid_durable_application_task_handle *handle = 
    build_durable_task_handle (task, serialization, delay, &period);

  if (handle != NULL)
    {
      gzochid_data_managed_reference *handle_reference = 
	gzochid_data_create_reference 
	(context, &gzochid_durable_application_task_handle_serialization, 
	 handle);
      gzochid_task *wrapped_task = wrap_durable_task
	(context, identity,
	 gzochid_durable_application_task_new (handle_reference->oid), handle);
      
      schedule_durable_task (context, wrapped_task);
    }

  return handle;
}

void gzochid_cancel_periodic_task 
(gzochid_application_context *context, gzochid_periodic_task_handle *handle)
{
  handle->repeats = FALSE;

  if (gzochid_data_mark 
      (context, &gzochid_durable_application_task_handle_serialization, 
       handle) != 0)
    
    /* Nothing to be done here. */

    ;
}
