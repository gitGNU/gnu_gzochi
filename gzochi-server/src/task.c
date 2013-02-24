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
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "auth.h"
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

gzochid_application_task_serialization *
gzochid_lookup_task_serialization (char *name)
{
  return (gzochid_application_task_serialization *) 
    g_hash_table_lookup (serialization_registry, name);
}

void gzochid_task_register_serialization 
(char *name, gzochid_application_task_serialization *serialization)
{
  g_hash_table_insert (serialization_registry, name, serialization);
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
  g_list_free_full (tx_context->scheduled_recurring_tasks, free);
  g_list_free_full (tx_context->canceled_recurring_tasks, free);

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

static void gzochid_transactional_application_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  void *args[3];
  
  args[0] = context;
  args[1] = identity;
  args[2] = data;

  gzochid_transaction_execute (transactional_task_worker, args);  
}

gzochid_task *gzochid_task_make_transactional_application_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_worker worker, gpointer data, 
 struct timeval target_execution_time)
{
  gzochid_application_task *transactional_task = 
    gzochid_application_task_new (context, identity, worker, data);
  gzochid_application_task *application_task = 
    gzochid_application_task_new 
    (context, identity, gzochid_transactional_application_task_worker, 
     transactional_application_task);
  
  gzochid_task *task = malloc (sizeof (gzochid_task));
  
  task->worker = gzochid_application_task_thread_worker;
  task->data = application_task;
  task->target_execution_time = target_execution_time;

  return task;
}

gzochid_durable_application_task *gzochid_durable_application_task_new 
(gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization, 
 struct timeval target_execution_time)
{
  gzochid_durable_application_task *durable_task = 
    malloc (sizeof (gzochid_durable_application_task));

  durable_task->task = task;
  durable_task->serialization = serialization;
  durable_task->target_execution_time = target_execution_time;
  durable_task->repeats = FALSE;

  mpz_init (durable_task->oid);
  
  return durable_task;
}

void gzochid_durable_application_task_free 
(gzochid_durable_application_task *task)
{
  mpz_clear (task->oid);
  free (task);
}

gpointer deserialize_durable_task
(gzochid_application_context *context, GString *in)
{
  char *serialization_name = gzochid_util_deserialize_string (in);
  gzochid_application_task_serialization *serialization = 
    gzochid_lookup_task_serialization (serialization_name);
  gzochid_application_task *task =
    gzochid_deserialize_application_task (context, serialization, in);
  struct timeval target_execution_time = gzochid_util_deserialize_timeval (in);

  return gzochid_durable_application_task_new 
    (task, serialization, target_execution_time);
}

void serialize_durable_task 
(gzochid_application_context *context, gpointer data, GString *out)
{
  gzochid_durable_application_task *task = 
    (gzochid_durable_application_task *) data;

  gzochid_util_serialize_string (task->serialization->name, out);  
  gzochid_serialize_application_task 
    (context, task->serialization, task->task, out);
  gzochid_util_serialize_timeval (task->target_execution_time, out);
}

void finalize_durable_task 
(gzochid_application_context *context, gpointer data)
{
  gzochid_durable_application_task_free 
    ((gzochid_durable_application_task *) data);
}

gzochid_io_serialization durable_task_serialization = 
  { serialize_durable_task, deserialize_durable_task, finalize_durable_task };

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

static gzochid_task *rebuild_durable_task 
(gzochid_application_context *context, mpz_t oid)
{
  gzochid_data_managed_reference *task_reference = 
    gzochid_data_create_reference_to_oid 
    (context, &durable_task_serialization, oid);

  gzochid_durable_application_task *task = NULL;
  gzochid_application_task *transactional_task = NULL;
  gzochid_transactional_application_task_execution *execution = NULL;
  gzochid_application_task *application_task = NULL;

  gzochid_data_dereference (task_reference);

  task = (gzochid_durable_application_task *) task_reference->obj;
  transactional_task = gzochid_transactional_application_task_new 
    (durable_task_application_worker, task);
  application_task = gzochid_application_task_new
    (context, task->task->identity, 
     gzochid_transactional_application_task_worker, transactional_task);

  mpz_set (task->oid, oid);

  return gzochid_task_new 
    (gzochid_application_task_thread_worker, application_task, 
     task->target_execution_time);
}

void gzochid_restart_tasks (gzochid_application_context *context)
{
  mpz_t oid;
  char *next_binding = NULL;
  int prefix_len = strlen (PENDING_TASK_PREFIX);
  gzochid_game_context *game_context = (gzochid_game_context *)
    ((gzochid_context *) context)->parent;
  int num_tasks = 0;

  mpz_init (oid);
  gzochid_tx_info (context, "Resubmitting durable tasks.");
  next_binding = gzochid_data_next_binding_oid 
    (context, PENDING_TASK_PREFIX, oid);

  while (next_binding != NULL 
	 && strncmp (PENDING_TASK_PREFIX, next_binding, prefix_len) == 0)
    {
      char *next_next_binding = 
	gzochid_data_next_binding_oid (context, next_binding, oid);
      gzochid_task *task = rebuild_durable_task (context, oid);
      
      gzochid_schedule_submit_task (game_context->task_queue, task);

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

gzochid_task *build_durable_task 
(gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization)
{
  struct timeval now;

  gettimeofday (&now, NULL);

  {
    gzochid_durable_application_task *durable_task = 
      gzochid_durable_application_task_new (task, serialization, now);
    gzochid_data_managed_reference *reference = gzochid_data_create_reference 
      (task->context, &durable_task_serialization, durable_task);
    
    char *oid_str = mpz_get_str (NULL, 16, reference->oid);
    int oid_str_len = strlen (oid_str), 
      prefix_len = strlen (PENDING_TASK_PREFIX);
    char *binding_name = calloc (oid_str_len + prefix_len + 1, sizeof (char));
 
    gzochid_application_task *transactional_task = gzochid_application_task_new
      (task->context, task->identity, durable_task_application_worker, 
       durable_task);
    gzochid_transactional_application_task_execution *execution = 
    gzochid_application_task *application_task = gzochid_application_task_new
      (task->context, task->identity, 
       gzochid_transactional_application_task_worker, transactional_task);
    
    gzochid_task *ret = gzochid_task_new 
      (gzochid_application_task_thread_worker, application_task, now);

    mpz_set (durable_task->oid, reference->oid);
    binding_name = strncat (binding_name, PENDING_TASK_PREFIX, prefix_len);
    binding_name = strncat (binding_name, oid_str, oid_str_len);
    gzochid_data_set_binding_to_oid 
      (task->context, binding_name, reference->oid);

    free (oid_str);
    free (binding_name);

    return ret;
  }
}

static void durable_task_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_durable_application_task *task = 
    (gzochid_durable_application_task *) data;
  gzochid_application_task *inner_task = task->task;
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;
  
  inner_task->worker (context, identity, inner_task->data);

  if (task->repeats) 
    gzochid_schedule_submit_task 
      (game_context->task_queue, 
       build_durable_task (inner_task, task->serialization));
  else 
    {
      char *oid_str = mpz_get_str (NULL, 16, task->oid);
      GString *binding = g_string_new (PENDING_TASK_PREFIX);
      
      g_string_append (binding, oid_str);

      gzochid_data_remove_binding (context, binding->str);

      g_string_free (binding, FALSE);
      free (oid_str);
    }  
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

void gzochid_task_free (gzochid_task *task)
{
  free (task);
}

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

void gzochid_schedule_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization)
{
  gzochid_schedule_delayed_durable_task 
    (context, identity, task, serialization, 0);
}

void gzochid_schedule_delayed_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_task *task, 
 gzochid_application_task_serialization *serialization, 
 unsigned long delay)
{
  gzochid_task *wrapped_task = build_durable_task (task, serialization);
  gzochid_task_transaction_context *tx_context = NULL;
  
  join_transaction (context);
  tx_context = (gzochid_task_transaction_context *) 
    gzochid_transaction_context (&task_participant);

  tx_context->scheduled_tasks = g_list_append 
    (tx_context->scheduled_tasks, wrapped_task);
}
