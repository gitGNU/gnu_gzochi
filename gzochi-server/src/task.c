/* task.c: Application task management routines for gzochid
 * Copyright (C) 2011 Julian Graham
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
#include "guile.h"
#include "io.h"
#include "scheme.h"
#include "task.h"
#include "tx.h"

#define PENDING_TASK_PREFIX "s.pendingTask."
#define DELAYED_TASK_PREFIX "s.delayedTask."

extern gzochid_task_serialization gzochid_client_logged_in_task_serialization;
extern gzochid_task_serialization 
gzochid_client_received_message_task_serialization;
extern gzochid_task_serialization 
gzochid_client_disconnected_task_serialization;

GHashTable *serializer_registry = NULL;
GHashTable *deserializer_registry = NULL;

static void registry_serializer 
(gzochid_application_context *context, void *ptr, GString *out)
{
  char *serialization_name = g_hash_table_lookup (serializer_registry, ptr);
  
  assert (serialization_name != NULL);
  g_string_append_len
    (out, serialization_name, strlen (serialization_name) + 1);
}

static void *registry_deserializer 
(gzochid_application_context *context, GString *in)
{
  char *serialization_name = strndup (in->str, in->len);
  void *ptr = g_hash_table_lookup (deserializer_registry, serialization_name);

  assert (ptr != NULL);
  g_string_erase (in, 0, strlen (serialization_name) + 1);
  free (serialization_name);

  return ptr;
}

gzochid_io_serialization gzochid_task_serialization_registry_serialization =
  { registry_serializer, registry_deserializer };

void gzochid_task_register_serialization 
(char *name, gzochid_task_serialization *serialization)
{
  g_hash_table_insert (serializer_registry, serialization, name);
  g_hash_table_insert (deserializer_registry, name, serialization);
}

void gzochid_task_initialize_serialization_registry (void)
{
  serializer_registry = g_hash_table_new (g_direct_hash, g_direct_equal);
  deserializer_registry = g_hash_table_new (g_str_hash, g_str_equal);

  gzochid_task_register_serialization 
    ("scheme", &gzochid_scheme_task_serialization);
  gzochid_task_register_serialization
    ("logged-in", &gzochid_client_logged_in_task_serialization);
  gzochid_task_register_serialization
    ("received-message", &gzochid_client_received_message_task_serialization);
  gzochid_task_register_serialization
    ("disconnected", &gzochid_client_disconnected_task_serialization);
}

typedef struct _gzochid_pending_task_context 
{
  gzochid_application_worker application_worker;
  gzochid_pending_task *pending_task;
} gzochid_pending_task_context;

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

typedef struct _task_wrapper
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  gzochid_task *task;
} task_wrapper;

static void task_wrapper_worker (gpointer data)
{
  task_wrapper *wrapper = (task_wrapper *) data;
  wrapper->task->worker 
    (wrapper->context, wrapper->identity, wrapper->task->data);
}

static void transactional_task_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gpointer data)
{
  gzochid_task *task = (gzochid_task *) data;
  task_wrapper wrapper;

  wrapper.context = context;
  wrapper.identity = identity;
  wrapper.task = task;

  gzochid_transaction_execute (task_wrapper_worker, &wrapper);
}

static void task_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_task *task = (gzochid_task *) data;
  task->worker (context, identity, task->data);
}

static void pending_durable_task_application_worker (gpointer data)
{
  gzochid_pending_task *pending_task = (gzochid_pending_task *) data;

  gzochid_data_dereference (pending_task->reference);
  pending_task->descriptor = (gzochid_task_descriptor *) 
    pending_task->reference->obj;
 
  task_application_worker 
    (pending_task->descriptor->context, 
     pending_task->descriptor->identity, 
     pending_task->descriptor->task);
}

static void pending_task_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_pending_task *pending_task = data;

  g_mutex_lock (pending_task->mutex);
  if (pending_task->descriptor != NULL)
    {
      assert (pending_task->reference == NULL);

      if (pending_task->descriptor->transactional 
	  && !gzochid_transaction_active())
	transactional_task_application_worker 
	  (context, identity, pending_task->descriptor->task);
      else task_application_worker 
	     (context, identity, pending_task->descriptor->task);
    }
  else
    {
      assert (pending_task->reference != NULL);
      gzochid_transaction_execute 
	(pending_durable_task_application_worker, pending_task);
    }

  pending_task->state = COMPLETED;
  g_cond_broadcast (pending_task->cond);
  g_mutex_unlock (pending_task->mutex);
}

static void submit_pending_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_pending_task *pending_task)
{
  gzochid_application_work_unit *unit = gzochid_application_work_unit_new 
    (pending_task_application_worker, pending_task);

  gzochid_application_schedule_work_unit (context, identity, unit);
}

static void commit_scheduled_task (gpointer data, gpointer user_data)
{
  gzochid_task_transaction_context *tx_context = 
    (gzochid_task_transaction_context *) user_data;
  gzochid_pending_task *task = (gzochid_pending_task *) data;
 
  submit_pending_task (tx_context->context, tx_context->identity, task);  
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

gzochid_task *gzochid_task_new 
(gzochid_application_worker worker, gpointer data)
{
  gzochid_task *task = calloc (1, sizeof (gzochid_task));

  task->worker = worker;
  task->data = data;

  return task;
}

void gzochid_task_free (gzochid_task *task)
{
  free (task);
}

gzochid_task_descriptor *gzochid_task_descriptor_new (void)
{
  gzochid_task_descriptor *descriptor = 
    calloc (1, sizeof (gzochid_task_descriptor));

  return descriptor;
}

void gzochid_task_descriptor_free (gzochid_task_descriptor *descriptor)
{
  free (descriptor);
}

static void serialize_task 
(gzochid_application_context *context, gzochid_task *task, 
 gzochid_task_serialization *serialization, GString *out)
{
  serialization->worker_serialization->serializer (context, task->worker, out);
  serialization->data_serialization->serializer (context, task->data, out);
}

static gzochid_task *deserialize_task
(gzochid_application_context *context, 
 gzochid_task_serialization *serialization, GString *in)
{
  gzochid_application_worker worker = 
    serialization->worker_serialization->deserializer (context, in);
  gpointer data = serialization->data_serialization->deserializer (context, in);
  return gzochid_task_new (worker, data);
}

static void task_descriptor_serializer 
(gzochid_application_context *context, gpointer ptr, GString *out)
{
  gzochid_task_descriptor *descriptor = (gzochid_task_descriptor *) ptr;

  gzochid_auth_identity_serializer (context, descriptor->identity, out);
  gzochid_task_serialization_registry_serialization.serializer 
    (context, descriptor->serialization, out);
  serialize_task (context, descriptor->task, descriptor->serialization, out);
  g_string_append_c (out, descriptor->transactional ? 0x01 : 0x00);
}

static void *task_descriptor_deserializer 
(gzochid_application_context *context, GString *in)
{
  gzochid_task_descriptor *descriptor = gzochid_task_descriptor_new ();
  gzochid_task_serialization *serialization = NULL;

  descriptor->context = context;
  descriptor->identity = (gzochid_auth_identity *)
    gzochid_auth_identity_deserializer (context, in);

  serialization = 
    gzochid_task_serialization_registry_serialization.deserializer 
    (context, in);
  descriptor->task = deserialize_task (context, serialization, in);
  descriptor->serialization = serialization;

  descriptor->durable = TRUE;
  descriptor->transactional = in->str[0] != 0;
  g_string_erase (in, 0, 1);
  
  return descriptor;
}

gzochid_io_serialization task_descriptor_serialization =
  { task_descriptor_serializer, task_descriptor_deserializer };

static void delayed_task_descriptor_serializer 
(gzochid_application_context *context, gpointer ptr, GString *out)
{
}

static void *delayed_task_descriptor_deserializer 
(gzochid_application_context *context, GString *in)
{
  return NULL;
}

gzochid_io_serialization delayed_task_descriptor_serialization =
  { delayed_task_descriptor_serializer, delayed_task_descriptor_deserializer };

static void periodic_task_descriptor_serializer 
(gzochid_application_context *context, gpointer ptr, GString *out)
{
}

static void *periodic_task_descriptor_deserializer 
(gzochid_application_context *context, GString *in)
{
  return NULL;
}

gzochid_io_serialization periodic_task_descriptor_serialization =
  { periodic_task_descriptor_serializer, 
    periodic_task_descriptor_deserializer };

gzochid_task_descriptor *gzochid_task_get 
(gzochid_application_context *context, char *name)
{
  return gzochid_data_get_binding 
    (context, name, &task_descriptor_serialization);
}

static gzochid_pending_task *gzochid_pending_task_new (void)
{
  gzochid_pending_task *task = calloc (1, sizeof (gzochid_pending_task));

  task->mutex = g_mutex_new ();
  task->cond = g_cond_new ();

  return task;
}

static void gzochid_pending_task_free (gzochid_pending_task *task)
{
  free (task);
}

gzochid_task_descriptor *create_task_descriptor
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_task *task, gboolean transactional)
{
  gzochid_task_descriptor *descriptor = gzochid_task_descriptor_new ();
  
  descriptor->context = context;
  descriptor->identity = identity;
  descriptor->task = task;
  descriptor->transactional = transactional;

  return descriptor;
}

gzochid_task_descriptor *create_durable_task_descriptor
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_task *task, gzochid_task_serialization *serialization, 
 unsigned long delay)
{
  gzochid_task_descriptor *descriptor = gzochid_task_descriptor_new ();
  
  descriptor->context = context;
  descriptor->identity = identity;
  descriptor->task = task;
  descriptor->delay = delay;
  descriptor->durable = TRUE;
  descriptor->transactional = TRUE;
  descriptor->serialization = serialization;

  return descriptor;
}

void gzochid_run_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_task *task, gboolean transactional)
{
  if (transactional)
    {
      if (gzochid_transaction_active ())
	{
	  gzochid_task_descriptor *descriptor = create_task_descriptor
	    (context, identity, task, TRUE);
	  gzochid_pending_task *pending_task = gzochid_pending_task_new ();
	  
	  pending_task->descriptor = descriptor;
	  g_mutex_lock (pending_task->mutex);
	  submit_pending_task (context, identity, pending_task);
	  g_cond_wait (pending_task->cond, pending_task->mutex);
	  g_mutex_unlock (pending_task->mutex);

	  gzochid_task_descriptor_free (pending_task->descriptor);
	  gzochid_pending_task_free (pending_task);
	}
      else transactional_task_application_worker (context, identity, task);
    }
  else task_application_worker (context, identity, task);
}

static void create_task_reference (gpointer data)
{
  gzochid_data_managed_reference_holder *holder = 
    (gzochid_data_managed_reference_holder *) data;

  holder->reference = gzochid_data_create_reference 
    (holder->context, holder->serialization, holder->data);
}

struct persistent_task_holder
{
  gzochid_task_descriptor *descriptor;
  mpz_t oid;
};

static void persist_task_descriptor
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  struct persistent_task_holder *task_holder = 
    (struct persistent_task_holder *) data;
  gzochid_data_managed_reference *reference = NULL;

  if (gzochid_transaction_active ())    
    reference = gzochid_data_create_reference 
      (context, &task_descriptor_serialization, task_holder->descriptor);
  else 
    {
      gzochid_data_managed_reference_holder ref_holder;

      ref_holder.context = context;     
      ref_holder.data = task_holder->descriptor;
      ref_holder.reference = NULL;
      ref_holder.serialization = &task_descriptor_serialization;

      gzochid_transaction_execute (create_task_reference, &ref_holder);
      reference = ref_holder.reference;
    }

  mpz_set (task_holder->oid, reference->oid);
}

static gzochid_data_managed_reference *persist_task_sync
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_task *task, gzochid_task_serialization *serialization,
 unsigned long delay)
{
  gzochid_data_managed_reference *reference = NULL;
  gzochid_task_descriptor *descriptor = create_durable_task_descriptor
    (context, identity, task, serialization, delay);
  struct persistent_task_holder holder;

  holder.descriptor = descriptor;
  mpz_init (holder.oid);

  if (gzochid_transaction_active ())
    {      
      gzochid_application_work_unit *unit = 
	gzochid_application_work_unit_new (persist_task_descriptor, &holder);

      g_mutex_lock (unit->lock);
      gzochid_application_schedule_work_unit (context, identity, unit);
      g_cond_wait (unit->cond, unit->lock);
      g_mutex_unlock (unit->lock);

      gzochid_application_work_unit_free (unit);
    }
  else persist_task_descriptor (context, identity, &holder);

  reference = gzochid_data_create_reference_to_oid 
    (context, &task_descriptor_serialization, holder.oid);
  mpz_clear (holder.oid);
  return reference;
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

void gzochid_run_durable_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_task *task, gzochid_task_serialization *serialization)
{
  gzochid_data_managed_reference *reference = persist_task_sync
    (context, identity, task, serialization, 0);
  gzochid_pending_task *pending_task = gzochid_pending_task_new ();

  pending_task->reference = reference;

  g_mutex_lock (pending_task->mutex);
  submit_pending_task (context, identity, pending_task);
  g_cond_wait (pending_task->cond, pending_task->mutex);
  g_mutex_unlock (pending_task->mutex);
  
  gzochid_pending_task_free (pending_task);
}

typedef struct _transactional_task_work
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  void (*worker) (gzochid_application_context *, 
		  gzochid_auth_identity *, 
		  gpointer);
  gpointer data;
} transactional_task_work;

static void schedule_task_reference
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 mpz_t oid)
{
  gzochid_pending_task *pending_task = gzochid_pending_task_new ();
  gzochid_task_transaction_context *tx_context = join_transaction (context);
  
  pending_task->reference = gzochid_data_create_reference_to_oid 
    (context, &task_descriptor_serialization, oid);
  
  tx_context = (gzochid_task_transaction_context *) 
    gzochid_transaction_context (&task_participant);
  tx_context->scheduled_tasks = g_list_append 
    (tx_context->scheduled_tasks, pending_task);
}

static void execute_transactional_task_work (gpointer data)
{
  transactional_task_work *work = (transactional_task_work *) data;

  work->worker (work->context, work->identity, work->data);
}

static void schedule_durable_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 mpz_t oid)
{
  if (gzochid_transaction_active ())
    schedule_task_reference (context, identity, oid);
  else 
    {
      transactional_task_work work;

      work.context = context;
      work.identity = identity;
      work.worker = (void (*) (gzochid_application_context *, 
			       gzochid_auth_identity *, 
			       gpointer)) schedule_task_reference;
      work.data = oid;

      gzochid_transaction_execute (execute_transactional_task_work, &work);
    }
}

void gzochid_schedule_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_task *task, gzochid_task_serialization *serialization)
{
  gzochid_schedule_delayed_durable_task 
    (context, identity, task, serialization, 0);
}

void gzochid_schedule_delayed_durable_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_task *task, gzochid_task_serialization *serialization, 
 unsigned long delay)
{
  gzochid_task_descriptor *descriptor = create_durable_task_descriptor
    (context, identity, task, serialization, delay);

  struct persistent_task_holder holder;

  holder.descriptor = descriptor;
  mpz_init (holder.oid);

  persist_task_descriptor (context, identity, &holder);
  schedule_durable_task (context, identity, holder.oid);

  mpz_clear (holder.oid);
}
