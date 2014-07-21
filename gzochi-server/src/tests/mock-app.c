/* mock-app.c: Test-time replacements for app.c routines.
 * Copyright (C) 2014 Julian Graham
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
#include <stddef.h>
#include <stdlib.h>

#include "../app.h"
#include "../auth.h"
#include "../event.h"
#include "../storage.h"
#include "../tx.h"

static GPrivate thread_application_context_key;

gzochid_application_context *gzochid_application_context_new ()
{
  gzochid_application_context *context = 
    calloc (1, sizeof (gzochid_application_context));

  g_mutex_init (&context->free_oids_lock);
  g_mutex_init (&context->client_mapping_lock);

  context->storage_context = gzochid_storage_initialize ("/dev/null?");
  context->meta = gzochid_storage_open 
    (context->storage_context, "/dev/null", 0);
  context->oids = gzochid_storage_open 
    (context->storage_context, "/dev/null", 0);
  context->names = gzochid_storage_open 
    (context->storage_context, "/dev/null", 0);

  context->event_source = gzochid_application_event_source_new ();

  return context;
}

void gzochid_application_task_worker (gpointer data)
{
  gzochid_application_task *task = (gzochid_application_task *) data;
  task->worker (task->context, task->identity, task->data);
}

void gzochid_application_task_thread_worker (gpointer data, gpointer user_data)
{
  gzochid_application_task_worker (data);
}

static gzochid_transactional_application_task_execution *execution_new
(gzochid_application_task *task, gzochid_application_task *cleanup_task, 
 struct timeval *timeout)
{
  gzochid_transactional_application_task_execution *execution = 
    malloc (sizeof (gzochid_transactional_application_task_execution));

  execution->task = task;
  execution->cleanup_task = cleanup_task;

  if (timeout != NULL)
    {
      execution->timeout = malloc (sizeof (struct timeval));
      execution->timeout->tv_sec = timeout->tv_sec;
      execution->timeout->tv_usec = timeout->tv_usec;
    }
  else execution->timeout = NULL;

  execution->attempts = 0;
  execution->result = GZOCHID_TRANSACTION_PENDING;

  return execution;
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_execution_new 
(gzochid_application_task *task, gzochid_application_task *cleanup_task)
{
  return execution_new (task, cleanup_task, NULL);
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_timed_execution_new 
(gzochid_application_task *task, gzochid_application_task *cleanup_task, 
 struct timeval timeout)
{
  return execution_new (task, cleanup_task, &timeout);
}

void gzochid_serialize_application_task 
(gzochid_application_context *context, 
 gzochid_application_task_serialization *serialization, 
 gzochid_application_task *task, GString *out)
{
}

gzochid_application_task *gzochid_deserialize_application_task 
(gzochid_application_context *context, 
 gzochid_application_task_serialization *serialization, GString *in)
{
  return NULL;
}

void gzochid_transactional_application_task_execution_free
(gzochid_transactional_application_task_execution *execution)
{
}

static void transactional_task_worker (gpointer data)
{
  void **args = (void **) data;
  
  gzochid_application_context *context = 
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];
  gzochid_application_task *task = (gzochid_application_task *) args[2];
  
  task->worker (context, identity, task->data);
}

void gzochid_application_transactional_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  void *args[3];
  gzochid_transactional_application_task_execution *execution = 
    (gzochid_transactional_application_task_execution *) data;

  args[0] = context;
  args[1] = identity;
  args[2] = execution->task;

  if (execution->timeout != NULL)
    execution->result = gzochid_transaction_execute_timed 
      (transactional_task_worker, args, *execution->timeout);
  else execution->result = 
         gzochid_transaction_execute (transactional_task_worker, args);

  execution->attempts++;
}

void gzochid_application_resubmitting_transactional_task_worker 
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_transactional_application_task_execution *execution = 
    (gzochid_transactional_application_task_execution *) data;
  
  gzochid_application_transactional_task_worker 
    (app_context, identity, execution);
}

gzochid_transaction_result gzochid_application_transaction_execute 
(gzochid_application_context *context, void (*fn) (gpointer), gpointer data)
{
  gzochid_transaction_execute (fn, data);
}

gzochid_transaction_result gzochid_application_transaction_execute_timed 
(gzochid_application_context *context, void (*fn) (gpointer), gpointer data, 
 struct timeval timeout)
{
  gzochid_transaction_execute_timed (fn, data, timeout);
}

gzochid_application_callback *gzochid_application_callback_new
(char *procedure, GList *module, mpz_t scm_oid)
{
  gzochid_application_callback *callback = calloc
    (1, sizeof (gzochid_application_callback));

  callback->module = module;
  callback->procedure = procedure;

  mpz_init (callback->scm_oid);
  mpz_set (callback->scm_oid, scm_oid);

  return callback;
}

gzochid_application_context *
gzochid_get_current_application_context ()
{
  return (gzochid_application_context *)
    g_private_get (&thread_application_context_key);
}

void *
gzochid_with_application_context
(gzochid_application_context *context, gzochid_auth_identity *auth,
 void *(*worker) (gpointer), gpointer data)
{
  worker (data);
}
