/* app-task.c: Support for application-bound transactional task execution
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "app.h"
#include "app-task.h"
#include "auth_int.h"
#include "context.h"
#include "game.h"
#include "gzochid-auth.h"
#include "io.h"
#include "schedule.h"
#include "task.h"
#include "tx.h"

#define GZOCHID_APPLICATION_MAX_ATTEMPTS_DEFAULT 3

struct _gzochid_event_transaction_context
{
  gzochid_application_context *app_context;
  struct timeval start_time;
};

typedef struct _gzochid_event_transaction_context
gzochid_event_transaction_context;

gzochid_application_task *
gzochid_application_task_new (gzochid_application_context *context,
			      gzochid_auth_identity *identity, 
			      gzochid_application_worker worker, gpointer data)
{
  gzochid_application_task *task = malloc (sizeof (gzochid_application_task));

  task->context = context;
  task->identity = gzochid_auth_identity_ref (identity);
  task->worker = worker;
  task->data = data;

  task->ref_count = 1;
  
  return task;
}

gzochid_application_task *
gzochid_application_task_ref (gzochid_application_task *task)
{
  g_atomic_int_inc (&task->ref_count);
  return task;
}

void
gzochid_application_task_unref (gzochid_application_task *task)
{
  if (g_atomic_int_dec_and_test (&task->ref_count)) {
    gzochid_application_task_free (task);
  }
}

static gzochid_transactional_application_task_execution *
execution_new (gzochid_application_task *task,
	       gzochid_application_task *catch_task,
	       gzochid_application_task *cleanup_task, struct timeval *timeout)
{
  gzochid_transactional_application_task_execution *execution = 
    malloc (sizeof (gzochid_transactional_application_task_execution));

  assert (task != NULL);
  
  execution->task = gzochid_application_task_ref (task);
  execution->catch_task = catch_task != NULL
    ? gzochid_application_task_ref (catch_task) : NULL;
  execution->cleanup_task = cleanup_task != NULL
    ? gzochid_application_task_ref (cleanup_task) : NULL;

  if (timeout != NULL)
    {
      execution->timeout = malloc (sizeof (struct timeval));
      execution->timeout->tv_sec = timeout->tv_sec;
      execution->timeout->tv_usec = timeout->tv_usec;
    }
  else execution->timeout = NULL;

  execution->attempts = 0;
  execution->max_attempts = GZOCHID_APPLICATION_MAX_ATTEMPTS_DEFAULT;
  execution->result = GZOCHID_TRANSACTION_PENDING;

  return execution;
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_execution_new 
(gzochid_application_task *task, gzochid_application_task *catch_task,
 gzochid_application_task *cleanup_task)
{
  return execution_new (task, catch_task, cleanup_task, NULL);
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_timed_execution_new 
(gzochid_application_task *task, gzochid_application_task *catch_task,
 gzochid_application_task *cleanup_task, struct timeval timeout)
{
  return execution_new (task, catch_task, cleanup_task, &timeout);
}

void 
gzochid_transactional_application_task_execution_free
(gzochid_transactional_application_task_execution *execution)
{
  gzochid_application_task_unref (execution->task);

  if (execution->catch_task != NULL)
    gzochid_application_task_unref (execution->catch_task);
  if (execution->cleanup_task != NULL)
    gzochid_application_task_unref (execution->cleanup_task);
  
  if (execution->timeout != NULL)
    free (execution->timeout);
  free (execution);
}

static void 
transactional_task_worker (gpointer data)
{
  void **args = data;
  
  gzochid_application_context *context = args[0];
  gzochid_auth_identity *identity = args[1];
  gzochid_application_task *task = args[2];
  
  task->worker (context, identity, task->data);
}

void 
gzochid_application_transactional_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  void *args[3];
  gzochid_transactional_application_task_execution *execution = data;

  args[0] = context;
  args[1] = identity;
  args[2] = execution->task;

  if (execution->timeout != NULL)
    execution->result = gzochid_application_transaction_execute_timed 
      (context, transactional_task_worker, args, *execution->timeout);
  else execution->result = gzochid_application_transaction_execute 
	 (context, transactional_task_worker, args);

  execution->attempts++;
}

static gzochid_event_transaction_context *
create_transaction_context (gzochid_application_context *app_context)
{
  gzochid_event_transaction_context *tx_context = 
    malloc (sizeof (gzochid_event_transaction_context));

  tx_context->app_context = app_context;
  gettimeofday (&tx_context->start_time, NULL);

  return tx_context;
}

static int 
event_prepare (gpointer data)
{
  return TRUE;
}

static void 
event_commit (gpointer data)
{
  gzochid_event_transaction_context *tx_context = data;
  struct timeval now, duration;

  gettimeofday (&now, NULL);
  timersub (&now, &tx_context->start_time, &duration);

  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source,
     gzochid_application_transaction_event_new (TRANSACTION_COMMIT, duration));

  free (tx_context);
}

static void 
event_rollback (gpointer data)
{
  gzochid_event_transaction_context *tx_context = data;
  struct timeval now, duration;

  gettimeofday (&now, NULL);
  timersub (&now, &tx_context->start_time, &duration);
  
  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source,
     gzochid_application_transaction_event_new
     (TRANSACTION_ROLLBACK, duration));

  free (tx_context);
}

static gzochid_transaction_participant 
event_participant = { "event", event_prepare, event_commit, event_rollback };

static void 
join_transaction (gzochid_application_context *context)
{
  if (!gzochid_transaction_active ()
      || gzochid_transaction_context (&event_participant) == NULL)
    {
      gzochid_event_transaction_context *tx_context =
	create_transaction_context (context); 
      gzochid_transaction_join (&event_participant, tx_context);
    }
}

static void 
event_func_wrapper (gpointer data)
{
  gpointer *args = data;
  gzochid_application_context *context = args[0];
  void (*func) (gpointer) = (void (*) (gpointer)) args[1];
  gpointer func_data = args[2];

  join_transaction (context);

  gzochid_application_event_dispatch
    (context->event_source, gzochid_application_event_new (TRANSACTION_START));

  func (func_data);  
}

gzochid_transaction_result 
gzochid_application_transaction_execute (gzochid_application_context *context, 
					 void (*func) (gpointer), gpointer data)
{
  gpointer args[3];

  args[0] = context;
  args[1] = func;
  args[2] = data;

  return gzochid_transaction_execute (event_func_wrapper, args);
}

gzochid_transaction_result 
gzochid_application_transaction_execute_timed 
(gzochid_application_context *context, void (*func) (gpointer), gpointer data,
 struct timeval timeout)
{
  gpointer args[3];

  args[0] = context;
  args[1] = func;
  args[2] = data;

  return gzochid_transaction_execute_timed (event_func_wrapper, args, timeout);
}

void 
gzochid_application_reexecuting_transactional_task_worker 
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_transactional_application_task_execution *execution = data;

  do gzochid_application_transactional_task_worker 
       (app_context, identity, execution);
  while (gzochid_application_should_retry (execution));

  if (execution->result != GZOCHID_TRANSACTION_SUCCESS
      && execution->catch_task != NULL)
    {
      gzochid_transactional_application_task_execution *catch_execution = NULL;

      if (execution->timeout != NULL)
	catch_execution =
	  gzochid_transactional_application_task_timed_execution_new
	  (execution->catch_task, NULL, NULL, *execution->timeout);
      else catch_execution =
	     gzochid_transactional_application_task_execution_new
	     (execution->catch_task, NULL, NULL);

      catch_execution->max_attempts = 1;
      
      gzochid_application_reexecuting_transactional_task_worker
	(app_context, identity, catch_execution);
    }

  if (execution->cleanup_task != NULL)
    execution->cleanup_task->worker
      (app_context, identity, execution->cleanup_task->data);

  gzochid_transactional_application_task_execution_free (execution);
}

void 
gzochid_application_resubmitting_transactional_task_worker 
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gboolean resubmitting_execution = FALSE;
  gzochid_application_task *application_task = NULL;

  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gzochid_transactional_application_task_execution *execution = data;

  gzochid_application_transactional_task_worker 
    (app_context, identity, execution);
  
  if (gzochid_application_should_retry (execution))
    {
      resubmitting_execution = TRUE;
      execution->result = GZOCHID_TRANSACTION_PENDING;
      
      application_task = gzochid_application_task_new
	(app_context, identity,
	 gzochid_application_resubmitting_transactional_task_worker, 
	 execution);	  
    }
  else if (execution->result != GZOCHID_TRANSACTION_SUCCESS
	   && execution->catch_task != NULL)
    {
      gzochid_transactional_application_task_execution *catch_execution = 
	gzochid_transactional_application_task_execution_new
	(execution->catch_task, NULL, execution->cleanup_task);

      catch_execution->max_attempts = 1;
      
      application_task = gzochid_application_task_new
	(app_context, identity,
	 gzochid_application_resubmitting_transactional_task_worker,
	 catch_execution);
    }
  else if (execution->cleanup_task != NULL)
    application_task = gzochid_application_task_ref (execution->cleanup_task);

  if (!resubmitting_execution)

    /* Freeing the execution has the effect of unref'ing the primary task. */
      
    gzochid_transactional_application_task_execution_free (execution);

  if (application_task != NULL)
    {
      gzochid_task task;

      task.worker = gzochid_application_task_thread_worker;
      task.data = application_task;
      gettimeofday (&task.target_execution_time, NULL);
      
      gzochid_schedule_submit_task (game_context->task_queue, &task);
    }
}

void
gzochid_application_task_free (gzochid_application_task *task)
{
  gzochid_auth_identity_unref (task->identity);
  free (task);
}

void 
gzochid_application_task_thread_worker (gpointer data, gpointer user_data)
{
  gzochid_application_task *task = data;
  task->worker (task->context, task->identity, task->data);
  gzochid_application_task_unref (task);
}

gboolean 
gzochid_application_should_retry 
(gzochid_transactional_application_task_execution *execution)
{
  return execution->result == GZOCHID_TRANSACTION_SHOULD_RETRY
    && execution->attempts < execution->max_attempts;
}
