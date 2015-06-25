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

gzochid_application_worker_serialization 
received_message_worker_serialization = { NULL, NULL };

gzochid_io_serialization 
received_message_data_serialization = { NULL, NULL, NULL };

gzochid_application_task_serialization 
gzochid_client_received_message_task_serialization = 
  { 
    "received-message",
    &received_message_worker_serialization, 
    &received_message_data_serialization 
  };

gzochid_application_task *
gzochid_application_task_new (gzochid_application_context *context,
			      gzochid_auth_identity *identity, 
			      gzochid_application_worker worker, gpointer data)
{
  gzochid_application_task *task = malloc (sizeof (gzochid_application_task));

  task->context = context;
  task->identity = identity;
  task->worker = worker;
  task->data = data;

  return task;
}

gzochid_application_task *
gzochid_deserialize_application_task 
(gzochid_application_context *context, 
 gzochid_application_task_serialization *serialization, GString *in)
{
  gzochid_auth_identity *identity = 
    gzochid_auth_identity_deserializer (context, in, NULL);
  gzochid_application_worker worker = 
    serialization->worker_serialization->deserializer (context, in);
  gpointer data = serialization->data_serialization->deserializer 
    (context, in, NULL);
  return gzochid_application_task_new (context, identity, worker, data);
}

void 
gzochid_serialize_application_task 
(gzochid_application_context *context,
 gzochid_application_task_serialization *serialization, 
 gzochid_application_task *task, GString *out)
{
  gzochid_auth_identity_serializer (context, task->identity, out, NULL);
  serialization->worker_serialization->serializer (context, task->worker, out);
  serialization->data_serialization->serializer 
    (context, task->data, out, NULL);
}

static gzochid_transactional_application_task_execution *
execution_new (gzochid_application_task *task,
	       gzochid_application_task *catch_task,
	       gzochid_application_task *cleanup_task, struct timeval *timeout)
{
  gzochid_transactional_application_task_execution *execution = 
    malloc (sizeof (gzochid_transactional_application_task_execution));

  execution->task = task;
  execution->catch_task = catch_task;
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
  gzochid_application_transaction_event *event = 
    malloc (sizeof (gzochid_application_transaction_event));
  gzochid_application_event *base_event = (gzochid_application_event *) event;
  struct timeval now;

  gettimeofday (&now, NULL);
  base_event->type = TRANSACTION_COMMIT;
  gettimeofday (&base_event->timestamp, NULL);
  timersub (&now, &tx_context->start_time, &event->duration);

  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source, base_event);
  free (tx_context);
}

static void 
event_rollback (gpointer data)
{
  gzochid_event_transaction_context *tx_context = data;
  gzochid_application_transaction_event *event = 
    malloc (sizeof (gzochid_application_transaction_event));
  gzochid_application_event *base_event = (gzochid_application_event *) event;
  struct timeval now;

  gettimeofday (&now, NULL);
  base_event->type = TRANSACTION_ROLLBACK;
  gettimeofday (&base_event->timestamp, NULL);
  timersub (&now, &tx_context->start_time, &event->duration);

  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source, base_event);
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

  gzochid_application_event *event = 
    malloc (sizeof (gzochid_application_event));

  event->type = TRANSACTION_START;
  gettimeofday (&event->timestamp, NULL);

  join_transaction (context);

  gzochid_application_event_dispatch (context->event_source, event);
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
gzochid_application_resubmitting_transactional_task_worker 
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gpointer data)
{
  struct timeval now;
  gzochid_task *task = NULL;
  gzochid_application_task *application_task = NULL;

  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gzochid_transactional_application_task_execution *execution = data;

  gzochid_application_transactional_task_worker 
    (app_context, identity, execution);
  
  if (gzochid_application_should_retry (execution))
    {	  
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
      
      application_task = gzochid_application_task_new
	(app_context, identity,
	 gzochid_application_resubmitting_transactional_task_worker,
	 catch_execution);
    }
  else if (execution->cleanup_task != NULL)
    {
      gzochid_transactional_application_task_execution *cleanup_execution = 
	gzochid_transactional_application_task_execution_new
	(execution->cleanup_task, NULL, NULL);
      
      application_task = gzochid_application_task_new
	(app_context, identity,
	 gzochid_application_resubmitting_transactional_task_worker,
	 cleanup_execution);
    }

  if (application_task != NULL)
    {
      gettimeofday (&now, NULL);
      
      task = gzochid_task_new
	(gzochid_application_task_thread_worker, application_task, now);
      
      gzochid_schedule_submit_task (game_context->task_queue, task);
    }
}

void 
gzochid_application_task_thread_worker (gpointer data, gpointer user_data)
{
  gzochid_application_task *task = data;
  task->worker (task->context, task->identity, task->data);

}

gboolean 
gzochid_application_should_retry 
(gzochid_transactional_application_task_execution *execution)
{
  return execution->result == GZOCHID_TRANSACTION_SHOULD_RETRY
    && execution->attempts < GZOCHID_APPLICATION_MAX_ATTEMPTS_DEFAULT;
}

void 
gzochid_register_client_received_message_task_serialization (void)
{
  gzochid_task_register_serialization 
    (&gzochid_client_received_message_task_serialization);
}
