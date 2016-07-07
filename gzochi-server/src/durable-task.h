/* durable-task.h: Prototypes and declarations for durable-task.c
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

#ifndef GZOCHID_DURABLE_TASK_H
#define GZOCHID_DURABLE_TASK_H

#include <glib.h>
#include <sys/time.h>

#include "app.h"
#include "app-task.h"
#include "gzochid-auth.h"
#include "io.h"
#include "queue.h"

struct _gzochid_application_worker_serialization
{
  void (*serializer) 
  (gzochid_application_context *, gzochid_application_worker, GString *);
  gzochid_application_worker (*deserializer) 
  (gzochid_application_context *, GString *);
};

typedef struct _gzochid_application_worker_serialization
gzochid_application_worker_serialization;

struct _gzochid_application_task_serialization
{
  char *name;
  gzochid_application_worker_serialization *worker_serialization;
  gzochid_io_serialization *data_serialization;
};

typedef struct _gzochid_application_task_serialization
gzochid_application_task_serialization;

typedef struct _gzochid_durable_application_task_handle
gzochid_durable_application_task_handle;

typedef gzochid_durable_application_task_handle gzochid_periodic_task_handle;

/* Declaration for durable task handle object serialization. */

extern gzochid_io_serialization 
gzochid_durable_application_task_handle_serialization;

/* Declarations for pre-defined durable task serializations. */

extern gzochid_application_task_serialization
gzochid_client_received_message_task_serialization;

extern gzochid_application_task_serialization
gzochid_task_chain_bootstrap_task_serialization;

/* Creates and returns a new `gzochid_durable_task_handle' instance representing
   the specified durable application task, with the specified schedule delay and
   repeat period (or `NULL' if this task should not repeat). */

gzochid_durable_application_task_handle *
gzochid_create_durable_application_task_handle
(gzochid_application_task *, gzochid_application_task_serialization *,
 struct timeval, struct timeval *, GError **);

void gzochid_schedule_durable_task
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_application_task *, gzochid_application_task_serialization *);

void gzochid_schedule_delayed_durable_task 
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_application_task *, gzochid_application_task_serialization *, 
 struct timeval);

gzochid_periodic_task_handle *gzochid_schedule_periodic_durable_task 
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_application_task *, gzochid_application_task_serialization *, 
 struct timeval, struct timeval);

/*
  Schedules the specified durable task handle within the context of the current
  transaction.

  When this function returns successfully, the serialized form of the specified
  task has been persisted to the data store within the current transaction, and
  added to the pending task journal to allow it to be resubmitted upon container
  restart.

  If the task cannot be scheduled or its binding cannot be created because the 
  current transaction is not in a healthy state, this function will set the 
  error return argument appropriately.
*/

void gzochid_schedule_durable_task_handle
(gzochid_application_context *, gzochid_durable_application_task_handle *,
 GError **err);

/*
  Schedules the specified sequence of tasks, in the form of a 
  `gzochid_durable_queue' for serial execution; each task in the queue will be 
  executed (retryably) in its own transaction, and subsequent tasks will not be
  scheduled for execution until the current task completes successfully.

  When this function returns successfully, a wrapper around the serialized form
  of the task queue has been persisted to the data store within the current 
  transaction, and added to the pending task journal to allow task execution to
  pick up where it left off upon container restart.

  If the task chain cannot be scheduled or its binding cannot be created because
  the current transaction is not in a healthy state, this function will set the 
  error return argument appropriately.

  Restrictions: 

  - The elements in the queue must be uniformly of type 
  `gzochid_durable_application_task_handle'.

  - These task handles may not be repeating. (They may, however, specify a
  pre-execution delay.)

  - The durable queue passed to this function will be modified destructively as
  tasks are removed from it for execution, and removed from the data store once
  it is empty. It should not be re-used.
*/

void gzochid_schedule_durable_task_chain
(gzochid_application_context *, gzochid_auth_identity *,
 gzochid_durable_queue *, GError **);

void gzochid_cancel_periodic_task 
(gzochid_application_context *, gzochid_periodic_task_handle *);

void gzochid_task_initialize_serialization_registry (void);

void gzochid_task_register_serialization 
(gzochid_application_task_serialization *);

void gzochid_restart_tasks (gzochid_application_context *);

#endif /* GZOCHID_DURABLE_TASK_H */
