/* app-task.h: Prototypes and declarations for app-task.c
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

#ifndef GZOCHID_APP_TASK_H
#define GZOCHID_APP_TASK_H

#include <glib.h>
#include <sys/time.h>

#include "app.h"
#include "gzochid-auth.h"
#include "io.h"
#include "tx.h"

typedef void (*gzochid_application_worker) 
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

struct _gzochid_application_task
{
  gzochid_application_worker worker;
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  gpointer data;
};

typedef struct _gzochid_application_task gzochid_application_task;

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

struct _gzochid_transactional_application_task_execution
{
  /* The main task to execute. */
  
  gzochid_application_task *task;

  /* An optional "catch" task to be executed if and only if the main task's
     transaction is marked for rollback and cannot be retried. This task is
     executed in a separate transaction. */
  
  gzochid_application_task *catch_task;

  /* An optional "cleanup" task that is always executed after the main task's
     final attempt - and after the catch task, if one is present. This task is
     executed in a separate transaction. */
  
  gzochid_application_task *cleanup_task;

  struct timeval *timeout;
  unsigned int attempts;
  gzochid_transaction_result result;
};

typedef struct _gzochid_transactional_application_task_execution
gzochid_transactional_application_task_execution;

gzochid_transaction_result gzochid_application_transaction_execute 
(gzochid_application_context *, void (*) (gpointer), gpointer);

gzochid_transaction_result gzochid_application_transaction_execute_timed 
(gzochid_application_context *, void (*) (gpointer), gpointer, struct timeval);

gzochid_application_task *gzochid_application_task_new 
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_application_worker, gpointer);

/* Create a new transactional application task execution context with the
   specified main task and optional catch and cleanup tasks. */

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_execution_new 
(gzochid_application_task *, gzochid_application_task *,
 gzochid_application_task *);

/* Create a new transactional application task execution context with the
   specified main task, optional catch and cleanup tasks, a timeval giving an
   upper bound on the main task's allowed execution time. */

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_timed_execution_new 
(gzochid_application_task *, gzochid_application_task *,
 gzochid_application_task *, struct timeval);

void gzochid_transactional_application_task_execution_free
(gzochid_transactional_application_task_execution *);

gzochid_application_task *gzochid_deserialize_application_task 
(gzochid_application_context *, gzochid_application_task_serialization *, 
 GString *);

void gzochid_serialize_application_task 
(gzochid_application_context *, gzochid_application_task_serialization *, 
 gzochid_application_task *, GString *);

void gzochid_application_transactional_task_worker 
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_application_resubmitting_transactional_task_worker 
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

void gzochid_application_task_worker (gpointer);
void gzochid_application_task_thread_worker (gpointer, gpointer);

gboolean gzochid_application_should_retry 
(gzochid_transactional_application_task_execution *);

void gzochid_register_client_received_message_task_serialization (void);

#endif /* GZOCHID_APP_TASK_H */
