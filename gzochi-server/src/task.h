/* task.h: Prototypes and declarations for task.c
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

#ifndef GZOCHID_TASK_H
#define GZOCHID_TASK_H

#include <glib.h>
#include <gmp.h>
#include <sys/time.h>

#include "app.h"
#include "auth.h"
#include "io.h"
#include "threads.h"

typedef struct _gzochid_periodic_task_handle
{
  mpz_t descriptor_oid;
  mpz_t scm_oid;
} gzochid_periodic_task_handle;

typedef struct _gzochid_task
{
  gzochid_thread_worker worker;
  gpointer data;
  struct timeval target_execution_time;
} gzochid_task;

typedef struct _gzochid_task_transaction_context
{
  gzochid_application_context *context;
  
  gzochid_auth_identity *identity;

  GList *scheduled_tasks;
} gzochid_task_transaction_context;

gzochid_task *gzochid_task_new 
(gzochid_thread_worker, gpointer, struct timeval);
gzochid_task *gzochid_task_immediate_new (gzochid_thread_worker, gpointer);

gzochid_application_task *gzochid_application_task_new 
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_application_worker, gpointer);

gzochid_task *gzochid_task_make_transactional_application_task
(gzochid_application_context *, gzochid_auth_identity *,
 gzochid_application_worker, gpointer, struct timeval);

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

void gzochid_cancel_periodic_task (gzochid_periodic_task_handle *);

void gzochid_task_initialize_serialization_registry (void);
void gzochid_task_register_serialization 
(gzochid_application_task_serialization *);
void gzochid_restart_tasks (gzochid_application_context *);

#endif /* GZOCHID_TASK_H */
