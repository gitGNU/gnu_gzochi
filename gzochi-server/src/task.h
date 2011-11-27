/* task.h: Prototypes and declarations for task.c
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

#ifndef GZOCHID_TASK_H
#define GZOCHID_TASK_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "auth.h"
#include "data.h"
#include "io.h"

enum gzochid_pending_task_state
  {
    PENDING,
    COMPLETED,
    ERROR
  };

typedef struct _gzochid_task_serialization
{
  gzochid_data_worker_serialization *worker_serialization;
  gzochid_io_serialization *data_serialization;
} gzochid_task_serialization;

typedef struct _gzochid_periodic_task_handle
{
  mpz_t descriptor_oid;
  mpz_t scm_oid;
} gzochid_periodic_task_handle;

typedef struct _gzochid_task
{
  gzochid_application_worker worker;
  gpointer data;
} gzochid_task;

typedef struct _gzochid_task_descriptor
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  gzochid_task *task;

  gboolean durable;
  gboolean transactional;
  gboolean repeats;
  unsigned long delay;

  gzochid_task_serialization *serialization;
} gzochid_task_descriptor;

typedef struct _gzochid_periodic_task_descriptor
{
  gzochid_task_descriptor base;
  unsigned long period;
  mpz_t handle_oid;
} gzochid_periodic_task_descriptor;

typedef struct _gzochid_pending_task
{
  GCond *cond;
  GMutex *mutex;
  
  enum gzochid_pending_task_state state;

  gzochid_task_descriptor *descriptor;
  gzochid_data_managed_reference *reference;
} gzochid_pending_task;

typedef struct _gzochid_task_transaction_context
{
  gzochid_application_context *context;
  
  gzochid_auth_identity *identity;

  GList *scheduled_tasks;
  GList *scheduled_recurring_tasks;
  GList *canceled_recurring_tasks;
} gzochid_task_transaction_context;

gzochid_task_descriptor *gzochid_task_get 
(gzochid_application_context *, char *);

void gzochid_run_task
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gboolean);

void gzochid_run_durable_task
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *,
 gzochid_task_serialization *);

void gzochid_schedule_task
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gboolean);

void gzochid_schedule_durable_task
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gzochid_task_serialization *);

void gzochid_schedule_delayed_task 
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gboolean, unsigned long);

void gzochid_schedule_delayed_durable_task 
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gzochid_task_serialization *, unsigned long);

gzochid_periodic_task_handle *gzochid_schedule_periodic_task 
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gboolean, unsigned long, unsigned long);

gzochid_periodic_task_handle *gzochid_schedule_periodic_durable_task 
(gzochid_application_context *, gzochid_auth_identity *, gzochid_task *, 
 gzochid_task_serialization *, unsigned long, unsigned long);

void gzochid_cancel_periodic_task (gzochid_periodic_task_handle *);

void gzochid_task_initialize_serialization_registry (void);
void gzochid_task_register_serialization (char *, gzochid_task_serialization *);
void gzochid_restart_all_tasks (gzochid_application_context *);
void gzochid_restart_tasks 
(gzochid_application_context *, gzochid_auth_identity *);

#endif /* GZOCHID_TASK_H */
