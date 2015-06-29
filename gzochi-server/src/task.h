/* task.h: Prototypes and declarations for task.c
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

#ifndef GZOCHID_TASK_H
#define GZOCHID_TASK_H

#include <glib.h>
#include <sys/time.h>

#include "app.h"
#include "data.h"
#include "gzochid-auth.h"
#include "io.h"
#include "threads.h"

struct _gzochid_task
{
  gzochid_thread_worker worker;
  gpointer data;
  struct timeval target_execution_time;
};

typedef struct _gzochid_task gzochid_task;

/* Construct a new task with specified worker and data, to be executed at some
   time after the specified timeout has elapsed. */

gzochid_task *gzochid_task_new
(gzochid_thread_worker, gpointer, struct timeval);

/* Construct a new task with specified worker and data, to be executed as soon
   as possible. The `GDestroyNotify' argument gives a finalization function to 
   be called on the data argument when `gzochid_task_free' is called on the 
   task. */

gzochid_task *gzochid_task_immediate_new (gzochid_thread_worker, gpointer);

/* Free the specified task, invoking its finalizer (if one was specified at the
   time the task was constructed) on the task data. */

void gzochid_task_free (gzochid_task *);

#endif /* GZOCHID_TASK_H */
