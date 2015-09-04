/* schedule.h: Prototypes and declarations for schedule.c
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

#ifndef GZOCHID_SCHEDULE_H
#define GZOCHID_SCHEDULE_H

#include <glib.h>

#include "task.h"

typedef struct _gzochid_task_queue gzochid_task_queue;

gpointer gzochid_schedule_task_executor (gpointer);

gzochid_task_queue *gzochid_schedule_task_queue_new (GThreadPool *);
void gzochid_schedule_task_queue_start (gzochid_task_queue *);

/* Submits the specified task for execution in the specified task queue and 
   returns immediately. The task will be executed no earlier than its configured
   execution time. */

void gzochid_schedule_submit_task (gzochid_task_queue *, gzochid_task *);

/* Submits the specified list of tasks for execution in the specified task 
   queue and returns immediately. Each task is submitted in the order specified
   in the queue once the previous task has executed; each task will be executed
   no earlier than its configured execution time. 

   The task execution code operates on a shallow copy of the specified 
   `GList'. */

void gzochid_schedule_submit_task_chain (gzochid_task_queue *, GList *);

/* Submits the specified tasks for execution in the specified task queue and 
   waits until it has been executed before returning. The task will be executed
   no earlier than its configured execution time. */

void gzochid_schedule_run_task (gzochid_task_queue *, gzochid_task *);

/* Synchronously executes the specified task in the calling thread. */

void gzochid_schedule_execute_task (gzochid_task *);

#endif /* GZOCHID_SCHEDULE_H */
