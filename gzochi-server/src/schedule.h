/* schedule.h: Prototypes and declarations for schedule.c
 * Copyright (C) 2012 Julian Graham
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

enum gzochid_pending_task_state
  {
    GZOCHID_PENDING_TASK_STATE_PENDING,
    GZOCHID_PENDING_TASK_STATE_COMPLETED,
    GZOCHID_PENDING_TASK_STATE_ERROR
  };

typedef struct _gzochid_task_queue
{
  GCond *cond;
  GMutex *mutex;
  GThread *consumer_thread;

  GQueue *queue;
  GThreadPool *pool;
} gzochid_task_queue;

typedef struct _gzochid_pending_task
{
  GCond *cond;
  GMutex *mutex;
  
  struct timeval scheduled_execution_time;
  enum gzochid_pending_task_state state;

  gzochid_task *task;
} gzochid_pending_task;

gpointer gzochid_schedule_task_executor (gpointer);

gzochid_task_queue *gzochid_schedule_task_queue_new (GThreadPool *);
void gzochid_schedule_task_queue_start (gzochid_task_queue *);

gzochid_pending_task *gzochid_schedule_submit_task 
(gzochid_task_queue *, gzochid_task *);
gzochid_pending_task *gzochid_schedule_submit_task_chain 
(gzochid_task_queue *, GList *);

void gzochid_schedule_run_task (gzochid_task_queue *, gzochid_task *);
void gzochid_schedule_execute_task (gzochid_task *);

#endif /* GZOCHID_SCHEDULE_H */
