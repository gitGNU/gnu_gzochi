/* task.c: Application task management routines for gzochid
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

#include "task.h"

gzochid_task *
gzochid_task_new (gzochid_thread_worker worker, gpointer data, 
		  struct timeval target_execution_time)
{
  gzochid_task *task = malloc (sizeof (gzochid_task));
  
  task->worker = worker;
  task->data = data;
  task->target_execution_time = target_execution_time;
  
  return task;
}

gzochid_task *
gzochid_task_immediate_new (gzochid_thread_worker worker, gpointer data)
{
  struct timeval now;
  gettimeofday (&now, NULL);

  return gzochid_task_new (worker, data, now);
}

void 
gzochid_task_free (gzochid_task *task)
{
  free (task);
}

