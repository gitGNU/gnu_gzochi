/* schedule.c: Task execution and task queue management routines for gzochid
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

#include <glib.h>

#include "guile.h"
#include "schedule.h"
#include "task.h"

static void pending_task_executor (gpointer data, gpointer user_data)
{
  gzochid_pending_task *pending_task = (gzochid_pending_task *) data;

  g_mutex_lock (pending_task->mutex);
  pending_task->task->worker (pending_task->task->data, user_data);
  pending_task->state = GZOCHID_PENDING_TASK_STATE_COMPLETED;
  g_cond_broadcast (pending_task->cond);
  g_mutex_unlock (pending_task->mutex);
}

gpointer gzochid_schedule_task_executor (gpointer data)
{
  gzochid_task_queue *task_queue = (gzochid_task_queue *) data;

  g_mutex_lock (task_queue->mutex);
  while (TRUE)
    {
      if (g_queue_is_empty (task_queue->queue))
	g_cond_wait (task_queue->cond, task_queue->mutex);
      else 
	{
	  struct timeval current_time;
	  GTimeVal wait_time;
	  gzochid_pending_task *pending_task = 
	    (gzochid_pending_task *) g_queue_peek_head (task_queue->queue);

	  gettimeofday (&current_time, NULL);

	  if (current_time.tv_sec 
	      > pending_task->scheduled_execution_time.tv_sec
	      || (current_time.tv_sec 
		  == pending_task->scheduled_execution_time.tv_sec
		  && (current_time.tv_usec 
		      > pending_task->scheduled_execution_time.tv_usec)))
	    {
	      pending_task = (gzochid_pending_task *) 
		g_queue_pop_head (task_queue->queue);
	      gzochid_guile_thread_pool_push 
		(task_queue->pool, pending_task_executor, pending_task, NULL);
	    }
	  else 
	    {
	      wait_time.tv_sec = 
		pending_task->scheduled_execution_time.tv_sec 
		- current_time.tv_sec;
	      wait_time.tv_usec = 
		pending_task->scheduled_execution_time.tv_usec 
		- current_time.tv_usec;

	      g_cond_timed_wait 
		(task_queue->cond, task_queue->mutex, &wait_time);
	    }
	}
    }
  g_mutex_unlock (task_queue->mutex);

  return NULL;
}

void gzochid_schedule_task_queue_start (gzochid_task_queue *task_queue)
{
  if (task_queue->consumer_thread == NULL)
    task_queue->consumer_thread = g_thread_create 
      (gzochid_schedule_task_executor, task_queue, FALSE, NULL);
}

gzochid_pending_task *gzochid_pending_task_new (gzochid_task *task)
{
  gzochid_pending_task *pending_task = malloc (sizeof (gzochid_pending_task));

  pending_task->task = task;
  pending_task->state = GZOCHID_PENDING_TASK_STATE_PENDING;
  pending_task->cond = g_cond_new ();
  pending_task->mutex = g_mutex_new ();

  pending_task->scheduled_execution_time = task->target_execution_time;
  
  return pending_task;
}

gint pending_task_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
  gzochid_pending_task *pending_task_a = (gzochid_pending_task *) a;
  gzochid_pending_task *pending_task_b = (gzochid_pending_task *) b;

  if (pending_task_a->scheduled_execution_time.tv_sec <
      pending_task_b->scheduled_execution_time.tv_sec)
    return -1;
  else if (pending_task_a->scheduled_execution_time.tv_sec 
	   > pending_task_b->scheduled_execution_time.tv_sec)
    return 1;
  else return pending_task_a->scheduled_execution_time.tv_usec 
	 - pending_task_b->scheduled_execution_time.tv_usec;
}

gzochid_pending_task *gzochid_schedule_submit_task
(gzochid_task_queue *task_queue, gzochid_task *task)
{
  gzochid_pending_task *pending_task = gzochid_pending_task_new (task);
  
  g_mutex_lock (task_queue->mutex);
  g_queue_insert_sorted 
    (task_queue->queue, pending_task, pending_task_compare, NULL);
  g_cond_signal (task_queue->cond);
  g_mutex_unlock (task_queue->mutex);

  return pending_task;
}

void gzochid_schedule_run_task 
(gzochid_task_queue *task_queue, gzochid_task *task)
{
  gzochid_pending_task *pending_task = 
    gzochid_schedule_submit_task (task_queue, task);

  g_mutex_lock (pending_task->mutex);
  while (pending_task->state == GZOCHID_PENDING_TASK_STATE_PENDING)
    g_cond_wait (pending_task->cond, pending_task->mutex);
  g_mutex_unlock (pending_task->mutex);
  free (pending_task);
}
