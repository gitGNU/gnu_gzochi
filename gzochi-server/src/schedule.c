/* schedule.c: Task execution and task queue management routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <libguile.h>

#include "schedule.h"
#include "task.h"

/* The enumeration of possible states for submitted tasks. */

enum gzochid_pending_task_state
  {
    GZOCHID_PENDING_TASK_STATE_PENDING,
    GZOCHID_PENDING_TASK_STATE_COMPLETED
  };

/* The task queue structure. */

struct _gzochid_task_queue
{
  /* A condition variable to signal when the contents of the queue have been 
     modified. */
  
  GCond cond; 
  GMutex mutex; /* The accompanying mutex. */

  GThread *consumer_thread; /* The pool feeder thread. */

  /* Indicates whether or not the consumer thread should continue. */
  
  gboolean running; 

  GQueue *queue; /* The task queue. */
  GThreadPool *pool; /* The pool to which ready tasks are fed. */
};

/* The pending task structure. Represents the status of a task submitted to a
   task execution queue. */

struct _gzochid_pending_task
{
  GCond cond; /* A condition variable to signal when the task state changes. */
  GMutex mutex; /* The accompanying mutex. */

  /* This task's scheduled execution time. Task execution is "best effort" with
     respect to this time; this value is used to order the task queue such that
     the task will never execute before this timestamp. */
  
  struct timeval scheduled_execution_time;
  enum gzochid_pending_task_state state; /* The task state. */

  gzochid_task *task; /* The task to be executed. */

  /* Whether to free this structure on task completion or to allow some other 
     party - e.g., `gzochid_schedule_run_task' - to handle destruction. */

  gboolean destroy_on_execute; 
};

typedef struct _gzochid_pending_task gzochid_pending_task;

gzochid_task_queue *
gzochid_schedule_task_queue_new (GThreadPool *pool)
{
  gzochid_task_queue *task_queue = malloc (sizeof (gzochid_task_queue));
  
  g_cond_init (&task_queue->cond);
  g_mutex_init (&task_queue->mutex);
  task_queue->queue = g_queue_new ();

  task_queue->pool = pool;
  task_queue->consumer_thread = NULL;

  return task_queue;
}

static void *
pending_task_executor_inner (void *data)
{
  void **args = data;
  gzochid_pending_task *pending_task = args[0];
  gpointer user_data = args[1];

  pending_task->task->worker (pending_task->task->data, user_data);

  return NULL;
}

static void 
free_pending_task (gzochid_pending_task *pending_task)
{
  g_mutex_clear (&pending_task->mutex);
  g_cond_clear (&pending_task->cond);
  free (pending_task);
}

static void 
pending_task_executor (gpointer data, gpointer user_data)
{
  gzochid_pending_task *pending_task = data;
  void *args[2];

  g_mutex_lock (&pending_task->mutex);

  args[0] = pending_task;
  args[1] = user_data;

  scm_with_guile (pending_task_executor_inner, args);

  pending_task->state = GZOCHID_PENDING_TASK_STATE_COMPLETED;

  if (pending_task->destroy_on_execute)
    {
      g_mutex_unlock (&pending_task->mutex);
      free_pending_task (pending_task);
    }
  else
    {
      g_cond_broadcast (&pending_task->cond);
      g_mutex_unlock (&pending_task->mutex);
    }
}

gpointer 
gzochid_schedule_task_executor (gpointer data)
{
  gzochid_task_queue *task_queue = data;

  g_mutex_lock (&task_queue->mutex);
  while (task_queue->running)
    {
      if (g_queue_is_empty (task_queue->queue))
	g_cond_wait (&task_queue->cond, &task_queue->mutex);
      else 
	{
	  struct timeval current_time;
	  gzochid_pending_task *pending_task = 
	    g_queue_peek_head (task_queue->queue);

	  gettimeofday (&current_time, NULL);

	  if (timercmp 
	      (&current_time, &pending_task->scheduled_execution_time, >))
	    {
	      pending_task = g_queue_pop_head (task_queue->queue);
	      gzochid_thread_pool_push 
		(task_queue->pool, pending_task_executor, pending_task, NULL);
	    }
	  else 
	    {
	      struct timeval interval;
	      gint64 until = g_get_monotonic_time ();
	      
	      timersub 
		(&pending_task->scheduled_execution_time, 
		 &current_time, 
		 &interval);

	      until += interval.tv_sec * G_TIME_SPAN_SECOND + interval.tv_usec;
	      g_cond_wait_until (&task_queue->cond, &task_queue->mutex, until);
	    }
	}
    }
  g_mutex_unlock (&task_queue->mutex);

  return NULL;
}

void 
gzochid_schedule_task_queue_start (gzochid_task_queue *task_queue)
{
  g_mutex_lock (&task_queue->mutex);
  if (task_queue->consumer_thread == NULL)
    {
      task_queue->consumer_thread = g_thread_new
	("task-consumer", gzochid_schedule_task_executor, task_queue);
      task_queue->running = TRUE;
    }
  g_mutex_unlock (&task_queue->mutex);
}

void
gzochid_schedule_task_queue_stop (gzochid_task_queue *task_queue)
{
  g_mutex_lock (&task_queue->mutex);
  if (task_queue->consumer_thread != NULL)
    {
      GThread *consumer_thread_handle = task_queue->consumer_thread;
      
      task_queue->running = FALSE;
      task_queue->consumer_thread = NULL;

      g_cond_signal (&task_queue->cond);
      g_mutex_unlock (&task_queue->mutex);

      g_thread_join (consumer_thread_handle);
    }
  else g_mutex_unlock (&task_queue->mutex);
}

static gzochid_pending_task *
gzochid_pending_task_new (gzochid_task *task, gboolean destroy_on_execute)
{
  gzochid_pending_task *pending_task = malloc (sizeof (gzochid_pending_task));

  pending_task->task = task;
  pending_task->state = GZOCHID_PENDING_TASK_STATE_PENDING;
  pending_task->destroy_on_execute = destroy_on_execute;
  
  g_cond_init (&pending_task->cond);
  g_mutex_init (&pending_task->mutex);
 
  pending_task->scheduled_execution_time = task->target_execution_time;
  
  return pending_task;
}

gint 
pending_task_compare (gconstpointer a, gconstpointer b, gpointer user_data)
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

static void 
task_chain_worker (gpointer data, gpointer user_data)
{
  GList *tasks = (GList *) data;
  GList *task_ptr = tasks;

  while (task_ptr != NULL)
    {
      gzochid_schedule_execute_task ((gzochid_task *) task_ptr->data);
      task_ptr = task_ptr->next;
    }

  g_list_free (tasks);
}

void
gzochid_schedule_submit_task_chain (gzochid_task_queue *task_queue, 
				    GList *tasks)
{
  GList *tasks_copy = g_list_copy (tasks);
  gzochid_task *task = malloc (sizeof (gzochid_task));

  assert (g_list_length (tasks) > 0);

  task->worker = task_chain_worker;
  task->data = tasks_copy;
  task->target_execution_time = 
    ((gzochid_task *) tasks_copy->data)->target_execution_time;

  gzochid_schedule_submit_task (task_queue, task);
}

static gzochid_pending_task *
submit_task (gzochid_task_queue *task_queue, gzochid_task *task,
	     gboolean destroy_on_execute)
{
  gzochid_pending_task *pending_task =
    gzochid_pending_task_new (task, destroy_on_execute);
  
  g_mutex_lock (&task_queue->mutex);
  g_queue_insert_sorted 
    (task_queue->queue, pending_task, pending_task_compare, NULL);
  g_cond_signal (&task_queue->cond);
  g_mutex_unlock (&task_queue->mutex);

  return pending_task;
}

void
gzochid_schedule_submit_task (gzochid_task_queue *task_queue,
			      gzochid_task *task)
{
  submit_task (task_queue, task, TRUE);
}

void 
gzochid_schedule_run_task (gzochid_task_queue *task_queue, gzochid_task *task)
{
  gzochid_pending_task *pending_task = submit_task (task_queue, task, FALSE);

  g_mutex_lock (&pending_task->mutex);
  while (pending_task->state == GZOCHID_PENDING_TASK_STATE_PENDING)
    g_cond_wait (&pending_task->cond, &pending_task->mutex);
  g_mutex_unlock (&pending_task->mutex);
  
  free_pending_task (pending_task);
}

void 
gzochid_schedule_execute_task (gzochid_task *task)
{
  task->worker (task->data, NULL);
}
