/* test-task.c: Test routines for task.c in gzochid.
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
#include <sys/time.h>

#include "task.h"

static void
test_thread_worker (gpointer data, gpointer user_data)
{
}

static void
test_immediate_new ()
{
  struct timeval t;
  gboolean data = FALSE;
  gzochid_task *task = NULL;

  gettimeofday (&t, NULL);

  task = gzochid_task_immediate_new (test_thread_worker, &data);
  
  g_assert (task->worker == test_thread_worker);
  g_assert (task->data == &data);

  /* Workaround for platforms with broken `timercmp'. */
  
  g_assert (! timercmp (&task->target_execution_time, &t, <));
  gzochid_task_free (task);
}

static void
test_new ()
{
  struct timeval t;
  gboolean data = FALSE;
  gzochid_task *task = NULL;

  gettimeofday (&t, NULL);

  task = gzochid_task_new (test_thread_worker, &data, t);

  g_assert (task->worker == test_thread_worker);
  g_assert (task->data == &data);

  /* Workaround for platforms with broken `timercmp'. */

  g_assert (! timercmp (&task->target_execution_time, &t, !=));
  gzochid_task_free (task);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/task/immediate_new", test_immediate_new);
  g_test_add_func ("/task/new", test_new);

  return g_test_run ();
}
