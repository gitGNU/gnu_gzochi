/* mock-schedule.c: Test-time replacements for schedule.c routines.
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

#include <stddef.h>

#include "../schedule.h"
#include "../task.h"

gzochid_pending_task *gzochid_schedule_submit_task
(gzochid_task_queue *task_queue, gzochid_task *task)
{
  gzochid_schedule_execute_task (task);
}

void gzochid_schedule_run_task 
(gzochid_task_queue *task_queue, gzochid_task *task)
{
  gzochid_schedule_execute_task (task);
}

void gzochid_schedule_execute_task (gzochid_task *task)
{
  task->worker (task->data, NULL);
}
