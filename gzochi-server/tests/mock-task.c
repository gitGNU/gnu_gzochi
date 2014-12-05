/* mock-task.c: Test-time replacements for task.c routines.
 * Copyright (C) 2014 Julian Graham
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
#include <stdlib.h>

#include "app.h"
#include "auth.h"

gzochid_application_task *gzochid_application_task_new 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_application_worker worker, gpointer data)
{
  gzochid_application_task *task = malloc (sizeof (gzochid_application_task));

  task->context = context;
  task->identity = identity;
  task->worker = worker;
  task->data = data;

  return task;
}
