/* task.c: Primitive functions for user-facing gzochi task management API
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

#include <libguile.h>

#include "../app.h"
#include "../auth.h"
#include "../scheme.h"
#include "../task.h"

#include "task.h"
#include "util.h"

SCM_DEFINE (primitive_schedule_task, "primitive-schedule-task", 2, 0, 0, 
	    (SCM callback, SCM delay), "Schedule a durable task to run.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context (); 
  gzochid_auth_identity *identity = gzochid_get_current_identity ();

  gzochid_application_task *scheme_task = gzochid_application_task_new 
    (context, identity, gzochid_scheme_application_worker, callback);
  
  unsigned long d = scm_to_ulong (delay);
  gzochid_schedule_delayed_durable_task 
    (context, identity, scheme_task, &gzochid_scheme_task_serialization, d);

  return SCM_UNSPECIFIED;
}

void gzochid_api_task_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_task = scm_c_resolve_module ("gzochi private task");
  scm_set_current_module (gzochi_private_task);

  #include "task.x"

  scm_set_current_module (current_module);
}
