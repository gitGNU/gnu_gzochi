/* task.c: Primitive functions for user-facing gzochi task management API
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
#include <gmp.h>
#include <libguile.h>
#include <stddef.h>
#include <sys/time.h>

#include "../app.h"
#include "../app-task.h"
#include "../auth_int.h"
#include "../durable-task.h"
#include "../gzochid-auth.h"
#include "../scheme.h"
#include "../scheme-task.h"

#include "task.h"
#include "util.h"

SCM_DEFINE (primitive_schedule_task, "primitive-schedule-task", 1, 2, 0, 
	    (SCM callback, SCM delay, SCM period), 
	    "Schedule a durable task to run.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context (); 
  gzochid_auth_identity *identity =
    gzochid_auth_identity_clone (gzochid_get_current_identity ());
  gzochid_application_task *scheme_task = gzochid_application_task_new 
    (context, identity, gzochid_scheme_application_task_worker, callback);

  /* FIXME: This object will stay protected even if the current transaction
     gets rolled back. */

  scm_gc_protect_object (callback);
  
  if (scm_is_false (delay) || scm_is_eq (delay, SCM_UNDEFINED))
    {
      gzochid_schedule_durable_task 
	(context, identity, scheme_task, &gzochid_scheme_task_serialization);
      
      gzochid_api_check_transaction ();
      
      return SCM_UNSPECIFIED;
    }
  else
    {
      unsigned long d = scm_to_ulong (delay);
      struct timeval delay_tv;
      
      delay_tv.tv_sec = d / 1000;
      delay_tv.tv_usec = (d % 1000) * 1000; 

      if (scm_is_false (period) || scm_is_eq (period, SCM_UNDEFINED))
	{
	  gzochid_schedule_delayed_durable_task 
	    (context, identity, scheme_task, 
	     &gzochid_scheme_task_serialization, delay_tv);
	  
	  gzochid_api_check_transaction ();
	  
	  return SCM_UNSPECIFIED;
	}
      else
	{
	  SCM ret = SCM_BOOL_F;
	  struct timeval period_tv;
	  gzochid_periodic_task_handle *handle = NULL;
	  gzochid_data_managed_reference *reference = NULL;

	  d = scm_to_ulong (period);
	  period_tv.tv_sec = d / 1000;
	  period_tv.tv_usec = (d % 1000) * 1000;
	  
	  handle = gzochid_schedule_periodic_durable_task 
	    (context, identity, scheme_task, 
	     &gzochid_scheme_task_serialization, delay_tv, period_tv);
	  reference = gzochid_data_create_reference 
	    (context, &gzochid_durable_application_task_handle_serialization, 
	     handle);
	  
	  ret = gzochid_scheme_create_periodic_task_handle (reference->oid);
	  
	  gzochid_api_check_transaction ();
	  
	  return ret;
	}
    }
}

SCM_DEFINE (primitive_cancel_task, "primitive-cancel-task", 1, 0, 0, 
	    (SCM scm_handle), "Cancel a previously scheduled periodic task.")
{
  GError *err = NULL;
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();   
  gzochid_data_managed_reference *handle_reference = NULL;
  gzochid_periodic_task_handle *task_handle = NULL;
  mpz_t oid;
  
  mpz_init (oid);
  
  gzochid_scheme_task_handle_oid (scm_handle, oid);
  handle_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_durable_application_task_handle_serialization, oid);

  mpz_clear (oid);

  gzochid_data_dereference (handle_reference, &err);

  if (err == NULL)
    {
      task_handle = (gzochid_periodic_task_handle *) handle_reference->obj;
      gzochid_cancel_periodic_task (context, task_handle);
    }
  else gzochid_api_check_not_found (err);

  gzochid_api_check_transaction ();

  return SCM_UNSPECIFIED;
}

void 
gzochid_api_task_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_task = scm_c_resolve_module ("gzochi private task");
  scm_set_current_module (gzochi_private_task);

  #include "task.x"

  scm_set_current_module (current_module);
}
