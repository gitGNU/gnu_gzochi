/* admin.c: Primitive functions for gzochi admin API
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

#include <glib.h>
#include <libguile.h>
#include <stdlib.h>

#include "../app.h"
#include "../auth.h"
#include "../game.h"
#include "../guile.h"

#include "task.h"
#include "util.h"

static gzochid_game_context *game_context;

static SCM scm_application_context_name;
static SCM scm_make_application_context;

SCM_DEFINE (primitive_applications, "primitive-applications", 0, 0, 0, 
	    (), "List the current active applications.")
{
  SCM ret = SCM_EOL;
  GList *applications = gzochid_game_context_get_applications (game_context);
  GList *applications_ptr = applications;
  
  while (applications_ptr != NULL)
    {
      gzochid_application_context *app_context = 
	(gzochid_application_context *) applications_ptr->data;
      SCM application_record = scm_call_1 
	(scm_make_application_context, 
	 scm_from_locale_string (app_context->descriptor->name));

      ret = scm_cons (application_record, ret);      
      applications_ptr = applications_ptr->next;
    }
  
  g_list_free (applications);

  return ret;
}

SCM_DEFINE (primitive_current_application, "primitive-current-application", 
	    0, 0, 0, (), "Returns the current application or #f if not set.")
{
  gzochid_application_context *context = 
    gzochid_get_current_application_context ();

  if (context == NULL)
    return SCM_BOOL_F;
  else return scm_call_1
	 (scm_make_application_context,
	  scm_from_locale_string (context->descriptor->name));
}

static void thunk_worker (gpointer data, gpointer user_data)
{
  void **args = (void **) data;

  SCM thunk = (SCM) args[0];
  SCM *out = (SCM *) args[1];
  SCM exception_var = (SCM) args[2];

  *out = gzochid_guile_invoke (thunk, SCM_EOL, exception_var);
  gzochid_api_check_rollback ();
}

static void *thunk_guile_worker (gpointer data)
{
  gzochid_guile_run (thunk_worker, data);
  return NULL;
}

static void thunk_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_with_application_context 
    (context, identity, thunk_guile_worker, data);
}

SCM_DEFINE (primitive_with_application, "primitive-with-application", 
	    2, 0, 0, (SCM context, SCM thunk), 
	    "Returns the current application or #f if not set.")
{
  char *name = scm_to_locale_string 
    (scm_call_1 (scm_application_context_name, context));
  gzochid_application_context *app_context = 
    gzochid_game_context_lookup_application (game_context, name);
  gzochid_auth_identity *debug_identity = calloc 
    (1, sizeof (gzochid_auth_identity));

  gzochid_transactional_application_task transactional_task;
  gzochid_application_task application_task;

  SCM ret = SCM_UNDEFINED;
  SCM exception_var = scm_make_variable (SCM_BOOL_F);
  SCM exception = SCM_UNDEFINED;
  void *args[3];

  debug_identity->name = "[DEBUG]";

  args[0] = thunk;
  args[1] = &ret;
  args[2] = exception_var;
  
  transactional_task.worker = thunk_application_worker;
  transactional_task.data = args;

  application_task.worker = gzochid_application_transactional_task_worker;
  application_task.context = app_context;
  application_task.identity = debug_identity;
  application_task.data = &transactional_task;

  gzochid_application_task_worker (&application_task);

  free (name);

  exception = scm_variable_ref (exception_var);
  if (exception != SCM_BOOL_F)
    scm_throw (SCM_CAR (exception), SCM_CDR (exception));

  return ret;
}

void gzochid_api_admin_init (gzochid_game_context *context)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_admin = scm_c_resolve_module ("gzochi admin");

  SCM make_application_context_var = scm_c_module_lookup 
    (gzochi_admin, "gzochi:make-application-context");
  SCM application_context_name_var = scm_c_module_lookup
    (gzochi_admin, "gzochi:application-context-name");

  scm_make_application_context = 
    scm_variable_ref (make_application_context_var);  
  scm_gc_protect_object (scm_make_application_context);

  scm_application_context_name = 
    scm_variable_ref (application_context_name_var);
  scm_gc_protect_object (scm_application_context_name);

  game_context = context;
  scm_set_current_module (gzochi_admin);

  #include "admin.x"

  scm_set_current_module (current_module);
}
