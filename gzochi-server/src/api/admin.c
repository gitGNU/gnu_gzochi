/* admin.c: Primitive functions for gzochi admin API
 * Copyright (C) 2017 Julian Graham
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
#include <glib-object.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "../app.h"
#include "../game.h"
#include "../guile.h"
#include "../gzochid-auth.h"
#include "../scheme.h"
#include "../tx.h"

#include "task.h"
#include "util.h"

static GzochidGameServer *game_server;

static SCM scm_application_context_name;
static SCM scm_make_application_context;

SCM_DEFINE (primitive_applications, "primitive-applications", 0, 0, 0, 
	    (), "List the current active applications.")
{
  SCM ret = SCM_EOL;
  GList *applications = gzochid_game_server_get_applications (game_server);
  GList *applications_ptr = applications;
  
  while (applications_ptr != NULL)
    {
      gzochid_application_context *app_context = applications_ptr->data;
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

/* Wraps `gzochid_transaction_end' in a dynamic wind "unwind handler," so it can
   be called on exit from the dynamic created by `primitive_with_application'.
*/

static void
with_application_unwind_handler (void *data)
{
  gzochid_transaction_end ();
}

SCM_DEFINE (primitive_with_application, "primitive-with-application", 
	    2, 0, 0, (SCM context, SCM thunk), 
	    "Returns the current application or #f if not set.")
{
  char *name = scm_to_locale_string 
    (scm_call_1 (scm_application_context_name, context));
  gzochid_application_context *app_context = 
    gzochid_game_server_lookup_application (game_server, name);
  gzochid_auth_identity *debug_identity =
    gzochid_auth_identity_from_name (app_context->identity_cache, "[DEBUG]");
  gzochid_transaction_timing timing = { { 0, 0 }, NULL };

  free (name);

  scm_dynwind_begin (0);
  gzochid_transaction_begin (&timing);

  /* Whether or not the thunk exits non-locally, we need to end the transaction
     cleanly. */

  scm_dynwind_unwind_handler 
    (with_application_unwind_handler, NULL, SCM_F_WIND_EXPLICITLY);

  gzochid_with_application_context 
    (app_context, debug_identity, (void * (*) (void *)) scm_call_0, thunk);
  scm_dynwind_end ();

  gzochid_auth_identity_unref (debug_identity);
  
  if (gzochid_transaction_active ())
    gzochid_api_check_transaction ();

  scm_remember_upto_here_1 (thunk);

  return SCM_UNSPECIFIED;
}

void 
gzochid_api_admin_init (GzochidGameServer *server)
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

  game_server = g_object_ref (server);
  scm_set_current_module (gzochi_admin);

  #include "admin.x"

  scm_set_current_module (current_module);
}
