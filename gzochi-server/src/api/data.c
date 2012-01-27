/* data.c: Primitive functions for user-facing gzochi data management API
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
#include <stdlib.h>

#include "../app.h"
#include "../data.h"
#include "../scheme.h"

#include "data.h"

SCM_DEFINE (primitive_create_reference, "primitive-create-reference", 1, 0, 0, 
	    (SCM obj), "Create a managed reference.")
{
  gzochid_application_context *context = 
    gzochid_scheme_current_application_context ();
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, obj);
  
  scm_gc_protect_object (obj);

  return gzochid_scheme_create_managed_reference (reference);
}

SCM_DEFINE (primitive_dereference, "primitive-dereference", 1, 0, 0, (SCM ref),
	    "Dereference a managed reference.")
{
  gzochid_application_context *context = 
    gzochid_scheme_current_application_context ();
  gzochid_data_managed_reference *reference = NULL;
  mpz_t oid;
  
  mpz_init (oid);

  gzochid_scheme_managed_reference_oid (ref, oid);

  reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_scheme_data_serialization, oid);
  gzochid_data_dereference (reference);

  if (reference->obj == NULL)
    return gzochid_scheme_r6rs_raise 
      (gzochid_scheme_make_object_removed_condition ());
  else return reference->obj;
}

SCM_DEFINE (primitive_get_binding, "primitive-get-binding", 1, 0, 0, (SCM name),
	    "Retrieve a managed record bound to a name.")
{
  gzochid_application_context *context =
    gzochid_scheme_current_application_context ();
  char *cname = scm_to_locale_string (name);
  SCM obj = (SCM) gzochid_data_get_binding 
    (context, cname, &gzochid_scheme_data_serialization);

  free (cname);

  return obj;
}

SCM_DEFINE (primitive_set_binding_x, "primitive-set-binding!", 2, 0, 0,
	    (SCM name, SCM obj), "Bind a managed record to a name.")
{
  gzochid_application_context *context =
    gzochid_scheme_current_application_context ();
  char *cname = scm_to_locale_string (name);
  
  gzochid_data_set_binding 
    (context, cname, &gzochid_scheme_data_serialization, obj);

  free (cname);
  
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_remove_binding_x, "primitive-remove-binding!", 1, 0, 0,
	    (SCM name), "Remove the binding for a name.")
{
  gzochid_application_context *context =
    gzochid_scheme_current_application_context ();
  char *cname = scm_to_locale_string (name);
  
  gzochid_data_remove_binding (context, cname);

  free (cname);
  
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_mark_for_write_x, "primitive-mark-for-write!", 1, 0, 0,
	    (SCM obj), "Mark a managed record that has been modified.")
{
  gzochid_data_mark 
    (gzochid_scheme_current_application_context (), 
     &gzochid_scheme_data_serialization, obj);
  return SCM_UNSPECIFIED;
}

void gzochid_api_data_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_data = scm_c_resolve_module ("gzochi private data");
  scm_set_current_module (gzochi_private_data);

  #include "data.x"

  scm_set_current_module (current_module);
}
