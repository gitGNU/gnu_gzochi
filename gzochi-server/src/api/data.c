/* data.c: Primitive functions for user-facing gzochi data management API
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

#include <glib.h>
#include <gmp.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "../app.h"
#include "../data.h"
#include "../reloc.h"
#include "../scheme.h"

#include "data.h"
#include "util.h"

SCM_DEFINE (primitive_create_reference, "primitive-create-reference", 1, 0, 0, 
	    (SCM obj), "Create a managed reference.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  gzochid_scm_location_info *obj_loc = gzochid_scm_location_get (context, obj);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_scm_location_aware_serialization, obj_loc);
  SCM ret = SCM_BOOL_F;

  scm_gc_protect_object (obj);

  ret = gzochid_scheme_create_managed_reference (reference);
  
  gzochid_api_check_transaction ();

  return ret;
}

SCM_DEFINE (primitive_dereference, "primitive-dereference", 1, 0, 0, (SCM ref),
	    "Dereference a managed reference.")
{
  GError *err = NULL;
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *reference = NULL;
  SCM ret = SCM_BOOL_F;
  mpz_t oid;
  
  mpz_init (oid);

  gzochid_scheme_managed_reference_oid (ref, oid);

  reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_scm_location_aware_serialization, oid);

  mpz_clear (oid);
  gzochid_data_dereference (reference, &err);

  if (err == NULL)
    {
      if (reference->obj == NULL
	  || reference->state == 
	  GZOCHID_MANAGED_REFERENCE_STATE_REMOVED_FETCHED)
	gzochid_guile_r6rs_raise 
	  (gzochid_scheme_make_object_removed_condition ());
      else 
	{
	  gzochid_scm_location_info *location = 
	    (gzochid_scm_location_info *) reference->obj;
	  ret = gzochid_scm_location_resolve (context, location);
	}
    }
  else gzochid_api_check_not_found (err);

  gzochid_api_check_transaction ();

  return ret;
}

static char *prefix_name (char *name)
{
  int name_len = strlen (name);
  char *prefixed_name = malloc (sizeof (char) * (name_len + 3));
  
  prefixed_name = strncpy (prefixed_name, "o.", 3);
  prefixed_name = strncat (prefixed_name, name, name_len);

  return prefixed_name;
}

SCM_DEFINE (primitive_get_binding, "primitive-get-binding", 1, 0, 0, (SCM name),
	    "Retrieve a managed record bound to a name.")
{
  GError *err = NULL;
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  
  char *cname = scm_to_locale_string (name);
  char *prefixed_name = prefix_name (cname);
  
  SCM ret = SCM_BOOL_F;
  SCM cond = SCM_BOOL_F;
  
  if (! gzochid_data_binding_exists (context, prefixed_name, &err))
    {
      if (err == NULL)	
	cond = gzochid_scheme_make_name_not_bound_condition (cname);
      else g_error_free (err);

      free (cname);
      free (prefixed_name);
    }
  else 
    {
      gzochid_scm_location_info *location = 
	(gzochid_scm_location_info *) gzochid_data_get_binding 
	(context, prefixed_name, &gzochid_scm_location_aware_serialization, 
	 &err);

      free (cname);
      free (prefixed_name);

      if (err == NULL)
	{
	  if (location != NULL)
	    ret = gzochid_scm_location_resolve (context, location);
	}
      else gzochid_api_check_not_found (err);
    }

  gzochid_api_check_transaction ();

  if (! scm_is_false (cond))    
    gzochid_guile_r6rs_raise (cond);

  return ret;
}

SCM_DEFINE (primitive_set_binding_x, "primitive-set-binding!", 2, 0, 0,
	    (SCM name, SCM obj), "Bind a managed record to a name.")
{
  GError *err = NULL;
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  char *cname = scm_to_locale_string (name);
  char *prefixed_name = prefix_name (cname);
  
  if (gzochid_data_binding_exists (context, prefixed_name, &err))
    {
      SCM cond = gzochid_scheme_make_name_exists_condition (cname);

      free (cname);
      free (prefixed_name);

      gzochid_guile_r6rs_raise (cond);
    }
  else if (err == NULL)
    {
      gzochid_scm_location_info *obj_loc = 
	gzochid_scm_location_get (context, obj);

      scm_gc_protect_object (obj);
      
      gzochid_data_set_binding 
	(context, prefixed_name, &gzochid_scm_location_aware_serialization, 
	 obj_loc, NULL);
      
      free (cname);
      free (prefixed_name);
    }

  gzochid_api_check_transaction ();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_remove_binding_x, "primitive-remove-binding!", 1, 0, 0,
	    (SCM name), "Remove the binding for a name.")
{
  GError *err = NULL;
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  char *cname = scm_to_locale_string (name);
  char *prefixed_name = prefix_name (cname);
  
  if (! gzochid_data_binding_exists (context, prefixed_name, &err))
    {
      SCM cond = gzochid_scheme_make_name_not_bound_condition (cname);

      free (cname);
      free (prefixed_name);

      gzochid_guile_r6rs_raise (cond);
    }
  else if (err == NULL)
    {
      gzochid_data_remove_binding (context, prefixed_name, NULL);

      free (cname);
      free (prefixed_name);
    }

  gzochid_api_check_transaction ();
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_remove_object_x, "primitive-remove-object!", 1, 0, 0,
	    (SCM obj), "Remove a managed record from the data store.")
{
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  gzochid_scm_location_info *obj_loc = gzochid_scm_location_get (context, obj);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_scm_location_aware_serialization, obj_loc);

  scm_gc_protect_object (obj);    

  gzochid_data_remove_object (reference, NULL);

  gzochid_api_check_transaction ();

  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_mark_for_write_x, "primitive-mark-for-write!", 1, 0, 0,
	    (SCM obj), "Mark a managed record that has been modified.")
{
  GError *err = NULL;
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  gzochid_scm_location_info *obj_loc = gzochid_scm_location_get (context, obj);

  scm_gc_protect_object (obj);    

  gzochid_data_mark 
    (context, &gzochid_scm_location_aware_serialization, obj_loc, &err);

  gzochid_api_check_transaction ();

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
