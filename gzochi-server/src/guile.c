/* guile.c: GNU Guile interface routines for gzochid
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
#include <libguile.h>
#include <stdio.h>
#include <stdlib.h>

#include "guile.h"
#include "threads.h"

static SCM scm_r6rs_with_exception_handler;
static SCM scm_r6rs_exception_handler;

static SCM scm_r6rs_raise;
static SCM scm_r6rs_raise_continuable;

static SCM scm_exception_printer;
static SCM scm_make_handler;
static SCM scm_make_thunk;

static SCM scm_add_to_load_path;

static SCM 
guile_catch_body (void *data)
{
  void **ptr = data;

  SCM procedure = ptr[0];
  SCM args = ptr[1];
  SCM exception_var = ptr[2];
  
  return scm_call_2 
    (scm_r6rs_with_exception_handler,
     scm_call_2 (scm_make_handler, scm_r6rs_exception_handler, exception_var),
     scm_call_2 (scm_make_thunk, procedure, args));
}

static SCM 
r6rs_exception_handler (SCM exception_var, SCM cond)
{
  SCM output = scm_open_output_string ();
  SCM output_string = SCM_BOOL_F;
  char *str = NULL;

  scm_display_backtrace 
    (scm_make_stack (SCM_BOOL_T, SCM_EOL), output, SCM_BOOL_F, SCM_BOOL_F);

  scm_call_2 (scm_exception_printer, output, cond);

  output_string = scm_get_output_string (output);
  str = scm_to_locale_string (output_string);

  fprintf (stderr, "%s", str);

  free (str);

  if (scm_variable_p (exception_var) == SCM_BOOL_T)
    scm_variable_set_x (exception_var, cond);

  return SCM_BOOL_F;
}

static SCM guile_catch_handler (void *data, SCM tag, SCM throw_args)
{
  SCM exception_variable = data;

  if (scm_variable_p (exception_variable) == SCM_BOOL_T
      && scm_variable_ref (exception_variable) == SCM_UNSPECIFIED)
    scm_variable_set_x (exception_variable, scm_cons (tag, throw_args));

  return SCM_BOOL_F;
}

static SCM guile_pre_unwind_handler (void *data, SCM tag, SCM throw_args)
{
  SCM exception_variable = data;

  if (scm_variable_p (exception_variable) == SCM_BOOL_T
      && scm_variable_ref (exception_variable) == SCM_UNSPECIFIED)
    {
      scm_display_backtrace 
	(scm_make_stack (SCM_BOOL_T, SCM_EOL),
	 scm_current_output_port (), 
	 SCM_BOOL_F, 
	 SCM_BOOL_F);
  
      scm_print_exception 
	(scm_current_output_port (), SCM_BOOL_F, tag, throw_args);
    }

  return SCM_BOOL_F;
}

SCM gzochid_guile_invoke (SCM procedure, SCM args, SCM exception_var)
{
  SCM ret = SCM_BOOL_F;
  void *sub_data[3];

  sub_data[0] = procedure;
  sub_data[1] = args;
  sub_data[2] = exception_var;

  ret = scm_c_catch 
    (SCM_BOOL_T,
     guile_catch_body, sub_data, 
     guile_catch_handler, exception_var, 
     guile_pre_unwind_handler, exception_var);

  scm_remember_upto_here_1 (procedure);
  scm_remember_upto_here_1 (args);

  return ret;
}

SCM 
gzochid_guile_r6rs_raise (SCM cond)
{
  return scm_call_1 (scm_r6rs_raise, cond);
}

SCM 
gzochid_guile_r6rs_raise_continuable (SCM cond)
{
  return scm_call_1 (scm_r6rs_raise_continuable, cond);
}

void 
gzochid_guile_add_to_load_path (char *path)
{
  SCM scm_path = scm_from_locale_string (path);

  gzochid_guile_invoke 
    (scm_add_to_load_path, scm_list_1 (scm_path), SCM_BOOL_F);
}

static void 
bind_scm (char *module, SCM *binding, char *name)
{
  SCM var = scm_c_public_variable (module, name);

  if (scm_is_false (var))
    g_error ("Missing Scheme binding for `%s'. Aborting...", name);

  *binding = scm_variable_ref (var);
  scm_gc_protect_object (*binding);
}

void gzochid_guile_init ()
{
  scm_r6rs_exception_handler = scm_c_make_gsubr 
    ("r6rs_exception_handler", 2, 0, 0, r6rs_exception_handler);
  scm_gc_protect_object (scm_r6rs_exception_handler);

  bind_scm ("rnrs exceptions", &scm_r6rs_raise, "raise");
  bind_scm ("rnrs exceptions", &scm_r6rs_raise_continuable, 
	    "raise-continuable");
  bind_scm ("rnrs exceptions", &scm_r6rs_with_exception_handler, 
	    "with-exception-handler");

  bind_scm ("gzochi private guile", &scm_add_to_load_path, 
	    "gzochi:add-to-load-path");
  bind_scm ("gzochi private guile", &scm_exception_printer, 
	    "gzochi:exception-printer");
  bind_scm ("gzochi private guile", &scm_make_handler, "gzochi:make-handler");
  bind_scm ("gzochi private guile", &scm_make_thunk, "gzochi:make-thunk");

  
}
