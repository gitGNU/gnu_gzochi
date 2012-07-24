/* util.c: Shared utility procedures for Scheme->C API.
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
#include "../scheme.h"
#include "../tx.h"

#include "util.h"

static SCM scm_make_no_current_application_condition;
static SCM scm_make_transaction_aborted_condition;

gzochid_application_context *gzochid_api_ensure_current_application_context ()
{
  gzochid_application_context *context = 
    gzochid_get_current_application_context();
  if (context == NULL)
    gzochid_scheme_r6rs_raise 
      (scm_call_0 (scm_make_no_current_application_condition));

  return context;
}

void gzochid_api_check_rollback ()
{
  if (gzochid_transaction_rollback_only ())
    gzochid_scheme_r6rs_raise 
      (scm_call_0 (scm_make_transaction_aborted_condition));
}

void gzochid_api_util_init ()
{
  SCM gzochi_conditions = scm_c_resolve_module ("gzochi conditions");
  SCM var = scm_c_module_lookup 
    (gzochi_conditions, "gzochi:make-no-current-application-condition");
  
  scm_make_no_current_application_condition = scm_variable_ref (var);
  scm_gc_protect_object (scm_make_no_current_application_condition);

  var = scm_c_module_lookup 
    (gzochi_conditions, "gzochi:make-transaction-aborted-condition");
  scm_make_transaction_aborted_condition = scm_variable_ref (var);
  scm_gc_protect_object (scm_make_transaction_aborted_condition);
}
