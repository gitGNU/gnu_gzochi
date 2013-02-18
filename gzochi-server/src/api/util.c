/* util.c: Shared utility procedures for Scheme->C API.
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

#include <libguile.h>

#include "../app.h"
#include "../scheme.h"
#include "../tx.h"

#include "util.h"

static SCM scm_condition;
static SCM scm_make_no_current_application_condition;
static SCM scm_make_transaction_aborted_condition;
static SCM scm_make_transaction_retry_condition;
static SCM scm_make_transaction_timeout_condition;

gzochid_application_context *gzochid_api_ensure_current_application_context ()
{
  gzochid_application_context *context = 
    gzochid_get_current_application_context();
  if (context == NULL)
    gzochid_scheme_r6rs_raise 
      (scm_call_0 (scm_make_no_current_application_condition));

  return context;
}

void gzochid_api_check_transaction ()
{
  if (gzochid_transaction_timed_out ())
    gzochid_scheme_r6rs_raise
      (scm_call_2 (scm_condition, 
		   scm_call_0 (scm_make_transaction_timeout_condition),
		   scm_call_0 (scm_make_transaction_retry_condition)));

  if (gzochid_transaction_rollback_only ())
    gzochid_scheme_r6rs_raise 
      (scm_call_0 (scm_make_transaction_aborted_condition));
}

static void bind_scm (char *module, SCM *binding, char *name)
{
  SCM var = scm_c_public_variable (module, name);

  if (scm_is_false (var))
    g_error ("Missing Scheme binding for `%s'. Aborting...", name);

  *binding = scm_variable_ref (var);
  scm_gc_protect_object (*binding);
}

void gzochid_api_util_init ()
{
  bind_scm ("rnrs conditions", &scm_condition, "condition");
  
  bind_scm ("gzochi conditions", &scm_make_no_current_application_condition,
	    "gzochi:make-no-current-application-condition");
  bind_scm ("gzochi conditions", &scm_make_transaction_aborted_condition,
	    "gzochi:make-transaction-aborted-condition");
  bind_scm ("gzochi conditions", &scm_make_transaction_retry_condition,
	    "gzochi:make-transaction-retry-condition");
  bind_scm ("gzochi conditions", &scm_make_transaction_timeout_condition,
	    "gzochi:make-transaction-timeout-condition");
}
