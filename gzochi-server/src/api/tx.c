/* tx.c: Primitive functions for gzochi external transaction API
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

#include <assert.h>
#include <glib.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "../guile.h"
#include "../gzochid-auth.h"
#include "../scheme.h"
#include "../tx.h"

#include "tx.h"
#include "util.h"

/* This file contains an implementation of a transaction participant that acts
   as a bridge between the gzochid transaction subsystem and transaction
   participants registered from userland Scheme code. It works by having a
   meta-participant that dispatches all transaction lifecycle events to a list
   of registered Scheme participants. */

struct _gzochid_api_tx_context
{
  gzochid_application_context *context; /* The application context. */
  gzochid_auth_identity *identity; /* The system identity. */
  SCM participants; /* The cons list of bound participants. */
};

typedef struct _gzochid_api_tx_context gzochid_api_tx_context;

/* Some useful `(rnrs conditions)' exports. */

static SCM scm_condition;
static SCM scm_make_assertion_violation;
static SCM scm_make_message_condition;

/* Accessor procedures for the transaction participant record type. */

static SCM scm_participant_prepare;
static SCM scm_participant_commit;
static SCM scm_participant_rollback;

/* The following functions provide the prepare, commit, and rollback handlers
   for the transaction participant interface. In this context, they dispatch to
   the appropriate Scheme procedures for the registered Scheme-side transaction
   participants. */

/* The `prepare' implementation.
   
   The `data' argument is the transaction context for the meta-participant. 
   Returns true if all participants return true, false otherwise.
*/

static int
prepare (gpointer data)
{
  gzochid_api_tx_context *tx_context = data;

  SCM participants = tx_context->participants;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  while (participants != SCM_EOL)
    {
      SCM participant = SCM_CAR (participants);
      SCM participant_prepare = 
	scm_call_1 (scm_participant_prepare, participant);
      SCM result = gzochid_scheme_invoke 
	(tx_context->context, tx_context->identity, participant_prepare, 
	 SCM_EOL, exception_var);

      if (scm_is_false (result) 
	  || scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
	return FALSE;

      participants = SCM_CDR (participants);
    }

  return TRUE;
}

/* Shared transaction cleanup code for use by `commit' and `rollback'. */

static void
free_transaction_context (gzochid_api_tx_context *tx_context)
{
  SCM participants = tx_context->participants;

  while (participants != SCM_EOL)
    {
      /* Every `car' aof the participants list was protected, and so needs to be
	 unprotected. */

      scm_gc_unprotect_object (participants);
      participants = SCM_CDR (participants);
    }

  free (tx_context->identity);
  free (tx_context);
}

/* The `commit' implementation.

   The `data' argument is the transaction context for the meta-participant. 
*/

static void
commit (gpointer data)
{
  gzochid_api_tx_context *tx_context = data;

  SCM participants = tx_context->participants;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  while (participants != SCM_EOL)
    {
      SCM participant = SCM_CAR (participants);
      SCM participant_commit = 
	scm_call_1 (scm_participant_commit, participant);
      
      gzochid_scheme_invoke 
	(tx_context->context, tx_context->identity, participant_commit, 
	 SCM_EOL, exception_var);

      assert (scm_variable_ref (exception_var) == SCM_UNSPECIFIED);
      participants = SCM_CDR (participants);
    }

  free_transaction_context (tx_context);
}

/* The `rollback' implementation.

   The `data' argument is the transaction context for the meta-participant. 
*/

static void
rollback (gpointer data)
{
  gzochid_api_tx_context *tx_context = data;

  SCM participants = tx_context->participants;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  while (participants != SCM_EOL)
    {
      SCM participant = SCM_CAR (participants);
      SCM participant_rollback = 
	scm_call_1 (scm_participant_rollback, participant);

      gzochid_scheme_invoke 
	(tx_context->context, tx_context->identity, participant_rollback, 
	 SCM_EOL, exception_var);

      assert (scm_variable_ref (exception_var) == SCM_UNSPECIFIED);
      participants = SCM_CDR (participants);
    }

  free_transaction_context (tx_context);
}

static gzochid_transaction_participant tx_participant =
  { "ext", prepare, commit, rollback };

/* The snarf-definition for primitive_join_transaction, which replaces the
   default `#f' definition of `primitive-join-transaction' in `(gzochi tx)'.

   Expects a `gzochi:transaction-participant' record, which should have been
   type-checked by the Scheme-side implementation of `gzochi:join-transaction'.
*/

SCM_DEFINE (primitive_join_transaction, "primitive-join-transaction", 1, 0, 0,
	    (SCM participant), "Join the current transaction.")
{
  gzochid_api_tx_context *tx_context = NULL;

  if (!gzochid_transaction_active ()
      || (tx_context = gzochid_transaction_context (&tx_participant)) == NULL)
    {
      tx_context = malloc (sizeof (gzochid_api_tx_context));

      tx_context->context = gzochid_api_ensure_current_application_context ();
      tx_context->identity = calloc (1, sizeof (gzochid_auth_identity));
      tx_context->identity->name = "[SYSTEM]";
      tx_context->participants = SCM_EOL;

      gzochid_transaction_join (&tx_participant, tx_context);
    }

  /* The transaction should remain active after we join it. It's not safe to
     assert this condition earlier since in certain contexts (e.g. testing) we
     may be the first to join the transaction, and the transaction is not
     considered active before the first participant is registered. */

  assert (gzochid_transaction_active ());
  
  /* Ensure that the transaction is in a good state and hasn't timed out. */

  gzochid_api_check_transaction ();

  /* Silently ignore multiple joins of the same (by `eq') participant. */

  if (scm_is_false (scm_memq (participant, tx_context->participants)))
    {
      tx_context->participants =
	scm_cons (participant, tx_context->participants);
      scm_gc_protect_object (tx_context->participants);
    }

  return SCM_UNSPECIFIED;
}

/* The snarf-definition for primitive_abort_transaction, which replaces the
   default `#f' definition of `primitive-abort-transaction' in `(gzochi tx)'.

   Expects a `gzochi:transaction-participant' record, which should have been
   type-checked by the Scheme-side implementation of `gzochi:join-transaction'.
   This participant must have previously joined the transaction via a call to
   `gzochi:join-transaction'. Optionally, a retry flag may also be provided, 
   which is passed to `gzochid_transaction_mark_for_rollback'.
*/

SCM_DEFINE (primitive_abort_transaction, "primitive-abort-transaction", 1, 1, 0,
	    (SCM participant, SCM retry), "Abort the current transaction.")
{
  gzochid_api_tx_context *tx_context = NULL;

  /* The transaction isn't active, that means nobody (not even the 
     meta-participant) has joined it. */
  
  if (!gzochid_transaction_active ()
      || (tx_context = gzochid_transaction_context (&tx_participant)) == NULL
      || scm_is_false (scm_memq (participant, tx_context->participants)))

    gzochid_guile_r6rs_raise 
      (scm_call_2 
       (scm_condition,
	scm_call_0 (scm_make_assertion_violation),
	scm_call_1 (scm_make_message_condition,
		    scm_from_locale_string 
		    ("Attempted to abort transaction without joining."))));

  else gzochid_transaction_mark_for_rollback 
	 (&tx_participant, scm_is_true (retry));

  return SCM_UNSPECIFIED;
}

/* Some copy-pasted glue code to help create protected bindings. */

static void 
bind_scm (char *module, SCM *binding, char *name)
{
  SCM var = scm_module_variable
    (scm_c_resolve_module (module), scm_from_locale_symbol (name));

  if (scm_is_false (var))
    g_error ("Missing Scheme binding for `%s'. Aborting...", name);

  *binding = scm_variable_ref (var);
  scm_gc_protect_object (*binding);
}

/* The snarf-initialization function. */

void
gzochid_api_tx_init (void)
{
  /* Save the current module... */

  SCM current_module = scm_current_module ();
  SCM gzochi_tx = scm_c_resolve_module ("gzochi tx");
  scm_set_current_module (gzochi_tx);

  #include "tx.x"
  
  /* ...and restore it post-snarf. */
  
  scm_set_current_module (current_module);

  bind_scm ("rnrs conditions", &scm_condition, "condition");
  bind_scm ("rnrs conditions", &scm_make_assertion_violation, 
	    "make-assertion-violation");
  bind_scm ("rnrs conditions", &scm_make_message_condition, 
	    "make-message-condition");

  bind_scm ("gzochi tx", &scm_participant_prepare, 
	    "gzochi:transaction-participant-prepare");
  bind_scm ("gzochi tx", &scm_participant_commit, 
	    "gzochi:transaction-participant-commit");
  bind_scm ("gzochi tx", &scm_participant_rollback, 
	    "gzochi:transaction-participant-rollback");
}
