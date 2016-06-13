/* api/test-tx.c: Test routines for api/tx.c in gzochid.
 * Copyright (C) 2016 Julian Graham
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
#include <stddef.h>
#include <stdlib.h>

#include "../app.h"
#include "../channel.h"
#include "../guile.h"
#include "../gzochid-auth.h"
#include "../tx.h"

#include "tx.h"
#include "util.h"

/* Fake implementation to avoid having to pull in `channel.o'. */

const char *
gzochid_channel_name (gzochid_channel *channel)
{
  return NULL;
}

extern void
primitive_join_transaction (SCM);

extern void
primitive_abort_transaction (SCM, SCM);

struct test_context
{
  gzochid_application_context *app_context;
  gzochid_auth_identity *identity;
};

static SCM scm_make_transaction_participant;

static int prepare_count = 0;
static int commit_count = 0;
static int rollback_count = 0;

static void
reset_counters ()
{
  prepare_count = 0;
  commit_count = 0;
  rollback_count = 0;
}

static SCM
prepare_true ()
{
  prepare_count++;
  return SCM_BOOL_T;
}

static SCM
prepare_false ()
{
  prepare_count++;
  return SCM_BOOL_F;
}

static SCM
commit ()
{
  commit_count++;
  return SCM_UNSPECIFIED;
}

static SCM
rollback ()
{
  rollback_count++;
  return SCM_UNSPECIFIED;
}

static SCM
make_test_participant ()
{
  return scm_call_3 
    (scm_make_transaction_participant, 
     scm_c_make_gsubr ("prepare_true", 0, 0, 0, prepare_true),
     scm_c_make_gsubr ("commit", 0, 0, 0, commit),
     scm_c_make_gsubr ("rollback", 0, 0, 0, rollback));
}

static void *
test_primitive_join_transaction_inner (void *data)
{
  gzochid_transaction_timing timing;
  SCM participant = make_test_participant ();

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);

  primitive_join_transaction (participant);
  gzochid_transaction_end ();
  return NULL;
}

static void
test_primitive_join_transaction (gconstpointer data)
{
  const struct test_context *t = data;

  gzochid_with_application_context 
    (t->app_context, t->identity, test_primitive_join_transaction_inner, NULL);
}

SCM
abort_transaction_body (void *data)
{
  SCM participant = data;
  primitive_abort_transaction (participant, SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}

SCM
abort_transaction_handler (void *data, SCM key, SCM args)
{
  gboolean *exception_thrown = data;

  *exception_thrown = TRUE;
  return SCM_UNSPECIFIED;
}

static void *
test_primitive_abort_transaction_inner (void *data)
{
  gzochid_transaction_timing timing;
  gboolean exception_thrown = FALSE;
  SCM participant = make_test_participant (); 

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);

  scm_c_catch 
    (SCM_BOOL_T, abort_transaction_body, participant, abort_transaction_handler,
     &exception_thrown, NULL, NULL);

  g_assert (exception_thrown);

  primitive_join_transaction (participant);
  primitive_abort_transaction (participant, SCM_BOOL_F);
  g_assert (gzochid_transaction_rollback_only ());
  gzochid_transaction_end ();

  return NULL;
}

static void
test_primitive_abort_transaction (gconstpointer data)
{
  const struct test_context *t = data;

  gzochid_with_application_context 
    (t->app_context, t->identity, test_primitive_abort_transaction_inner, NULL);
}

static void *
test_prepare_inner (void *data)
{
  gzochid_transaction_timing timing;
  SCM participant1 = make_test_participant (); 
  SCM participant2 = make_test_participant (); 

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);
  
  primitive_join_transaction (participant1);
  primitive_join_transaction (participant2);

  gzochid_transaction_end ();

  g_assert_cmpint (prepare_count, ==, 2);

  return NULL;
}

static void
test_prepare (gconstpointer data)
{
  const struct test_context *t = data;

  reset_counters ();

  gzochid_with_application_context 
    (t->app_context, t->identity, test_prepare_inner, NULL);
}

static void *
test_prepare_fail_inner (void *data)
{
  gzochid_transaction_timing timing;
  SCM participant1 = make_test_participant (); 
  SCM participant2 = scm_call_3 
    (scm_make_transaction_participant, 
     scm_c_make_gsubr ("prepare_false", 0, 0, 0, prepare_false),
     scm_c_make_gsubr ("commit", 0, 0, 0, commit),
     scm_c_make_gsubr ("rollback", 0, 0, 0, rollback));

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);
  
  primitive_join_transaction (participant1);
  primitive_join_transaction (participant2);

  gzochid_transaction_end ();

  g_assert_cmpint (prepare_count, ==, 1);

  return NULL;
}

static void
test_prepare_fail (gconstpointer data)
{
  const struct test_context *t = data;

  reset_counters ();

  gzochid_with_application_context 
    (t->app_context, t->identity, test_prepare_fail_inner, NULL);
}

static void *
test_commit_inner (void *data)
{
  gzochid_transaction_timing timing;
  SCM participant1 = make_test_participant (); 
  SCM participant2 = make_test_participant (); 

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);
  
  primitive_join_transaction (participant1);
  primitive_join_transaction (participant2);

  gzochid_transaction_end ();

  g_assert_cmpint (commit_count, ==, 2);

  return NULL;
}

static void
test_commit (gconstpointer data)
{
  const struct test_context *t = data;

  reset_counters ();

  gzochid_with_application_context 
    (t->app_context, t->identity, test_commit_inner, NULL);
}

static void *
test_rollback_inner (void *data)
{
  gzochid_transaction_timing timing;
  SCM participant1 = make_test_participant (); 
  SCM participant2 = make_test_participant (); 

  timing.timeout = NULL;
  gzochid_transaction_begin (&timing);
  
  primitive_join_transaction (participant1);
  primitive_join_transaction (participant2);
  primitive_abort_transaction (participant1, SCM_BOOL_F);

  gzochid_transaction_end ();

  g_assert_cmpint (rollback_count, ==, 2);

  return NULL;
}

static void
test_rollback (gconstpointer data)
{
  const struct test_context *t = data;

  reset_counters ();

  gzochid_with_application_context 
    (t->app_context, t->identity, test_rollback_inner, NULL);
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

static void 
inner_main (void *data, int argc, char *argv[])
{
  struct test_context t;

  g_test_init (&argc, &argv, NULL);

  gzochid_api_tx_init ();
  gzochid_api_util_init ();
  gzochid_guile_init ();

  bind_scm ("gzochi tx", &scm_make_transaction_participant,
	    "gzochi:make-transaction-participant");
  
  t.app_context = gzochid_application_context_new ();
  t.app_context->identity_cache = gzochid_auth_identity_cache_new ();
  t.app_context->descriptor = 
    calloc (1, sizeof (gzochid_application_descriptor));
  t.app_context->deployment_root = "";

  t.identity = gzochid_auth_identity_new ("[TEST]");
  
  g_test_add_data_func ("/api/tx/primitive-join-transaction", &t,
			test_primitive_join_transaction);
  g_test_add_data_func ("/api/tx/primitive-abort-transaction", &t,
			test_primitive_abort_transaction);

  g_test_add_data_func ("/api/tx/prepare", &t, test_prepare);
  g_test_add_data_func ("/api/tx/prepare/fail", &t, test_prepare_fail);
  g_test_add_data_func ("/api/tx/commit", &t, test_commit);
  g_test_add_data_func ("/api/tx/rollback", &t, test_rollback);

  g_test_run ();

  gzochid_auth_identity_unref (t.identity);
  free (t.app_context->descriptor);
  gzochid_auth_identity_cache_destroy (t.app_context->identity_cache);
  gzochid_application_context_free (t.app_context);
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);
  return 0;
}
