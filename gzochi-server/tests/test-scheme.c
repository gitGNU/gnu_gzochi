/* test-scheme.c: Test routines for scheme.c in gzochid.
 * Copyright (C) 2014 Julian Graham
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
#include <gmp.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "auth.h"
#include "mock-data.h"
#include "scheme.h"

static SCM raise_condition (SCM session)
{
  scm_throw (scm_from_locale_symbol ("test-tag"), SCM_EOL);
  return SCM_BOOL_F;
}

static gzochid_application_callback *
make_callback (char *name, GList *module)
{
  mpz_t scm_oid;

  mpz_init (scm_oid);
  mpz_set_si (scm_oid, -1);

  return gzochid_application_callback_new 
    ("logged-in", g_list_append (NULL, "test"), scm_oid);
}

static void 
test_logged_in_worker_throws_exception ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_application_descriptor *descriptor = 
    calloc (1, sizeof (gzochid_application_descriptor));
  gzochid_auth_identity *identity = calloc (1, sizeof (gzochid_auth_identity));
  gzochid_client_session *session = NULL;

  SCM module = scm_c_resolve_module ("test");
  SCM logged_in = scm_c_make_gsubr ("logged-in", 1, 0, 0, raise_condition);

  scm_c_module_define (module, "logged-in", logged_in);

  descriptor->logged_in = make_callback 
    ("logged-in", g_list_append (NULL, "test"));
  context->descriptor = descriptor;

  identity->name = "[TEST]";
  session = gzochid_client_session_new (identity);

  char *oid_str = NULL;

  { mpz_t oid;

    mpz_init (oid);
    gzochid_test_mock_data_store 
      (context, &gzochid_client_session_serialization, session, oid);
    oid_str = mpz_get_str (NULL, 16, oid);
    mpz_clear (oid);
  }
  
  gzochid_scheme_application_logged_in_worker (context, identity, oid_str);
}

static void
test_serialization ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GString *str = g_string_new ("");
  gpointer data = NULL;

  gzochid_util_serialize_string ("test-unknown-type", str);
  data = gzochid_scheme_data_serialization.deserializer (context, str, NULL);

  g_assert (data == SCM_BOOL_F);
  g_assert (gzochid_transaction_rollback_only ());
}

void gzochid_api_channel_init () { }
void gzochid_api_data_init () { }
void gzochid_api_log_init () { }
void gzochid_api_session_init () { }
void gzochid_api_task_init () { }
void gzochid_api_util_init () { }

static void
inner_main (void *data, int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/scheme/serialization/exception", test_serialization);
  g_test_add_func ("/scheme/worker/logged-in/exception", 
		   test_logged_in_worker_throws_exception);

  gzochid_guile_init ();
  gzochid_scheme_initialize_bindings ();
  gzochid_test_mock_data_initialize ();
  exit (g_test_run ());
}

int main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);

  return 0;
}
