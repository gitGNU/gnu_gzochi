/* test-scheme-task.c: Test routines for scheme-task.c in gzochid.
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
#include <gmp.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "app-task.h"
#include "context.h"
#include "game.h"
#include "guile.h"
#include "gzochid-auth.h"
#include "scheme.h"
#include "scheme-task.h"
#include "storage-mem.h"
#include "session.h"
#include "tx.h"

struct test_scheme_task_fixture
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  gzochid_storage_engine_interface *storage_interface;
};

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static void
test_scheme_task_fixture_setup (struct test_scheme_task_fixture *fixture,
				gconstpointer user_data)
{
  gzochid_context *base_context = NULL;
  gzochid_game_context *game_context = gzochid_game_context_new (NULL);
  gzochid_storage_engine *storage_engine = 
    malloc (sizeof (gzochid_storage_engine));

  fixture->context = gzochid_application_context_new ();
  fixture->context->deployment_root = "/tmp";
  fixture->context->descriptor = 
    calloc (1, sizeof (gzochid_application_descriptor));
  fixture->context->identity_cache = gzochid_auth_identity_cache_new ();

  fixture->identity = gzochid_auth_identity_new ("[TEST]");
  
  base_context = (gzochid_context *) fixture->context;
  base_context->parent = (gzochid_context *) game_context;

  game_context->storage_engine = storage_engine;
  storage_engine->interface = &gzochid_storage_engine_interface_mem;
  
  fixture->storage_interface = storage_engine->interface;
  
  fixture->context->storage_context =
    fixture->storage_interface->initialize ("/tmp");
  fixture->context->meta = fixture->storage_interface->open
    (fixture->context->storage_context, "/tmp/meta", GZOCHID_STORAGE_CREATE);
  fixture->context->oids = fixture->storage_interface->open
    (fixture->context->storage_context, "/tmp/oids", GZOCHID_STORAGE_CREATE);
  fixture->context->names = fixture->storage_interface->open
    (fixture->context->storage_context, "/tmp/names", GZOCHID_STORAGE_CREATE);
}

static void
test_scheme_task_fixture_teardown (struct test_scheme_task_fixture *fixture,
				   gconstpointer user_data)
{
  gzochid_game_context *game_context = (gzochid_game_context *)
    ((gzochid_context *) fixture->context)->parent;
    
  free (fixture->context->descriptor);
  gzochid_auth_identity_cache_destroy (fixture->context->identity_cache);

  fixture->storage_interface->close_store (fixture->context->meta);
  fixture->storage_interface->close_store (fixture->context->oids);
  fixture->storage_interface->close_store (fixture->context->names);
  fixture->storage_interface->close_context (fixture->context->storage_context);

  free (game_context->storage_engine);
  gzochid_game_context_free (game_context);
  gzochid_application_context_free (fixture->context);
  gzochid_auth_identity_unref (fixture->identity);
}

static char *
persist_client_session (struct test_scheme_task_fixture *fixture,
			gzochid_client_session *session)
{
  mpz_t oid;
  char *oid_str = NULL;
  GString *out = g_string_new (NULL);
  
  gzochid_client_session_serialization.serializer 
    (fixture->context, session, out, NULL);
  
  mpz_init (oid);
  mpz_set_ui (oid, 1);
  oid_str = mpz_get_str (NULL, 16, oid);
  
  fixture->storage_interface->put
    (fixture->context->oids, oid_str, strlen (oid_str) + 1, out->str, out->len);
  gzochid_data_set_binding_to_oid (fixture->context, "s.session.1", oid, NULL);
  
  mpz_clear (oid);
  g_string_free (out, TRUE);

  return oid_str;
}

static void 
test_disconnected_worker_no_handler_inner (gpointer data)
{
  struct test_scheme_task_fixture *fixture = data;
  gzochid_client_session *session =
    gzochid_client_session_new (fixture->identity);
  char *oid_str = persist_client_session (fixture, session);

  g_test_log_set_fatal_handler (ignore_warnings, NULL);
  
  gzochid_scheme_application_disconnected_worker
    (fixture->context, fixture->identity, oid_str);

  g_assert_null
    (fixture->storage_interface->get
     (fixture->context->oids, oid_str, strlen (oid_str) + 1, NULL));

  free (oid_str);
  gzochid_client_session_free (session);
}

static void 
test_disconnected_worker_no_handler (struct test_scheme_task_fixture *fixture,
				     gconstpointer user_data)
{
  gzochid_transaction_execute
    (test_disconnected_worker_no_handler_inner, fixture);
}

static SCM 
raise_condition (SCM session)
{
  scm_throw (scm_from_locale_symbol ("test-tag"), SCM_EOL);
  return SCM_BOOL_F;
}

static SCM 
return_unspecified (SCM session)
{
  return SCM_UNSPECIFIED;
}

static gzochid_application_callback *
make_callback (char *name, GList *module)
{
  mpz_t scm_oid;
  gzochid_application_callback *callback = NULL;
  
  mpz_init (scm_oid);
  mpz_set_si (scm_oid, -1);

  callback = gzochid_application_callback_new (name, module, scm_oid);
  mpz_clear (scm_oid);

  return callback;
}

static void 
test_logged_in_worker_throws_exception_inner (gpointer data)
{
  struct test_scheme_task_fixture *fixture = data;
  gzochid_client_session *session =
    gzochid_client_session_new (fixture->identity);
  char *oid_str = persist_client_session (fixture, session);
  
  SCM module = scm_c_resolve_module ("test");
  SCM logged_in = scm_c_make_gsubr 
    ("logged-in-exception", 1, 0, 0, raise_condition);
  GList *test_module = g_list_append (NULL, "test");

  scm_c_module_define (module, "logged-in-exception", logged_in);

  fixture->context->descriptor->logged_in = make_callback 
    ("logged-in-exception", test_module);
  
  gzochid_scheme_application_logged_in_worker
    (fixture->context, fixture->identity, oid_str);

  free (oid_str);
  g_list_free (test_module);
  gzochid_application_callback_free (fixture->context->descriptor->logged_in);
  gzochid_client_session_free (session);
}

static void 
test_logged_in_worker_throws_exception
(struct test_scheme_task_fixture *fixture, gconstpointer user_data)
{
  gzochid_transaction_execute 
    (test_logged_in_worker_throws_exception_inner, fixture);
}

static void 
test_logged_in_worker_returns_unspecified_inner (gpointer data)
{
  struct test_scheme_task_fixture *fixture = data;
  gzochid_client_session *session =
    gzochid_client_session_new (fixture->identity);
  char *oid_str = persist_client_session (fixture, session);
    
  SCM module = scm_c_resolve_module ("test");
  SCM logged_in = scm_c_make_gsubr 
    ("logged-in-unspecified", 1, 0, 0, return_unspecified);
  GList *test_module = g_list_append (NULL, "test");

  scm_c_module_define (module, "logged-in-unspecified", logged_in);

  fixture->context->descriptor->logged_in = make_callback 
    ("logged-in-unspecified", test_module);

  gzochid_scheme_application_logged_in_worker
    (fixture->context, fixture->identity, oid_str);

  free (oid_str);
  g_list_free (test_module);
  gzochid_application_callback_free (fixture->context->descriptor->logged_in);
  gzochid_client_session_free (session);
}

static void 
test_logged_in_worker_returns_unspecified
(struct test_scheme_task_fixture *fixture, gconstpointer data)
{
  gzochid_transaction_execute 
    (test_logged_in_worker_returns_unspecified_inner, fixture);
}

static void 
test_ready_throws_exception ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_application_descriptor *descriptor = 
    calloc (1, sizeof (gzochid_application_descriptor));
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("[TEST]");
  
  SCM module = scm_c_resolve_module ("test");
  SCM ready = scm_c_make_gsubr ("ready", 1, 0, 0, raise_condition);
  GList *test_module = g_list_append (NULL, "test");

  GError *tmp_err = NULL;

  scm_c_module_define (module, "ready", ready);

  descriptor->properties = g_hash_table_new (g_str_hash, g_str_equal);
  descriptor->ready = make_callback ("ready", test_module);

  context->deployment_root = "/";
  context->descriptor = descriptor;

  gzochid_scheme_application_ready (context, identity, &tmp_err);

  g_assert_error (tmp_err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED);

  g_error_free (tmp_err);
  g_list_free (test_module);
  gzochid_application_callback_free (descriptor->ready);
  gzochid_auth_identity_unref (identity);
  g_hash_table_destroy (descriptor->properties);
  free (descriptor);
  gzochid_application_context_free (context);
}

void gzochid_api_channel_init () { }
void gzochid_api_data_init () { }
void gzochid_api_log_init () { }
void gzochid_api_session_init () { }
void gzochid_api_task_init () { }
void gzochid_api_tx_init () { }
void gzochid_api_util_init () { }

static void
inner_main (void *data, int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/scheme/worker/disconnected/no-handler", struct test_scheme_task_fixture,
     NULL, test_scheme_task_fixture_setup, test_disconnected_worker_no_handler,
     test_scheme_task_fixture_teardown);
  g_test_add
    ("/scheme/worker/logged-in/exception", struct test_scheme_task_fixture,
     NULL, test_scheme_task_fixture_setup,
     test_logged_in_worker_throws_exception, test_scheme_task_fixture_teardown);
  g_test_add
    ("/scheme/worker/logged-in/unspecified", struct test_scheme_task_fixture,
     NULL, test_scheme_task_fixture_setup,
     test_logged_in_worker_returns_unspecified,
     test_scheme_task_fixture_teardown);

  g_test_add_func ("/scheme/ready/exception", test_ready_throws_exception);

  gzochid_guile_init ();
  gzochid_scheme_initialize_bindings ();
  gzochid_scheme_task_initialize_bindings ();
  exit (g_test_run ());
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);

  return 0;
}
