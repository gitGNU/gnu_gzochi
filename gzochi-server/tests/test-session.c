/* test-session.c: Test routines for session.c in gzochid.
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

#include "app.h"
#include "game.h"
#include "guile.h"
#include "scheme.h"
#include "scheme-task.h"
#include "session.h"
#include "storage-mem.h"
#include "tx.h"

static int
prepare (gpointer data)
{
  return FALSE;
}

static void
commit (gpointer data)
{
}

static void
rollback (gpointer data)
{
}

static gzochid_transaction_participant test_participant =
  { "test", prepare, commit, rollback };

static SCM
disconnected ()
{
  gzochid_transaction_join (&test_participant, NULL);
  gzochid_transaction_mark_for_rollback (&test_participant, FALSE);
}

static SCM
received_message (SCM msg)
{
}

static void
create_session_handlers (void *arg)
{
  scm_c_define_gsubr ("received-message", 1, 0, 0, received_message);
  scm_c_define_gsubr ("disconnected", 0, 0, 0, disconnected);
}

static void
application_context_init (gzochid_application_context *context)
{
  gzochid_context *base = (gzochid_context *) context;
  gzochid_game_context *game_context = gzochid_game_context_new (NULL);
  base->parent = (gzochid_context *) game_context;
  
  game_context->storage_engine = malloc (sizeof (gzochid_storage_engine));
  game_context->storage_engine->interface = 
    &gzochid_storage_engine_interface_mem;

  context->deployment_root = "/";
  
  context->storage_context = 
    gzochid_storage_engine_interface_mem.initialize ("/dev/null");
  context->meta = gzochid_storage_engine_interface_mem.open
    (context->storage_context, "/dev/null", 0);
  context->oids = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);
  context->names = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);

  context->identity_cache = gzochid_auth_identity_cache_new ();
}

static void
application_context_clear (gzochid_application_context *context)
{
  gzochid_context *base = (gzochid_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) base->parent;

  gzochid_storage_engine_interface_mem.close_store (context->meta);
  gzochid_storage_engine_interface_mem.close_store (context->oids);
  gzochid_storage_engine_interface_mem.close_store (context->names);

  gzochid_storage_engine_interface_mem.close_context (context->storage_context);

  free (game_context->storage_engine);
  gzochid_game_context_free (game_context);
  gzochid_auth_identity_cache_destroy (context->identity_cache);
}

static void
persist_callback (gzochid_application_context *context,
		  gzochid_application_callback *callback, char *key,
		  size_t key_len)
{
  GString *callback_str = g_string_new ("");
  SCM scm_callback = gzochid_scheme_create_callback
    (callback, SCM_BOOL_F, NULL);
  GError *err = NULL;
  gzochid_storage_transaction *tx = NULL;
  
  gzochid_scheme_data_serialization.serializer
    (context, scm_callback, callback_str, &err);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (context->storage_context);
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->oids, key, key_len, callback_str->str, callback_str->len);
  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);
  
  mpz_set_str (callback->scm_oid, key, 16);
  
  g_string_free (callback_str, TRUE);
  
}

static void
sweep_client_session_rollback_nonretryable_transactional (gpointer data)
{
  GError *err = NULL;
  gzochid_application_context *context = data;
    
  gzochid_sweep_client_sessions (context, &err);

  g_assert_error (err, GZOCHID_SESSION_ERROR, GZOCHID_SESSION_ERROR_DISCONNECT);
  g_error_free (err);
}

static void
test_sweep_client_session_rollback_nonretryable ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  gzochid_client_session *session = gzochid_client_session_new (identity);

  GList *test_module = g_list_append (NULL, "test");
  gzochid_application_callback *disconnected_callback = NULL;
  gzochid_application_callback *received_message_callback = NULL;
  mpz_t received_message_callback_arg_oid, disconnected_callback_arg_oid;
  gzochid_storage_transaction *tx = NULL;
  
  gzochid_client_session_handler handler; 
  
  GString *serialized_session = g_string_new ("");
  
  /* Context and test setup. */
  
  mpz_init (received_message_callback_arg_oid);
  mpz_init (disconnected_callback_arg_oid);
  
  application_context_init (context);

  received_message_callback = gzochid_application_callback_new
    ("received-message", test_module, received_message_callback_arg_oid);
  disconnected_callback = gzochid_application_callback_new
    ("disconnected", test_module, disconnected_callback_arg_oid);

  persist_callback (context, received_message_callback, "0", 2);
  persist_callback (context, disconnected_callback, "1", 2);
  
  handler = (gzochid_client_session_handler)
    { received_message_callback, disconnected_callback };
  session->handler = &handler;

  gzochid_client_session_serialization.serializer
    (context, session, serialized_session, NULL);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (context->storage_context);
  
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->oids, "2", 2, serialized_session->str,
     serialized_session->len);
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->names, "s.session.0", 12, "2", 2);

  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  /* Execute the body of the test in a transaction. */
  
  gzochid_transaction_execute
    (sweep_client_session_rollback_nonretryable_transactional, context);

  /* Tear everything down. */

  g_string_free (serialized_session, TRUE);
  
  mpz_clear (received_message_callback_arg_oid);
  mpz_clear (disconnected_callback_arg_oid);

  gzochid_application_callback_free (received_message_callback);
  gzochid_application_callback_free (disconnected_callback);
  
  g_list_free (test_module);

  gzochid_auth_identity_unref (identity);
  gzochid_client_session_free (session);
  
  application_context_clear (context);
  gzochid_application_context_free (context);
}

static void
inner_main (void *data, int argc, char *argv[])
{
  scm_c_define_module ("test", create_session_handlers, NULL);
  
  g_test_init (&argc, &argv, NULL);

  g_test_add_func
    ("/session/sweep/rollback/nonretryable",
     test_sweep_client_session_rollback_nonretryable);

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
