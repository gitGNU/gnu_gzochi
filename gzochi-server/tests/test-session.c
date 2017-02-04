/* test-session.c: Test routines for session.c in gzochid.
 * Copyright (C) 2017 Julian Graham
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

#include <arpa/inet.h>
#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <libguile.h>
#include <netinet/in.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "config.h"
#include "data.h"
#include "guile.h"
#include "meta-protocol.h"
#include "metaclient.h"
#include "resolver.h"
#include "scheme.h"
#include "scheme-task.h"
#include "session.h"
#include "socket.h"
#include "storage-mem.h"
#include "tx.h"
#include "util.h"

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
  context->deployment_root = "/";

  context->descriptor = g_object_new
    (GZOCHID_TYPE_APPLICATION_DESCRIPTOR, NULL);
  context->descriptor->name = strdup ("test");
  
  context->storage_engine_interface = &gzochid_storage_engine_interface_mem;
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
  g_object_unref (context->descriptor);

  if (context->metaclient != NULL)
    g_object_unref (context->metaclient);
  
  gzochid_storage_engine_interface_mem.close_store (context->meta);
  gzochid_storage_engine_interface_mem.close_store (context->oids);
  gzochid_storage_engine_interface_mem.close_store (context->names);

  gzochid_storage_engine_interface_mem.close_context (context->storage_context);

  gzochid_auth_identity_cache_destroy (context->identity_cache);
}

static void
persist_callback (gzochid_application_context *context,
		  gzochid_application_callback *callback, guint64 key)
{
  GByteArray *callback_str = g_byte_array_new ();
  SCM scm_callback = gzochid_scheme_create_callback
    (callback, SCM_BOOL_F, NULL);
  GError *err = NULL;
  gzochid_storage_transaction *tx = NULL;
  guint64 encoded_oid = gzochid_util_encode_oid (key);
  
  gzochid_scheme_data_serialization.serializer
    (context, scm_callback, callback_str, &err);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (context->storage_context);
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->oids, (char *) &encoded_oid, sizeof (guint64),
     (char *) callback_str->data, callback_str->len);
  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  callback->scm_oid = key;
  
  g_byte_array_unref (callback_str);
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

static gzochid_client_session *
create_session (gzochid_application_context *context,
		gzochid_auth_identity *identity, guint64 session_oid)
{
  GList *test_module = g_list_append (NULL, "test");
  guint64 encoded_session_oid = gzochid_util_encode_oid (session_oid);
  GString *binding = g_string_new ("s.session.");

  gzochid_client_session *session = gzochid_client_session_new (identity);
  GByteArray *serialized_session = g_byte_array_new ();

  gzochid_application_callback *disconnected_callback = NULL;
  gzochid_application_callback *received_message_callback = NULL;

  guint64 disconnected_callback_arg_oid, received_message_callback_arg_oid;
  
  gzochid_client_session_handler handler;   
  gzochid_storage_transaction *tx = NULL;
  
  received_message_callback = gzochid_application_callback_new
    ("received-message", test_module, received_message_callback_arg_oid);
  disconnected_callback = gzochid_application_callback_new
    ("disconnected", test_module, disconnected_callback_arg_oid);

  persist_callback (context, received_message_callback, 0);
  persist_callback (context, disconnected_callback, 1);
  
  handler = (gzochid_client_session_handler)
    { received_message_callback, disconnected_callback };
  gzochid_client_session_set_handler (session, &handler);

  gzochid_client_session_serialization.serializer
    (context, session, serialized_session, NULL);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (context->storage_context);

  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->oids, (char *) &encoded_session_oid, sizeof (guint64),
     (char *) serialized_session->data, serialized_session->len);

  g_string_append_printf (binding, "%" G_GUINT64_FORMAT, session_oid);
  
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, context->names, binding->str, 12, (char *) &encoded_session_oid,
     sizeof (guint64));

  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  g_byte_array_unref (serialized_session);
  
  gzochid_application_callback_free (received_message_callback);
  gzochid_application_callback_free (disconnected_callback);

  g_string_free (binding, TRUE);
  g_list_free (test_module);

  return session;
}

static void
test_sweep_client_session_rollback_nonretryable ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  gzochid_client_session *session = NULL;

  /* Context and test setup. */
  
  application_context_init (context);

  /* Execute the body of the test in a transaction. */

  session = create_session (context, identity, 2);

  gzochid_transaction_execute
    (sweep_client_session_rollback_nonretryable_transactional, context);

  /* Tear everything down. */

  gzochid_auth_identity_unref (identity);
  gzochid_client_session_free (session);
  
  application_context_clear (context);
  gzochid_application_context_free (context);
}

struct _metaclient_fixture
{
  GzochidResolutionContext *resolution_context;
  GzochidMetaClient *metaclient;

  GzochidSocketServer *socket_server;
  gzochid_server_socket *server_socket;
  
  GByteArray *bytes_received;
};

typedef struct _metaclient_fixture metaclient_fixture;

static gboolean
can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return TRUE;
}

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  metaclient_fixture *fixture = user_data;

  g_byte_array_append (fixture->bytes_received, buffer->data, buffer->len);
  g_main_loop_quit (fixture->socket_server->main_loop);

  return buffer->len;
}

static void
client_error (gpointer user_data)
{
}

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol test_client_protocol =
  { can_dispatch, client_dispatch, client_error, client_free };

static gzochid_client_socket *
server_accept (GIOChannel *channel, const char *desc, gpointer data)
{
  return gzochid_client_socket_new (channel, desc, test_client_protocol, data);
}

gzochid_server_protocol test_server_protocol = { server_accept };

static void
metaclient_fixture_setup (metaclient_fixture *fixture, gconstpointer user_data)
{
  struct sockaddr_in addr;
  size_t addrlen = sizeof (struct sockaddr_in);
  char *server_address = NULL;
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);

  fixture->resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
  
  gzochid_resolver_provide
    (fixture->resolution_context, G_OBJECT (configuration), NULL);
  
  fixture->socket_server = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->server_socket = gzochid_server_socket_new
    ("test", test_server_protocol, fixture);

  fixture->bytes_received = g_byte_array_new ();

  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname
    (fixture->server_socket, (struct sockaddr *) &addr, &addrlen);

  server_address = g_strdup_printf 
    ("%d.%d.%d.%d:%d",
     addr.sin_addr.s_addr & 0xff,
     (addr.sin_addr.s_addr & 0xff00) >> 8,
     (addr.sin_addr.s_addr & 0xff0000) >> 16,
     (addr.sin_addr.s_addr & 0xff000000) >> 24, htons (addr.sin_port));

  g_key_file_set_integer (key_file, "game", "thread_pool.max_threads", 1);
  g_key_file_set_boolean (key_file, "metaserver", "client.enabled", TRUE);
  g_key_file_set_value
    (key_file, "metaserver", "server.address", server_address);

  g_free (server_address);

  fixture->metaclient = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_META_CLIENT, NULL);
  
  gzochid_metaclient_start (fixture->metaclient, NULL);

  g_object_unref (configuration);
  g_key_file_unref (key_file);
}

static void
metaclient_fixture_teardown (metaclient_fixture *fixture,
			     gconstpointer user_data)
{
  gzochid_metaclient_stop (fixture->metaclient);

  g_object_unref (fixture->metaclient);
  g_object_unref (fixture->resolution_context);
  g_object_unref (fixture->socket_server);
  
  g_byte_array_unref (fixture->bytes_received);
}

static gzochid_client_session *
get_session (gzochid_application_context *context, guint64 session_oid)
{
  gzochid_data_managed_reference *ref = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, session_oid);

  return gzochid_data_dereference (ref, NULL);
}

static gboolean
exit_loop (gpointer user_data)
{
  metaclient_fixture *fixture = user_data;
  g_main_loop_quit (fixture->socket_server->main_loop);
  return FALSE;
}

static void
set_timeout (metaclient_fixture *fixture, guint interval)
{
  GSource *timeout = g_timeout_source_new (interval);

  g_source_set_callback (timeout, exit_loop, fixture, NULL);
  g_source_attach (timeout, fixture->socket_server->main_context);
}

static void
discard_login (metaclient_fixture *fixture)
{
  if (fixture->bytes_received->len >= 3
      && fixture->bytes_received->data[2] == GZOCHID_META_PROTOCOL_LOGIN)

      g_byte_array_remove_range
	(fixture->bytes_received, 0,
	 gzochi_common_io_read_short (fixture->bytes_received->data, 0) + 3);
}

static void
forward_disconnect_transactional (gpointer data)
{
  gzochid_application_context *context = data;
  gzochid_client_session *session = get_session (context, 2);

  gzochid_client_session_disconnect (context, session);
}

static void
test_metaclient_forward_disconnect (metaclient_fixture *fixture,
				    gconstpointer user_data)
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  gzochid_client_session *session = NULL;

  /* Context and test setup. */
  
  application_context_init (context);
  context->metaclient = g_object_ref (fixture->metaclient);
  
  /* Execute the body of the test in a transaction. */

  session = create_session (context, identity, 2);
  gzochid_transaction_execute (forward_disconnect_transactional, context);

  set_timeout (fixture, 1000);
  g_main_loop_run (fixture->socket_server->main_loop);

  discard_login (fixture);
  g_assert_cmpint (fixture->bytes_received->len, ==, 16);  
  g_assert
    (memcmp (fixture->bytes_received->data,
	     "\x00\x0d\x62test\x00\x00\x00\x00\x00\x00\x00\x00\x02", 16) == 0);
  
  /* Tear everything down. */
  
  gzochid_client_session_free (session);
  gzochid_auth_identity_unref (identity);

  application_context_clear (context);
  gzochid_application_context_free (context);
}

static void
forward_message_transactional (gpointer data)
{
  gzochid_application_context *context = data;
  gzochid_client_session *session = get_session (context, 2);

  gzochid_client_session_send_message (context, session, "foo", 4);
}

static void
test_metaclient_forward_message (metaclient_fixture *fixture,
				 gconstpointer user_data)
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  gzochid_client_session *session = NULL;
  
  /* Context and test setup. */
  
  application_context_init (context);
  context->metaclient = g_object_ref (fixture->metaclient);
  
  /* Execute the body of the test in a transaction. */

  session = create_session (context, identity, 2);
  gzochid_transaction_execute (forward_message_transactional, context);

  set_timeout (fixture, 1000);
  g_main_loop_run (fixture->socket_server->main_loop);

  discard_login (fixture);
  g_assert_cmpint (fixture->bytes_received->len, ==, 22);  
  g_assert (memcmp (fixture->bytes_received->data,
		    "\x00\x13\x64test\x00\x00\x00\x00\x00\x00\x00\x00\x02"
		    "\x00\x04""foo\x00", 22) == 0);

  /* Tear everything down. */

  gzochid_client_session_free (session);
  gzochid_auth_identity_unref (identity);

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
  g_test_add
    ("/session/metaclient/forward-disconnect", metaclient_fixture, NULL,
     metaclient_fixture_setup, test_metaclient_forward_disconnect,
     metaclient_fixture_teardown);
  g_test_add ("/session/metaclient/forward-message", metaclient_fixture, NULL,
	      metaclient_fixture_setup, test_metaclient_forward_message,
	      metaclient_fixture_teardown);
  
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
