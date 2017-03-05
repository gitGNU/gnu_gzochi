/* test-sessionclient.c: Tests for sessionclient.c in gzochid.
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>

#include "game-protocol.h"
#include "game.h"
#include "resolver.h"
#include "sessionclient.h"
#include "socket.h"

struct _GzochidGameServer
{
  GObject parent_instance;

  GHashTable *applications;
};

G_DEFINE_TYPE (GzochidGameServer, gzochid_game_server, G_TYPE_OBJECT);

static void
game_server_finalize (GObject *obj)
{
  GzochidGameServer *self = GZOCHID_GAME_SERVER (obj);

  g_hash_table_destroy (self->applications);

  G_OBJECT_CLASS (gzochid_game_server_parent_class)->finalize (obj);
}

static void
gzochid_game_server_class_init (GzochidGameServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = game_server_finalize;
}

static gzochid_application_context *
application_context_new ()
{
  gzochid_application_context *app_context =
    calloc (1, sizeof (gzochid_application_context));

  app_context->oids_to_clients = g_hash_table_new (g_int64_hash, g_int64_equal);
  
  return app_context;
}

static void
application_context_free (gpointer data)
{
  gzochid_application_context *app_context = data;

  g_hash_table_destroy (app_context->oids_to_clients);

  free (app_context);
}

static void
gzochid_game_server_init (GzochidGameServer *self)
{
  self->applications = g_hash_table_new_full
    (g_str_hash, g_str_equal, NULL, (GDestroyNotify) application_context_free);
}

gzochid_application_context *
gzochid_game_server_lookup_application (GzochidGameServer *game_server,
					const char *app)
{
  return g_hash_table_lookup (game_server->applications, app);
}

struct _sessionclient_fixture
{
  GzochidGameServer *game_server;
  gzochid_reconnectable_socket *socket;
  GzochidSessionClient *sessionclient;
};

typedef struct _sessionclient_fixture sessionclient_fixture;

struct _gzochid_reconnectable_socket
{
  GByteArray *bytes_received;
};

gzochid_reconnectable_socket *
gzochid_reconnectable_socket_new ()
{
  gzochid_reconnectable_socket *sock =
    malloc (sizeof (gzochid_reconnectable_socket));

  sock->bytes_received = g_byte_array_new ();
  
  return sock;
}

void
gzochid_reconnectable_socket_free (gzochid_reconnectable_socket *sock)
{
  g_byte_array_unref (sock->bytes_received);
  free (sock);
}

void
gzochid_reconnectable_socket_write (gzochid_reconnectable_socket *sock,
				    unsigned char *buf, size_t len)
{
  g_byte_array_append (sock->bytes_received, buf, len);
}

struct _gzochid_game_client
{
  gboolean disconnected;
  GByteArray *bytes_received;
};

typedef struct _gzochid_game_client gzochid_game_client;

static gzochid_game_client *
gzochid_game_client_new ()
{
  gzochid_game_client *client = malloc (sizeof (gzochid_game_client));

  client->disconnected = FALSE;
  client->bytes_received = g_byte_array_new ();
  
  return client;
}

static void
gzochid_game_client_free (gzochid_game_client *client)
{
  g_byte_array_unref (client->bytes_received);
  free (client);
}

void
gzochid_game_client_disconnect (gzochid_game_client *client)
{
  client->disconnected = TRUE;  
}

void
gzochid_game_client_send (gzochid_game_client *client, const unsigned char *msg,
			  unsigned short len)
{
  g_byte_array_append (client->bytes_received, msg, len);
}

static void
sessionclient_fixture_setup (sessionclient_fixture *fixture,
			     gconstpointer user_data)
{
  fixture->game_server = gzochid_resolver_require
    (GZOCHID_TYPE_GAME_SERVER, NULL);  
  fixture->socket = gzochid_reconnectable_socket_new ();

  fixture->sessionclient = g_object_new
    (GZOCHID_TYPE_SESSION_CLIENT,
     "game-server", fixture->game_server,
     "reconnectable-socket", fixture->socket,
     NULL);
}

static void
sessionclient_fixture_teardown (sessionclient_fixture *fixture,
				gconstpointer user_data)
{
  g_object_unref (fixture->game_server);
  gzochid_reconnectable_socket_free (fixture->socket);
  g_object_unref (fixture->sessionclient);
}

static void
test_session_connected (sessionclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x0d\x60test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  
  gzochid_sessionclient_session_connected (fixture->sessionclient, "test", 1);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));
  
  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_session_disconnected (sessionclient_fixture *fixture,
			   gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x0d\x61test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  
  gzochid_sessionclient_session_disconnected
    (fixture->sessionclient, "test", 1);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));
  
  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_disconnect_from (sessionclient_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x0d\x62test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  
  gzochid_sessionclient_relay_disconnect_from
    (fixture->sessionclient, "test", 1);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));
  
  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_message_from (sessionclient_fixture *fixture,
			 gconstpointer user_data)
{
  GBytes *msg = g_bytes_new ("foo", 4);
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x13\x64test\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04""foo", 22);
  
  gzochid_sessionclient_relay_message_from
    (fixture->sessionclient, "test", 1, msg);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));

  g_bytes_unref (msg);
  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_disconnect_to_success (sessionclient_fixture *fixture,
				  gconstpointer user_data)
{
  GError *err = NULL;
  gzochid_game_client *client = gzochid_game_client_new ();
  gzochid_application_context *app_context = application_context_new ();
  guint64 session_id = 1;
  
  g_hash_table_insert (fixture->game_server->applications, "test", app_context);
  g_hash_table_insert (app_context->oids_to_clients, &session_id, client);
  
  gzochid_sessionclient_relay_disconnect_to
    (fixture->sessionclient, "test", 1, &err);

  g_assert_no_error (err);
  g_assert (client->disconnected);

  gzochid_game_client_free (client);
}

static void
test_relay_disconnect_to_failure (sessionclient_fixture *fixture,
				  gconstpointer user_data)
{
  GError *err = NULL;
  gzochid_game_client *client = gzochid_game_client_new ();
  
  gzochid_sessionclient_relay_disconnect_to
    (fixture->sessionclient, "test", 1, &err);

  g_assert_error
    (err, GZOCHID_SESSIONCLIENT_ERROR, GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED);
  g_error_free (err);

  gzochid_game_client_free (client);
}

static void
test_relay_message_to_success (sessionclient_fixture *fixture,
			       gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *actual = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  gzochid_game_client *client = gzochid_game_client_new ();
  gzochid_application_context *app_context = application_context_new ();
  guint64 session_id = 1;

  g_hash_table_insert (fixture->game_server->applications, "test", app_context);
  g_hash_table_insert (app_context->oids_to_clients, &session_id, client);
  
  gzochid_sessionclient_relay_message_to
    (fixture->sessionclient, "test", 1, msg, &err);
  
  actual = g_bytes_new
    (client->bytes_received->data, client->bytes_received->len);

  g_assert (g_bytes_equal (actual, msg));
  
  g_bytes_unref (actual);
  g_bytes_unref (msg);
  gzochid_game_client_free (client);
}

static void
test_relay_message_to_failure (sessionclient_fixture *fixture,
			       gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  gzochid_game_client *client = gzochid_game_client_new ();
  
  gzochid_sessionclient_relay_message_to
    (fixture->sessionclient, "test", 1, msg, &err);

  g_assert_error
    (err, GZOCHID_SESSIONCLIENT_ERROR, GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED);
  g_error_free (err);

  g_bytes_unref (msg);
  gzochid_game_client_free (client);
}

int
main (int argc, char *argv[])
{  
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/sessionclient/session-connected", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_session_connected,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/session-disconnected", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_session_disconnected,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/relay-disconnect-from", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_disconnect_from,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/relay-message-from", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_message_from,
     sessionclient_fixture_teardown);
  
  g_test_add
    ("/sessionclient/relay-disconnect-to/success", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_disconnect_to_success,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/relay-disconnect-to/failure", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_disconnect_to_failure,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/relay-message-to/success", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_message_to_success,
     sessionclient_fixture_teardown);
  g_test_add
    ("/sessionclient/relay-message-to/failure", sessionclient_fixture, NULL,
     sessionclient_fixture_setup, test_relay_message_to_failure,
     sessionclient_fixture_teardown);  
  
  return g_test_run ();
}
