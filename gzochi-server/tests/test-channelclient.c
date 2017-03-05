/* test-channelclient.c: Tests for channelclient.c in gzochid.
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
#include <string.h>

#include "channel.h"
#include "channelclient.h"
#include "config.h"
#include "game.h"
#include "resolver.h"
#include "socket.h"
#include "util.h"

static gboolean channel_joined = FALSE;
static gboolean channel_left = FALSE;
static gboolean channel_closed = FALSE;
static GBytes *channel_msg = NULL;

static void
clear_channel_state (void)
{
  channel_joined = FALSE;
  channel_left = FALSE;
  channel_closed = FALSE;

  if (channel_msg != NULL)
    {
      g_bytes_unref (channel_msg);
      channel_msg = NULL;
    }
}

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

void
gzochid_channel_join_direct (gzochid_application_context *app_context,
			     guint64 channel_oid, guint64 session_oid,
			     GError **err)
{
  if (!g_hash_table_contains (app_context->oids_to_clients, &session_oid))
    g_set_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MAPPED,
		 "Unmapped session.");
  else
    {
      GSequence *sessions = g_hash_table_lookup
	(app_context->channel_oids_to_local_session_oids, &channel_oid);

      if (sessions != NULL &&
	  g_sequence_lookup (sessions, &session_oid,
			     gzochid_util_guint64_data_compare, NULL) != NULL)

	g_set_error (err, GZOCHID_CHANNEL_ERROR,
		     GZOCHID_CHANNEL_ERROR_NOT_MAPPED, "Already joined.");

      else channel_joined = TRUE;
    }
}

void
gzochid_channel_leave_direct (gzochid_application_context *app_context,
			      guint64 channel_oid, guint64 session_oid,
			      GError **err)
{
  if (!g_hash_table_contains (app_context->oids_to_clients, &session_oid))
    g_set_error (err, GZOCHID_CHANNEL_ERROR, GZOCHID_CHANNEL_ERROR_NOT_MAPPED,
		 "Unmapped session.");
  else
    {
      GSequence *sessions = g_hash_table_lookup
	(app_context->channel_oids_to_local_session_oids, &channel_oid);

      if (sessions == NULL ||
	  g_sequence_lookup (sessions, &session_oid,
			     gzochid_util_guint64_data_compare, NULL) == NULL)

	g_set_error (err, GZOCHID_CHANNEL_ERROR,
		     GZOCHID_CHANNEL_ERROR_NOT_MAPPED, "Not a member.");

      else channel_left = TRUE;
    }
}

void
gzochid_channel_close_direct (gzochid_application_context *app_context,
			      guint64 channel_oid)
{
  channel_closed = TRUE;
}

void
gzochid_channel_message_direct (gzochid_application_context *app_context,
				guint64 channel_oid, GBytes *msg)
{
  channel_msg = g_bytes_ref (msg);
}

GQuark
gzochid_channel_error_quark ()
{
  return g_quark_from_static_string ("gzochid-channel-error-quark");
}

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

  app_context->oids_to_clients = g_hash_table_new_full
    (g_int64_hash, g_int64_equal, g_free, NULL);
  app_context->channel_oids_to_local_session_oids = g_hash_table_new_full
    (g_int64_hash, g_int64_equal, g_free, (GDestroyNotify) g_sequence_free);
  
  return app_context;
}

static void
application_context_free (gpointer data)
{
  gzochid_application_context *app_context = data;

  g_hash_table_destroy (app_context->oids_to_clients);
  g_hash_table_destroy (app_context->channel_oids_to_local_session_oids);

  free (app_context);
}

static void
gzochid_game_server_init (GzochidGameServer *self)
{
  self->applications = g_hash_table_new_full
    (g_str_hash, g_str_equal, free, (GDestroyNotify) application_context_free);
}

gzochid_application_context *
gzochid_game_server_lookup_application (GzochidGameServer *game_server,
					const char *app)
{
  return g_hash_table_lookup (game_server->applications, app);
}

struct _channelclient_fixture
{
  GzochidGameServer *game_server;
  gzochid_reconnectable_socket *socket;
  GzochidChannelClient *channelclient;
};

typedef struct _channelclient_fixture channelclient_fixture;

static void
channelclient_fixture_setup (channelclient_fixture *fixture,
			     gconstpointer user_data)
{
  fixture->game_server = gzochid_resolver_require
    (GZOCHID_TYPE_GAME_SERVER, NULL);  
  fixture->socket = gzochid_reconnectable_socket_new ();

  fixture->channelclient = g_object_new
    (GZOCHID_TYPE_CHANNEL_CLIENT,
     "game-server", fixture->game_server,
     "reconnectable-socket", fixture->socket,
     NULL);
}

static void
channelclient_fixture_teardown (channelclient_fixture *fixture,
				gconstpointer user_data)
{
  g_object_unref (fixture->game_server);
  gzochid_reconnectable_socket_free (fixture->socket);
  g_object_unref (fixture->channelclient);
}

static void
create_app (channelclient_fixture *fixture, const char *app)
{
  g_hash_table_insert
    (fixture->game_server->applications, strdup (app),
     application_context_new ());
}

static void
register_client (channelclient_fixture *fixture, const char *app,
		 guint64 session_oid)
{
  gzochid_application_context *app_context = g_hash_table_lookup
    (fixture->game_server->applications, app);
  guint64 *session_oid_ptr = g_memdup (&session_oid, sizeof (guint64));
  
  g_hash_table_insert
    (app_context->oids_to_clients, session_oid_ptr, session_oid_ptr);
}

static void
add_to_channel (channelclient_fixture *fixture, const char *app,
		guint64 channel_oid, guint64 session_oid)
{
  gzochid_application_context *app_context = g_hash_table_lookup
    (fixture->game_server->applications, app);
  GSequence *sessions = g_sequence_new (g_free);

  g_sequence_append (sessions, g_memdup (&session_oid, sizeof (guint64)));
  
  g_hash_table_insert
    (app_context->channel_oids_to_local_session_oids,
     g_memdup (&channel_oid, sizeof (guint64)), sessions);
}

static void
test_relay_join_from (channelclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x15\x70test\x00\x00\x00\x00\x00\x00\x00\x00\x01"
     "\x00\x00\x00\x00\x00\x00\x00\x02", 24);

  gzochid_channelclient_relay_join_from (fixture->channelclient, "test", 1, 2);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));

  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_leave_from (channelclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x15\x72test\x00\x00\x00\x00\x00\x00\x00\x00\x01"
     "\x00\x00\x00\x00\x00\x00\x00\x02", 24);

  gzochid_channelclient_relay_leave_from (fixture->channelclient, "test", 1, 2);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));

  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_close_from (channelclient_fixture *fixture, gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *expected = g_bytes_new_static
    ("\x00\x0d\x76test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);

  gzochid_channelclient_relay_close_from (fixture->channelclient, "test", 1);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));

  g_bytes_unref (actual);
  g_bytes_unref (expected);
}

static void
test_relay_message_from (channelclient_fixture *fixture,
			 gconstpointer user_data)
{
  GBytes *actual = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  GBytes *expected = g_bytes_new_static
    ("\x00\x13\x74test\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04""foo\x00",
     22);

  gzochid_channelclient_relay_message_from
    (fixture->channelclient, "test", 1, msg);

  actual = g_bytes_new_static
    (fixture->socket->bytes_received->data,
     fixture->socket->bytes_received->len);

  g_assert (g_bytes_equal (expected, actual));

  g_bytes_unref (actual);
  g_bytes_unref (msg);
  g_bytes_unref (expected);
}

static void
test_relay_join_to_success (channelclient_fixture *fixture,
			    gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");
  register_client (fixture, "test", 2);

  gzochid_channelclient_relay_join_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert (channel_joined);
}

static void
test_relay_join_to_no_such_app (channelclient_fixture *fixture,
				gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();

  gzochid_channelclient_relay_join_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_error
    (err, GZOCHID_CHANNELCLIENT_ERROR, GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED);
  g_assert_false (channel_joined);

  g_clear_error (&err);
}

static void
test_relay_join_to_no_such_session (channelclient_fixture *fixture,
				    gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");

  gzochid_channelclient_relay_join_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert_false (channel_joined);
}

static void
test_relay_join_to_already_member (channelclient_fixture *fixture,
				   gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");
  register_client (fixture, "test", 2);
  add_to_channel (fixture, "test", 1, 2);

  gzochid_channelclient_relay_join_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert_false (channel_joined);  
}

static void
test_relay_leave_to_success (channelclient_fixture *fixture,
			     gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");
  register_client (fixture, "test", 2);
  add_to_channel (fixture, "test", 1, 2);
  
  gzochid_channelclient_relay_leave_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert (channel_left);
}

static void
test_relay_leave_to_no_such_app (channelclient_fixture *fixture,
				 gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();

  gzochid_channelclient_relay_leave_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_error
    (err, GZOCHID_CHANNELCLIENT_ERROR, GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED);
  g_assert_false (channel_left);
  
  g_clear_error (&err);
}

static void
test_relay_leave_to_no_such_session (channelclient_fixture *fixture,
				     gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");

  gzochid_channelclient_relay_leave_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert_false (channel_left);
}

static void
test_relay_leave_to_not_member (channelclient_fixture *fixture,
				gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");
  register_client (fixture, "test", 2);

  gzochid_channelclient_relay_leave_to
    (fixture->channelclient, "test", 1, 2, &err);

  g_assert_no_error (err);
  g_assert_false (channel_left);
}

static void
test_relay_close_to_success (channelclient_fixture *fixture,
			     gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();
  create_app (fixture, "test");

  gzochid_channelclient_relay_close_to
    (fixture->channelclient, "test", 1, &err);

  g_assert_no_error (err);
  g_assert (channel_closed);
}

static void
test_relay_close_to_no_such_app (channelclient_fixture *fixture,
				 gconstpointer user_data)
{
  GError *err = NULL;

  clear_channel_state ();

  gzochid_channelclient_relay_close_to
    (fixture->channelclient, "test", 1, &err);

  g_assert_error
    (err, GZOCHID_CHANNELCLIENT_ERROR, GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED);
  g_assert_false (channel_closed);

  g_clear_error (&err);
}

static void
test_relay_message_to_success (channelclient_fixture *fixture,
			       gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  
  clear_channel_state ();
  create_app (fixture, "test");
  
  gzochid_channelclient_relay_message_to
    (fixture->channelclient, "test", 1, msg, &err);

  g_assert_no_error (err);
  g_assert (g_bytes_equal (msg, channel_msg));

  g_bytes_unref (msg);
}

static void
test_relay_message_to_no_such_app (channelclient_fixture *fixture,
				   gconstpointer user_data)
{
  GError *err = NULL;
  GBytes *msg = g_bytes_new_static ("foo", 4);
  
  clear_channel_state ();

  gzochid_channelclient_relay_message_to
    (fixture->channelclient, "test", 1, msg, &err);

  g_assert_error
    (err, GZOCHID_CHANNELCLIENT_ERROR, GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED);
  g_assert_null (channel_msg);

  g_clear_error (&err);
  g_bytes_unref (msg);
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
    ("/channelclient/relay-join-from", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_join_from,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-leave-from", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_leave_from,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-close-from", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_close_from,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-message-from", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_message_from,
     channelclient_fixture_teardown);
  
  g_test_add
    ("/channelclient/relay-join-to/success", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_join_to_success,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-join-to/no-such-app", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_join_to_no_such_app,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-join-to/no-such-session", channelclient_fixture,
     NULL, channelclient_fixture_setup, test_relay_join_to_no_such_session,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-join-to/already-member", channelclient_fixture,
     NULL, channelclient_fixture_setup, test_relay_join_to_already_member,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-leave-to/success", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_leave_to_success,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-leave-to/no-such-app", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_leave_to_no_such_app,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-leave-to/no-such-session", channelclient_fixture,
     NULL, channelclient_fixture_setup, test_relay_leave_to_no_such_session,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-leave-to/not-member", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_leave_to_not_member,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-close-to/success", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_close_to_success,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-close-to/no-such-app", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_close_to_no_such_app,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-message-to/success", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_message_to_success,
     channelclient_fixture_teardown);
  g_test_add
    ("/channelclient/relay-message-to/no-such-app", channelclient_fixture, NULL,
     channelclient_fixture_setup, test_relay_message_to_no_such_app,
     channelclient_fixture_teardown);
  
  return g_test_run ();
}
