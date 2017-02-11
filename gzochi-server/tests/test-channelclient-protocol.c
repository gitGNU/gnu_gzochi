/* test-channelclient-protocol.c: Tests for channelclient-protocol.c in gzochid.
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

#include "channelclient-protocol.h"
#include "channelclient.h"

struct _GzochidChannelClient
{
  GObject parent_instance;

  GList *activity_log;
};

G_DEFINE_TYPE (GzochidChannelClient, gzochid_channel_client, G_TYPE_OBJECT);

static void
channel_client_finalize (GObject *gobject)
{
  GzochidChannelClient *client = GZOCHID_CHANNEL_CLIENT (gobject);

  g_list_free_full (client->activity_log, g_free);
}

static void
gzochid_channel_client_class_init (GzochidChannelClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = channel_client_finalize;
}

static void
gzochid_channel_client_init (GzochidChannelClient *self)
{
}

void
gzochid_channelclient_relay_join_to (GzochidChannelClient *client,
				     const char *app, guint64 channel_oid,
				     guint64 session_oid, GError **err)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("JOIN %s/%" G_GUINT64_FORMAT ":%s/%" G_GUINT64_FORMAT,
		      app, channel_oid, app, session_oid));
}

void
gzochid_channelclient_relay_leave_to (GzochidChannelClient *client,
				      const char *app, guint64 channel_oid,
				      guint64 session_oid, GError **err)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("LEAVE %s/%" G_GUINT64_FORMAT ":%s/%" G_GUINT64_FORMAT,
		      app, channel_oid, app, session_oid));
}

void
gzochid_channelclient_relay_close_to (GzochidChannelClient *client,
				      const char *app, guint64 channel_oid,
				      GError **err)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("CLOSE %s/%" G_GUINT64_FORMAT, app, channel_oid));
}

void
gzochid_channelclient_relay_message_to (GzochidChannelClient *client,
					const char *app, guint64 channel_oid,
					GBytes *msg, GError **err)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("MESSAGE TO %s/%" G_GUINT64_FORMAT ":%s", app,
		      channel_oid, (char *) g_bytes_get_data (msg, NULL)));
}

static void
test_client_can_dispatch_true ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0d\x77test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  
  g_assert (gzochid_channelclient_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_can_dispatch_false ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0d\x77test\x00\x00\x00\x00\x00", 8);

  g_assert_false
    (gzochid_channelclient_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_join_to ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x15\x71test\x00\x00\x00\x00\x00\x00\x00\x00\x01"
     "\x00\x00\x00\x00\x00\x00\x00\x02", 24);
  
  g_assert_cmpint
    (gzochid_channelclient_client_protocol.dispatch (bytes, client), ==, 24);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr (client->activity_log->data, ==, "JOIN test/1:test/2");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_leave_to ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x15\x73test\x00\x00\x00\x00\x00\x00\x00\x00\x01"
     "\x00\x00\x00\x00\x00\x00\x00\x02", 24);
  
  g_assert_cmpint
    (gzochid_channelclient_client_protocol.dispatch (bytes, client), ==, 24);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr (client->activity_log->data, ==, "LEAVE test/1:test/2");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_close_to ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0d\x77test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  
  g_assert_cmpint
    (gzochid_channelclient_client_protocol.dispatch (bytes, client), ==, 16);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr (client->activity_log->data, ==, "CLOSE test/1");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_message_to ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x13\x75test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  g_byte_array_append (bytes, "\x00\x04""foo", 6);
  
  g_assert_cmpint
    (gzochid_channelclient_client_protocol.dispatch (bytes, client), ==, 22);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr (client->activity_log->data, ==, "MESSAGE TO test/1:foo");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_multiple ()
{
  GzochidChannelClient *client =
    g_object_new (GZOCHID_TYPE_CHANNEL_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0d\x77test\x00\x00\x00\x00\x00\x00\x00\x00\x01", 16);
  g_byte_array_append
    (bytes, "\x00\x0d\x77test\x00\x00\x00\x00\x00\x00\x00\x00\x02", 16);

  g_assert_cmpint
    (gzochid_channelclient_client_protocol.dispatch (bytes, client), ==, 32);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 2);
  g_assert_cmpstr (client->activity_log->data, ==, "CLOSE test/1");
  g_assert_cmpstr (client->activity_log->next->data, ==, "CLOSE test/2");
  
  g_byte_array_unref (bytes);
  g_object_unref (client);
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

  g_test_add_func ("/client/can_dispatch/true", test_client_can_dispatch_true);
  g_test_add_func
    ("/client/can_dispatch/false", test_client_can_dispatch_false);

  g_test_add_func ("/client/dispatch/one/join-to",
		   test_client_dispatch_one_join_to);
  g_test_add_func ("/client/dispatch/one/leave-to",
		   test_client_dispatch_one_leave_to);
  g_test_add_func ("/client/dispatch/one/close-to",
		   test_client_dispatch_one_close_to);
  g_test_add_func ("/client/dispatch/one/message-to",
		   test_client_dispatch_one_message_to);

  g_test_add_func ("/client/dispatch/multiple", test_client_dispatch_multiple);
  
  return g_test_run ();
}
