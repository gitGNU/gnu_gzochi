/* test-dataclient-protocol.c: Tests for dataclient-protocol.c in gzochid.
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
#include <glib-object.h>
#include <stddef.h>

#include "dataclient.h"
#include "dataclient-protocol.h"

struct _GzochidDataClient
{
  GObject parent_instance;

  GList *activity_log;
  gboolean connected;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

static void
gzochid_data_client_finalize (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);

  g_list_free_full (client->activity_log, g_free);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_data_client_finalize;
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  self->connected = TRUE;
}

void
gzochid_dataclient_nullify_connection (GzochidDataClient *client)
{
  client->connected = FALSE;
}

void
gzochid_dataclient_received_oids (GzochidDataClient *client,
				  gzochid_data_reserve_oids_response *response)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf
     ("RECEIVED OID BLOCK %s/%" G_GUINT64_FORMAT ":%hu", response->app,
      response->block.block_start, response->block.block_size));
}

void
gzochid_dataclient_received_value (GzochidDataClient *client,
				   gzochid_data_response *response)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("RECEIVED VALUE %s/%s:%s", response->app, response->store,
		      (char *) g_bytes_get_data (response->data, NULL)));
}

void
gzochid_dataclient_received_next_key (GzochidDataClient *client,
				      gzochid_data_response *response)
{
  client->activity_log = g_list_append
    (client->activity_log,
     g_strdup_printf ("RECEIVED KEY %s/%s:%s", response->app, response->store,
		      (char *) g_bytes_get_data (response->data, NULL)));
}

static void
test_client_can_dispatch_true ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x51test\x00oids\x00\x01\x00\x04""foo\x00", 20);
  
  g_assert_true
    (gzochid_dataclient_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_can_dispatch_false ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x51test\x00oids\x00\x01\x00\x04", 16);

  g_assert_false
    (gzochid_dataclient_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_oids_response ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0f\x50test\x00""\x00\x00\x00\x00\x00\x00\x00\x01\x00\x64",
     18);
  
  g_assert_cmpint
    (gzochid_dataclient_client_protocol.dispatch (bytes, client), ==, 18);
  
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr
    (client->activity_log->data, ==, "RECEIVED OID BLOCK test/1:100");
  
  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_value_response ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x12\x51test\x00names\x00\x01\x00\x04""foo\x00", 21);

  g_assert_cmpint
    (gzochid_dataclient_client_protocol.dispatch (bytes, client), ==, 21);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr
    (client->activity_log->data, ==, "RECEIVED VALUE test/names:foo");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_next_key_response ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x12\x52test\x00names\x00\x01\x00\x04""foo\x00", 21);

  g_assert_cmpint
    (gzochid_dataclient_client_protocol.dispatch (bytes, client), ==, 21);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 1);
  g_assert_cmpstr
    (client->activity_log->data, ==, "RECEIVED KEY test/names:foo");

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_multiple ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x12\x51test\x00names\x00\x01\x00\x04""foo\x00", 21);
  g_byte_array_append
    (bytes, "\x00\x12\x52test\x00names\x00\x01\x00\x04""bar\x00", 21);

  g_assert_cmpint
    (gzochid_dataclient_client_protocol.dispatch (bytes, client), ==, 42);
  g_assert_cmpint (g_list_length (client->activity_log), ==, 2);
  g_assert_cmpstr
    (client->activity_log->data, ==, "RECEIVED VALUE test/names:foo");
  g_assert_cmpstr
    (client->activity_log->next->data, ==, "RECEIVED KEY test/names:bar");
  
  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_error ()
{
  GzochidDataClient *client = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  
  gzochid_dataclient_client_protocol.error (client);
  g_assert_false (client->connected);
  
  g_object_unref (client);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/client/can_dispatch/true", test_client_can_dispatch_true);
  g_test_add_func
    ("/client/can_dispatch/false", test_client_can_dispatch_false);

  g_test_add_func
    ("/client/dispatch/one/oids-response",
     test_client_dispatch_one_oids_response);
  g_test_add_func
    ("/client/dispatch/one/value-response",
     test_client_dispatch_one_value_response);
  g_test_add_func
    ("/client/dispatch/one/next-key-response",
     test_client_dispatch_one_next_key_response);

  g_test_add_func ("/client/dispatch/multiple", test_client_dispatch_multiple);
  g_test_add_func ("/client/error", test_client_error);
  
  return g_test_run ();
}
