/* test-metaclient-protocol.c: Tests for metaclient-protocol.c in gzochid.
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

#include "event.h"
#include "event-app.h"
#include "metaclient.h"
#include "metaclient-protocol.h"

struct _GzochidMetaClient
{
  GObject parent_instance;

  GList *activity_log;
  gboolean connected;
  gzochid_event_source *event_source;
};

G_DEFINE_TYPE (GzochidMetaClient, gzochid_meta_client, G_TYPE_OBJECT);

enum test_metaclient_properties
  {
    PROP_CONNECTION_DESCRIPTION = 1,
    PROP_EVENT_SOURCE,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_meta_client_get_property (GObject *object, guint property_id,
				  GValue *value, GParamSpec *pspec)
{
  GzochidMetaClient *meta_client = GZOCHID_META_CLIENT (object);

  switch (property_id)
    {
    case PROP_CONNECTION_DESCRIPTION:
      g_value_set_static_string (value, "127.0.0.1");
      break;
      
    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, meta_client->event_source);
      break;
      
    default:
      g_test_fail ();
    }
}

static void
gzochid_meta_client_finalize (GObject *gobject)
{
  GzochidMetaClient *client = GZOCHID_META_CLIENT (gobject);

  g_list_free_full (client->activity_log, g_free);
  g_source_destroy ((GSource *) client->event_source);
  g_source_unref ((GSource *) client->event_source);
}

static void
gzochid_meta_client_class_init (GzochidMetaClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_meta_client_finalize;
  object_class->get_property = gzochid_meta_client_get_property;

  obj_properties[PROP_CONNECTION_DESCRIPTION] = g_param_spec_string
    ("connection-description", "conn-desc", "Test connection description", NULL,
     G_PARAM_READABLE);

  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "Test event source", G_TYPE_SOURCE,
     G_PARAM_READABLE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_meta_client_init (GzochidMetaClient *self)
{
  self->connected = TRUE;
  self->event_source = gzochid_event_source_new ();
}

void
gzochid_metaclient_nullify_connection (GzochidMetaClient *client)
{
  client->connected = FALSE;
}

struct callback_data
{
  GMutex mutex;
  GCond cond;

  gboolean handled;
};

static void
connected_event_handler (GzochidEvent *event, gpointer user_data)
{
  GzochidMetaServerEvent *metaserver_event = GZOCHID_META_SERVER_EVENT (event);
  struct callback_data *callback_data = user_data;
  gzochid_meta_server_event_type type;
  
  g_mutex_lock (&callback_data->mutex);

  g_object_get (metaserver_event, "type", &type, NULL);
  g_assert_cmpint (type, ==, META_SERVER_CONNECTED);  
  callback_data->handled = TRUE;

  g_cond_signal (&callback_data->cond);
  g_mutex_unlock (&callback_data->mutex);
}

static void
pump_login_response (GzochidMetaClient *client, GByteArray *bytes,
		     struct callback_data *callback_data)
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = NULL;

  g_object_get (client, "event-source", &event_source, NULL);
  gzochid_event_attach (event_source, connected_event_handler, callback_data);
  gzochid_event_source_attach (event_loop, event_source);
  g_source_unref ((GSource *) event_source);
  
  gzochid_event_loop_start (event_loop);

  g_mutex_lock (&callback_data->mutex);
  
  g_assert_cmpint
    (gzochid_metaclient_client_protocol.dispatch (bytes, client), ==,
     bytes->len);

  g_cond_wait_until
    (&callback_data->cond, &callback_data->mutex,
     g_get_monotonic_time () + 100000);

  g_mutex_unlock (&callback_data->mutex);

  gzochid_event_loop_stop (event_loop);
  g_object_unref (event_loop);
}

static void
test_client_dispatch_one_login_response ()
{
  GzochidMetaClient *client = g_object_new (GZOCHID_TYPE_META_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x02\x11\x02\x00", 5);
  pump_login_response (client, bytes, &callback_data);
  g_assert (callback_data.handled);

  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_dispatch_one_login_response_base_url ()
{
  GzochidMetaClient *client = g_object_new (GZOCHID_TYPE_META_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x13\x11\x02http://127.0.0.1/", 22);
  pump_login_response (client, bytes, &callback_data);
  g_assert (callback_data.handled);

  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_can_dispatch_true ()
{
  GzochidMetaClient *client = g_object_new (GZOCHID_TYPE_META_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x51test\x00oids\x00\x01\x00\x04""foo\x00", 20);
  
  g_assert (gzochid_metaclient_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
  g_object_unref (client);
}

static void
test_client_can_dispatch_false ()
{
  GzochidMetaClient *client = g_object_new (GZOCHID_TYPE_META_CLIENT, NULL);
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x51test\x00oids\x00\x01\x00\x04", 16);

  g_assert (! gzochid_metaclient_client_protocol.can_dispatch (bytes, client));

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
  
  g_test_add_func
    ("/client/dispatch/one/login-response/simple",
     test_client_dispatch_one_login_response);
  g_test_add_func
    ("/client/dispatch/one/login-response/base-url",
     test_client_dispatch_one_login_response_base_url);
  
  return g_test_run ();
}
