/* test-dataserver-protocol.c: Test routines for test-dataserver-protocol.c.
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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "data-protocol.h"
#include "dataserver.h"
#include "dataserver-protocol.h"
#include "event.h"
#include "event-meta.h"
#include "protocol.h"
#include "resolver.h"
#include "socket.h"

struct _GzochiMetadDataServer
{
  GObject parent_instance;

  gzochid_event_source *event_source;
};

G_DEFINE_TYPE (GzochiMetadDataServer, gzochi_metad_data_server, G_TYPE_OBJECT);

enum test_data_server_properties
  {
    PROP_ADMIN_SERVER_BASE_URL = 1,
    PROP_EVENT_SOURCE,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
get_property (GObject *object, guint property_id, GValue *value,
	      GParamSpec *pspec)
{
  GzochiMetadDataServer *data_server = GZOCHI_METAD_DATA_SERVER (object);

  switch (property_id)
    {
    case PROP_ADMIN_SERVER_BASE_URL:
      g_value_set_static_string (value, "http://localhost:8081/");
      break;

    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, data_server->event_source);
      break;
      
    default:
      g_test_fail ();
    }
}

static void
finalize (GObject *object)
{
  GzochiMetadDataServer *data_server = GZOCHI_METAD_DATA_SERVER (object);

  g_source_destroy ((GSource *) data_server->event_source);
  g_source_unref ((GSource *) data_server->event_source);
}

static void
gzochi_metad_data_server_class_init (GzochiMetadDataServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = get_property;
  object_class->finalize = finalize;
  
  obj_properties[PROP_ADMIN_SERVER_BASE_URL] = g_param_spec_string
    ("admin-server-base-url", "base-url", "Test admin server base URL", NULL,
     G_PARAM_READABLE);

  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "Test event source", G_TYPE_SOURCE,
     G_PARAM_READABLE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_data_server_init (GzochiMetadDataServer *self)
{
  self->event_source = gzochid_event_source_new ();
}

static GList *activity_log = NULL;

static void
clear_activity_log ()
{
  g_list_free_full (activity_log, g_free);
  activity_log = NULL;
}

gzochid_data_reserve_oids_response *
gzochi_metad_dataserver_reserve_oids (GzochiMetadDataServer *dataserver,
				      guint node_id, char *app)
{
  gzochid_data_oids_block block;
  gzochid_data_reserve_oids_response *response = NULL;

  block.block_start = 1;
  block.block_size = 100;

  response = gzochid_data_reserve_oids_response_new (app, &block);

  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_value (GzochiMetadDataServer *dataserver,
				       guint node_id, char *app, char *store,
				       GBytes *key, gboolean for_write,
				       GError **err)
{
  GBytes *data = g_bytes_new_static ("foo", 4);
  gzochid_data_response *response =
    gzochid_data_response_new (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_next_key (GzochiMetadDataServer *dataserver,
					  guint node_id, char *app, char *store,
					  GBytes *key, GError **err)
{
  GBytes *data = g_bytes_new_static ("foo1", 5);
  gzochid_data_response *response = gzochid_data_response_new
    (app, store, TRUE, data);

  g_bytes_unref (data);
  return response;
}

void
gzochi_metad_dataserver_release_key (GzochiMetadDataServer *dataserver,
				     guint node_id, char *app, char *store,
				     GBytes *key)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("RELEASE KEY/%s/%s: %s", app, store,
      (char *) g_bytes_get_data (key, NULL)));
}

void
gzochi_metad_dataserver_release_range (GzochiMetadDataServer *dataserver,
				       guint node_id, char *app, char *store,
				       GBytes *from, GBytes *to)
{
  activity_log = g_list_append
    (activity_log, g_strdup_printf
     ("RELEASE RANGE/%s/%s: %s -> %s", app, store,
      (char *) g_bytes_get_data (from, NULL),
      (char *) g_bytes_get_data (to, NULL)));
}

void
gzochi_metad_dataserver_release_all (GzochiMetadDataServer *dataserver,
				     guint node_id)
{
  activity_log = g_list_append (activity_log, g_strdup ("RELEASE ALL"));
}

void
gzochi_metad_dataserver_process_changeset (GzochiMetadDataServer *dataserver,
					   guint node_id,
					   gzochid_data_changeset *changeset,
					   GError **err)
{
  int i = 0;

  for (; i < changeset->changes->len; i++)
    {
      gzochid_data_change *change = &g_array_index
	(changeset->changes, gzochid_data_change, i);

      if (change->delete)
	activity_log = g_list_append
	  (activity_log, g_strdup_printf
	   ("PROCESS CHANGESET/%s/%s: DELETE KEY %s", changeset->app,
	    change->store, (char *) g_bytes_get_data (change->key, NULL)));
      else activity_log = g_list_append
	     (activity_log, g_strdup_printf
	      ("PROCESS CHANGESET/%s/%s: SET KEY %s -> %s", changeset->app,
	       change->store, (char *) g_bytes_get_data (change->key, NULL),
	       (char *) g_bytes_get_data (change->data, NULL)));
    }
}

struct _dataserver_protocol_fixture
{
  GzochiMetadDataServer *dataserver;
  GzochidSocketServer *socket_server;
  gzochid_client_socket *client_socket;
  gzochid_server_socket *server_socket;

  GIOChannel *socket_channel;
};

typedef struct _dataserver_protocol_fixture dataserver_protocol_fixture;

static gzochid_client_socket *
server_accept_wrapper (GIOChannel *channel, const char *desc, gpointer data)
{
  dataserver_protocol_fixture *fixture = data;

  fixture->dataserver = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_DATA_SERVER, NULL);
  fixture->client_socket = gzochi_metad_dataserver_server_protocol.accept
    (channel, desc, fixture->dataserver);
    
  return fixture->client_socket;
}

static gzochid_server_protocol dataserver_server_wrapper_protocol =
  { server_accept_wrapper };

static void
dataserver_protocol_fixture_set_up (dataserver_protocol_fixture *fixture,
				    gconstpointer user_data)
{
  struct sockaddr addr;
  size_t addrlen = sizeof (struct sockaddr);
  int socket_fd = socket (AF_INET, SOCK_STREAM, 0);

  fixture->socket_server = gzochid_resolver_require
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  
  fixture->server_socket = gzochid_server_socket_new
    ("test", dataserver_server_wrapper_protocol, fixture);
  
  gzochid_server_socket_listen
    (fixture->socket_server, fixture->server_socket, 0);
  _gzochid_server_socket_getsockname (fixture->server_socket, &addr, &addrlen);
  connect (socket_fd, &addr, addrlen);

  g_assert (g_main_context_iteration
	    (fixture->socket_server->main_context, TRUE));

  g_assert (fixture->client_socket != NULL);

  fixture->socket_channel = g_io_channel_unix_new (socket_fd);
  g_io_channel_set_flags (fixture->socket_channel, G_IO_FLAG_NONBLOCK, NULL);
}

static void
dataserver_protocol_fixture_tear_down (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_io_channel_unref (fixture->socket_channel);

  clear_activity_log ();
}

static void
test_server_accept (dataserver_protocol_fixture *fixture,
		    gconstpointer user_data)
{
  gzochid_client_protocol protocol =
    _gzochid_client_socket_get_protocol (fixture->client_socket);

  g_assert (memcmp (&protocol, &gzochi_metad_dataserver_client_protocol,
		    sizeof (gzochid_client_protocol)) == 0);
}

static void
test_client_can_dispatch_true (dataserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http://localhost:8080/", 27);

  g_assert
    (gzochi_metad_dataserver_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (dataserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http", 9);

  g_assert
    (! gzochi_metad_dataserver_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
}

struct callback_data
{
  GMutex mutex;
  GCond cond;

  gboolean handled;  
};

static void
handle_client_connected (GzochidEvent *event, gpointer user_data)
{
  GzochiMetadClientEvent *client_event = GZOCHI_METAD_CLIENT_EVENT (event);
  struct callback_data *callback_data = user_data;
  gzochi_metad_client_event_type type;

  g_mutex_lock (&callback_data->mutex);
  
  g_object_get (client_event, "type", &type, NULL);
  g_assert_cmpint (type, ==, CLIENT_CONNECTED);
  callback_data->handled = TRUE;
  
  g_cond_signal (&callback_data->cond);
  g_mutex_unlock (&callback_data->mutex);
}

static void
pump_login (GzochiMetadDataServer *dataserver,
	    gzochi_metad_dataserver_client *client, GByteArray *bytes,
	    struct callback_data *callback_data)
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = NULL;

  g_object_get (dataserver, "event-source", &event_source, NULL);
  gzochid_event_attach (event_source, handle_client_connected, callback_data);
  gzochid_event_source_attach (event_loop, event_source);
  g_source_unref ((GSource *) event_source);

  gzochid_event_loop_start (event_loop);

  g_mutex_lock (&callback_data->mutex);
  
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);

  g_cond_wait_until
    (&callback_data->cond, &callback_data->mutex,
     g_get_monotonic_time () + 100000);
  g_mutex_unlock (&callback_data->mutex);

  gzochid_event_loop_stop (event_loop);
  g_object_unref (event_loop);
}

static void
test_client_dispatch_one_login (dataserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  char buf[27];
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x02\x10\x02\x00", 5);
  pump_login (fixture->dataserver, client, bytes, &callback_data);
  g_byte_array_unref (bytes);
  
  g_assert (callback_data.handled);
  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 27, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert(memcmp (buf, "\x00\x18\x11\x02http://localhost:8081/", 27) == 0);
}

static void
test_client_dispatch_one_login_base_url (dataserver_protocol_fixture *fixture,
					 gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  char buf[27];
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x18\x10\x02http://localhost:8080/", 27);
  pump_login (fixture->dataserver, client, bytes, &callback_data);
  g_assert (callback_data.handled);
  
  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  g_byte_array_unref (bytes);
  
  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 27, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert(memcmp (buf, "\x00\x18\x11\x02http://localhost:8081/", 27) == 0);
}

static void
test_client_dispatch_one_reserve_oids (dataserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[18];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x05\x20test", 8);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 18, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert(memcmp (buf,
		   "\x00\x0f\x50test\x00"
		   "\x00\x00\x00\x00\x00\x00\x00\x01\x00\x64", 18) == 0);
}

static void
test_client_dispatch_one_request_value (dataserver_protocol_fixture *fixture,
					gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[20];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x0f\x21test\x00oids\x00\x00\x00\x02""1", 18);
  
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 20, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x11\x51test\x00oids\x00\x01\x00\x04""foo\x00", 20) ==
     0);
}

static void
test_client_dispatch_one_request_next_key
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  char buf[22];
  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x11\x22test\x00names\x00\x00\x04""foo", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 22, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);
  
  g_assert
    (memcmp (buf, "\x00\x13\x52test\x00names\x00\x01\x00\x05""foo1", 22) == 0);

}

static void
test_client_dispatch_one_release_key (dataserver_protocol_fixture *fixture,
				      gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x0e\x40test\x00oids\x00\x00\x02""1", 17);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE KEY/test/oids: 1");
}

static void
test_client_dispatch_one_release_range
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x19\x42test\x00names\x00\x00\x05""foo1\x00\x00\x05""foo2\x00",
     28);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr
    ((char *) activity_log->data, ==, "RELEASE RANGE/test/names: foo1 -> foo2");
}

static void
test_client_dispatch_one_process_changeset
(dataserver_protocol_fixture *fixture, gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x3f\x30test", 8);
  g_byte_array_append (bytes, "\x00\x04", 2);
  g_byte_array_append (bytes, "oids\x00\x00\x02""1\x00\x00\x00", 11);
  g_byte_array_append (bytes, "oids\x00\x00\x02""2\x00\x00\x04""foo", 15);
  g_byte_array_append (bytes, "names\x00\x00\x04""foo\x00\x00\x00", 14);
  g_byte_array_append (bytes, "names\x00\x00\x04""bar\x00\x00\x02""3", 16);

  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 4);
  g_assert_cmpstr (g_list_nth_data (activity_log, 0), ==,
		   "PROCESS CHANGESET/test/oids: DELETE KEY 1");
  g_assert_cmpstr (g_list_nth_data (activity_log, 1), ==,
		   "PROCESS CHANGESET/test/oids: SET KEY 2 -> foo");
  g_assert_cmpstr (g_list_nth_data (activity_log, 2), ==,
		   "PROCESS CHANGESET/test/names: DELETE KEY foo");
  g_assert_cmpstr (g_list_nth_data (activity_log, 3), ==,
		   "PROCESS CHANGESET/test/names: SET KEY bar -> 3");
}

static void
test_client_dispatch_multiple (dataserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""foo", 20);
  g_byte_array_append
    (bytes, "\x00\x11\x40test\x00names\x00""\x00\x04""bar", 20);
  gzochi_metad_dataserver_client_protocol.dispatch (bytes, client);
  g_byte_array_unref (bytes);

  g_assert_cmpint (g_list_length (activity_log), ==, 2);
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 0), ==, "RELEASE KEY/test/names: foo");
  g_assert_cmpstr
    (g_list_nth_data (activity_log, 1), ==, "RELEASE KEY/test/names: bar");
}

static void
handle_client_disconnected (GzochidEvent *event, gpointer user_data)
{
  GzochiMetadClientEvent *client_event = GZOCHI_METAD_CLIENT_EVENT (event);
  struct callback_data *callback_data = user_data;
  gzochi_metad_client_event_type type;

  g_mutex_lock (&callback_data->mutex);
  
  g_object_get (client_event, "type", &type, NULL);
  g_assert_cmpint (type, ==, CLIENT_DISCONNECTED);
  callback_data->handled = TRUE;
  
  g_cond_signal (&callback_data->cond);
  g_mutex_unlock (&callback_data->mutex);
}

static void
test_client_error (dataserver_protocol_fixture *fixture,
		   gconstpointer user_data)
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = NULL;
  GSource *source = g_main_context_find_source_by_user_data
    (fixture->socket_server->main_context, fixture->client_socket);
  gzochi_metad_dataserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  struct callback_data callback_data;
  
  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_object_get (fixture->dataserver, "event-source", &event_source, NULL);
  gzochid_event_attach
    (event_source, handle_client_disconnected, &callback_data);
  gzochid_event_source_attach (event_loop, event_source);
  g_source_unref ((GSource *) event_source);

  gzochid_event_loop_start (event_loop);

  g_mutex_lock (&callback_data.mutex);
  
  gzochi_metad_dataserver_client_protocol.error (client);

  g_cond_wait_until
    (&callback_data.cond, &callback_data.mutex,
     g_get_monotonic_time () + 100000);
  g_mutex_unlock (&callback_data.mutex);

  gzochid_event_loop_stop (event_loop);
  g_object_unref (event_loop);

  g_assert (callback_data.handled);
  g_assert_cmpint (g_list_length (activity_log), ==, 1);
  g_assert_cmpstr ((char *) activity_log->data, ==, "RELEASE ALL");

  g_assert (callback_data.handled);
  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_source_unref (source);
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

  g_test_add ("/server/accept", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_server_accept,
	      dataserver_protocol_fixture_tear_down);
  g_test_add ("/client/can_dispatch/true", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_can_dispatch_true,
	      dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/can_dispatch/false", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/login/simple", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_login,
     dataserver_protocol_fixture_tear_down);
  g_test_add
    ("/client/dispatch/one/login/base-url", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_login_base_url,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/reserve-oids", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_reserve_oids,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/request-value", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_request_value,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/request-next-key", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_request_next_key,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/release-key", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_release_key,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/release-range", dataserver_protocol_fixture, NULL,
     dataserver_protocol_fixture_set_up, test_client_dispatch_one_release_range,
     dataserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/process-changeset", dataserver_protocol_fixture,
     NULL, dataserver_protocol_fixture_set_up,
     test_client_dispatch_one_process_changeset,
     dataserver_protocol_fixture_tear_down);

  g_test_add ("/client/dispatch/multiple", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_dispatch_multiple,
	      dataserver_protocol_fixture_tear_down);

  g_test_add ("/client/error", dataserver_protocol_fixture, NULL,
	      dataserver_protocol_fixture_set_up, test_client_error,
	      dataserver_protocol_fixture_tear_down);
  
  return g_test_run ();
}
