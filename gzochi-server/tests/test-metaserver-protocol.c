/* test-metaserver-protocol.c: Test routines for test-metaserver-protocol.c.
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

#include <assert.h>
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
#include "meta-protocol.h"
#include "metaserver-protocol.h"
#include "protocol.h"
#include "resolver.h"
#include "socket.h"

#define GZOCHI_METAD_TYPE_ROOT_CONTEXT gzochi_metad_root_context_get_type ()

GType gzochi_metad_root_context_get_type (void);

struct _GzochiMetadRootContextClass
{
  GObjectClass parent_class;
};

typedef struct _GzochiMetadRootContextClass GzochiMetadRootContextClass;

struct _GzochiMetadRootContext
{
  GObject parent_instance;

  GObject *dataserver;

  gzochid_event_source *event_source;
  char *admin_server_base_url;
};

typedef struct _GzochiMetadRootContext GzochiMetadRootContext;

static inline GzochiMetadRootContext *
GZOCHI_METAD_ROOT_CONTEXT (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_root_context_get_type (), GzochiMetadRootContext);
}

G_DEFINE_TYPE (GzochiMetadRootContext, gzochi_metad_root_context,
	       G_TYPE_OBJECT);

enum gzochi_metad_data_server_properties
  {
    PROP_DATA_SERVER = 1,
    PROP_EVENT_SOURCE,
    PROP_ADMIN_SERVER_BASE_URL,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
root_context_get_property (GObject *object, guint property_id, GValue *value,
			   GParamSpec *pspec)
{
  GzochiMetadRootContext *root_context = GZOCHI_METAD_ROOT_CONTEXT (object);
  
  switch (property_id)
    {
    case PROP_DATA_SERVER:
      g_value_set_object (value, root_context->dataserver);
      break;
      
    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, root_context->event_source);
      break;

    case PROP_ADMIN_SERVER_BASE_URL:
      g_value_set_string (value, root_context->admin_server_base_url);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
root_context_set_property (GObject *object, guint property_id,
			   const GValue *value, GParamSpec *pspec)
{
  GzochiMetadRootContext *root_context = GZOCHI_METAD_ROOT_CONTEXT (object);
  
  switch (property_id)
    {
    case PROP_ADMIN_SERVER_BASE_URL:
      root_context->admin_server_base_url = strdup (g_value_get_string (value));
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochi_metad_root_context_class_init (GzochiMetadRootContextClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = root_context_get_property;
  object_class->set_property = root_context_set_property;

  obj_properties[PROP_DATA_SERVER] = g_param_spec_object
    ("data-server", "data-server", "Test dataserver object", G_TYPE_OBJECT,
     G_PARAM_READABLE);

  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "Test event source", G_TYPE_SOURCE,
     G_PARAM_READABLE);

  obj_properties[PROP_ADMIN_SERVER_BASE_URL] = g_param_spec_string
    ("admin-server-base-url", "admin-server-base-url", "Test admin server URL",
     "http://localhost:8081/", G_PARAM_READWRITE | G_PARAM_CONSTRUCT);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_root_context_init (GzochiMetadRootContext *self)
{
  self->dataserver = g_object_new (G_TYPE_OBJECT, NULL);
  self->event_source = gzochid_event_source_new ();
}

static gboolean
dataserver_can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  assert (1 == 0);
}

static GList *dataserver_messages = NULL;
static gboolean dataserver_error_flag = FALSE;

static unsigned int
dataserver_dispatch (const GByteArray *buffer, gpointer user_data)
{
  dataserver_messages = g_list_append
    (dataserver_messages, g_bytes_new (buffer->data, buffer->len));
  
  return 1;
}

static void
dataserver_error (gpointer user_data)
{
  dataserver_error_flag = TRUE;
}

static void
dataserver_free (gpointer user_data)
{
}

gzochid_client_protocol gzochi_metad_dataserver_client_protocol =
  {
    dataserver_can_dispatch,
    dataserver_dispatch,
    dataserver_error,
    dataserver_free
  };

struct _metaserver_protocol_fixture
{
  GzochiMetadRootContext *root_context;
  GzochidSocketServer *socket_server;
  gzochid_client_socket *client_socket;
  gzochid_server_socket *server_socket;

  GIOChannel *socket_channel;
};

typedef struct _metaserver_protocol_fixture metaserver_protocol_fixture;

gzochi_metad_dataserver_client *
gzochi_metad_dataserver_client_new (GzochiMetadDataServer *dataserver,
				    gzochid_client_socket *socket,
				    unsigned int node_id)
{
  return NULL;
}

static gzochid_client_socket *
server_accept_wrapper (GIOChannel *channel, const char *desc, gpointer data)
{
  metaserver_protocol_fixture *fixture = data;

  fixture->client_socket = gzochi_metad_metaserver_server_protocol.accept
    (channel, desc, fixture->root_context);
    
  return fixture->client_socket;
}

static gzochid_server_protocol metaserver_server_wrapper_protocol =
  { server_accept_wrapper };

static void
metaserver_protocol_fixture_set_up (metaserver_protocol_fixture *fixture,
				    gconstpointer user_data)
{
  struct sockaddr addr;
  size_t addrlen = sizeof (struct sockaddr);
  int socket_fd = socket (AF_INET, SOCK_STREAM, 0);

  fixture->root_context = gzochid_resolver_require
    (GZOCHI_METAD_TYPE_ROOT_CONTEXT, NULL);
  
  fixture->socket_server = gzochid_resolver_require
    (GZOCHID_TYPE_SOCKET_SERVER, NULL);
  
  fixture->server_socket = gzochid_server_socket_new
    ("test", metaserver_server_wrapper_protocol, fixture);
  
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
metaserver_protocol_fixture_tear_down (metaserver_protocol_fixture *fixture,
				       gconstpointer user_data)
{
  g_object_unref (fixture->socket_server);
  g_io_channel_unref (fixture->socket_channel);

  g_list_free_full (dataserver_messages, (GDestroyNotify) g_bytes_unref);
  dataserver_messages = NULL;
  
  dataserver_error_flag = FALSE;
}

static void
test_server_accept (metaserver_protocol_fixture *fixture,
		    gconstpointer user_data)
{
  gzochid_client_protocol protocol =
    _gzochid_client_socket_get_protocol (fixture->client_socket);

  g_assert (memcmp (&protocol, &gzochi_metad_metaserver_client_protocol,
		    sizeof (gzochid_client_protocol)) == 0);
}

static void
test_client_can_dispatch_true (metaserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http://localhost:8080/", 27);

  g_assert
    (gzochi_metad_metaserver_client_protocol.can_dispatch (bytes, client));

  g_byte_array_unref (bytes);
}

static void
test_client_can_dispatch_false (metaserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GByteArray *bytes = g_byte_array_new ();

  g_byte_array_append (bytes, "\x00\x18\x10\x02http", 9);

  g_assert
    (! gzochi_metad_metaserver_client_protocol.can_dispatch (bytes, client));

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
pump_login (GzochiMetadRootContext *root_context,
	    gzochi_metad_metaserver_client *client, GByteArray *bytes,
	    struct callback_data *callback_data)
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = NULL;

  g_object_get (root_context, "event-source", &event_source, NULL);
  gzochid_event_attach (event_source, handle_client_connected, callback_data);
  gzochid_event_source_attach (event_loop, event_source);
  g_source_unref ((GSource *) event_source);

  gzochid_event_loop_start (event_loop);

  g_mutex_lock (&callback_data->mutex);
  
  gzochi_metad_metaserver_client_protocol.dispatch (bytes, client);

  g_cond_wait_until
    (&callback_data->cond, &callback_data->mutex,
     g_get_monotonic_time () + 100000);
  g_mutex_unlock (&callback_data->mutex);

  gzochid_event_loop_stop (event_loop);
  g_object_unref (event_loop);
}

static void
test_client_dispatch_one_login (metaserver_protocol_fixture *fixture,
				gconstpointer user_data)
{
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  char buf[27];
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x02\x10\x02\x00", 5);
  pump_login (fixture->root_context, client, bytes, &callback_data);
  g_byte_array_unref (bytes);
  
  g_assert (callback_data.handled);
  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_main_context_iteration (fixture->socket_server->main_context, FALSE);
  
  g_assert_cmpint
    (g_io_channel_read_chars (fixture->socket_channel, buf, 27, NULL, NULL), ==,
     G_IO_STATUS_NORMAL);

  g_assert (memcmp (buf, "\x00\x18\x11\x02http://localhost:8081/", 27) == 0);
}

static void
test_client_dispatch_one_login_base_url (metaserver_protocol_fixture *fixture,
					 gconstpointer user_data)
{
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  
  char buf[27];
  GByteArray *bytes = g_byte_array_new ();
  struct callback_data callback_data;

  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_byte_array_append (bytes, "\x00\x18\x10\x02http://localhost:8080/", 27);
  pump_login (fixture->root_context, client, bytes, &callback_data);
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
test_client_dispatch_one_data (metaserver_protocol_fixture *fixture,
			       gconstpointer user_data)
{
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);

  GBytes *expected_bytes = g_bytes_new_static ("\x00\x04\x20""foo", 7);
  GByteArray *input_bytes = g_byte_array_new ();

  g_byte_array_append (input_bytes, "\x00\x04\x20""foo", 7);
  gzochi_metad_metaserver_client_protocol.dispatch (input_bytes, client);  
  g_byte_array_unref (input_bytes);
  
  g_assert (g_list_find_custom
	    (dataserver_messages, expected_bytes, g_bytes_compare) != NULL);

  g_bytes_unref (expected_bytes);
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
test_client_error (metaserver_protocol_fixture *fixture,
		   gconstpointer user_data)
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = NULL;
  GSource *source = g_main_context_find_source_by_user_data
    (fixture->socket_server->main_context, fixture->client_socket);
  gzochi_metad_metaserver_client *client =
    _gzochid_client_socket_get_protocol_data (fixture->client_socket);
  struct callback_data callback_data;
  
  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  g_object_get (fixture->root_context, "event-source", &event_source, NULL);
  gzochid_event_attach
    (event_source, handle_client_disconnected, &callback_data);
  gzochid_event_source_attach (event_loop, event_source);
  g_source_unref ((GSource *) event_source);

  gzochid_event_loop_start (event_loop);

  g_mutex_lock (&callback_data.mutex);
  
  gzochi_metad_metaserver_client_protocol.error (client);

  g_cond_wait_until
    (&callback_data.cond, &callback_data.mutex,
     g_get_monotonic_time () + 100000);
  g_mutex_unlock (&callback_data.mutex);

  gzochid_event_loop_stop (event_loop);
  g_object_unref (event_loop);

  g_assert (callback_data.handled);
  g_assert (dataserver_error_flag);
  
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

  g_test_add ("/server/accept", metaserver_protocol_fixture, NULL,
	      metaserver_protocol_fixture_set_up, test_server_accept,
	      metaserver_protocol_fixture_tear_down);
  g_test_add ("/client/can_dispatch/true", metaserver_protocol_fixture, NULL,
	      metaserver_protocol_fixture_set_up, test_client_can_dispatch_true,
	      metaserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/can_dispatch/false", metaserver_protocol_fixture, NULL,
     metaserver_protocol_fixture_set_up, test_client_can_dispatch_false,
     metaserver_protocol_fixture_tear_down);

  g_test_add
    ("/client/dispatch/one/login/simple", metaserver_protocol_fixture, NULL,
     metaserver_protocol_fixture_set_up, test_client_dispatch_one_login,
     metaserver_protocol_fixture_tear_down);
  g_test_add
    ("/client/dispatch/one/login/base-url", metaserver_protocol_fixture, NULL,
     metaserver_protocol_fixture_set_up,
     test_client_dispatch_one_login_base_url,
     metaserver_protocol_fixture_tear_down);

  g_test_add ("/client/dispatch/one/data", metaserver_protocol_fixture, NULL,
	      metaserver_protocol_fixture_set_up, test_client_dispatch_one_data,
	      metaserver_protocol_fixture_tear_down);
  
  g_test_add ("/client/error", metaserver_protocol_fixture, NULL,
	      metaserver_protocol_fixture_set_up, test_client_error,
	      metaserver_protocol_fixture_tear_down);
  
  return g_test_run ();
}
