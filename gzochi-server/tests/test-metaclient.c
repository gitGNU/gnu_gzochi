/* test-metaclient.c: Tests for metaclient.c in gzochid.
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
#include <netinet/in.h>
#include <stddef.h>
#include <string.h>

#include "config.h"
#include "event.h"
#include "event-app.h"
#include "dataclient.h"
#include "httpd.h"
#include "metaclient.h"
#include "resolver.h"
#include "socket.h"

struct _GzochidHttpServer
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochidHttpServer, gzochid_http_server, G_TYPE_OBJECT);

static void gzochid_http_server_class_init (GzochidHttpServerClass *klass)
{
}

static void gzochid_http_server_init (GzochidHttpServer *self)
{
}

const char *
gzochid_http_server_get_base_url (GzochidHttpServer *server)
{
  return "http://127.0.0.1/";
}

struct _GzochidDataClient
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

enum gzochid_data_client_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_MAIN_CONTEXT,
    PROP_SOCKET,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_data_client_dispose (GObject *gobject)
{
}

static void
gzochid_data_client_set_property (GObject *object, guint property_id,
				  const GValue *value, GParamSpec *pspec)
{
}

static void gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_data_client_dispose;
  object_class->set_property = gzochid_data_client_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_MAIN_CONTEXT] = g_param_spec_boxed
    ("main-context", "main-context", "The meta client's main context",
     G_TYPE_MAIN_CONTEXT, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_SOCKET] = g_param_spec_pointer
    ("reconnectable-socket", "socket",
     "The meta client's reconnectable socket",
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void gzochid_data_client_init (GzochidDataClient *self)
{
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
metaclient_fixture_setup_inner (metaclient_fixture *fixture, GKeyFile *key_file)
{
  struct sockaddr_in addr;
  size_t addrlen = sizeof (struct sockaddr_in);
  char *server_address = NULL;
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  GzochidResolutionContext *resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);

  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  
  fixture->bytes_received = g_byte_array_new ();

  fixture->resolution_context = resolution_context;
  fixture->socket_server = gzochid_resolver_require_full
    (resolution_context, GZOCHID_TYPE_SOCKET_SERVER, NULL);
  fixture->server_socket = gzochid_server_socket_new
    ("test", test_server_protocol, fixture);

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

  g_key_file_set_value
    (key_file, "metaserver", "server.address", server_address);

  g_free (server_address);

  fixture->metaclient = gzochid_resolver_require_full
    (resolution_context, GZOCHID_TYPE_META_CLIENT, NULL);
  
  gzochid_metaclient_start (fixture->metaclient, NULL);

  g_object_unref (configuration);
}

static void
metaclient_fixture_setup (metaclient_fixture *fixture,
				    gconstpointer user_data)
{
  GKeyFile *key_file = g_key_file_new ();

  g_key_file_set_value (key_file, "metaserver", "lock.release.msec", "10");
  g_key_file_set_value (key_file, "metaserver", "rangelock.release.msec", "10");
  
  metaclient_fixture_setup_inner (fixture, key_file);
  g_key_file_unref (key_file);  
}

static void
metaclient_fixture_with_httpd_setup (metaclient_fixture *fixture,
					       gconstpointer user_data)
{
  GKeyFile *key_file = g_key_file_new ();

  g_key_file_set_value (key_file, "admin", "module.httpd.enabled", "true");

  metaclient_fixture_setup_inner (fixture, key_file);
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
test_login_simple (metaclient_fixture *fixture, gconstpointer user_data)
{
  set_timeout (fixture, 1000);
  g_main_loop_run (fixture->socket_server->main_loop);

  g_assert_cmpint (fixture->bytes_received->len, ==, 5);  
  g_assert
    (memcmp (fixture->bytes_received->data, "\x00\x02\x10\x02\x00", 5) == 0);
}

static void
test_login_http_server_enabled (metaclient_fixture *fixture,
				gconstpointer user_data)
{
  set_timeout (fixture, 1000);
  g_main_loop_run (fixture->socket_server->main_loop);

  g_assert_cmpint (fixture->bytes_received->len, ==, 22);  
  g_assert (memcmp (fixture->bytes_received->data,
		    "\x00\x13\x10\x02http://127.0.0.1/\x00", 22) == 0);
}

struct callback_data
{
  GMutex mutex;
  GCond cond;

  gboolean handled;
};

static void
disconnected_event_handler (GzochidEvent *event, gpointer user_data)
{
  GzochidMetaServerEvent *metaserver_event = GZOCHID_META_SERVER_EVENT (event);
  struct callback_data *callback_data = user_data;
  gzochid_meta_server_event_type type;

  g_mutex_lock (&callback_data->mutex);
  
  g_object_get (metaserver_event, "type", &type, NULL);
  g_assert_cmpint (type, ==, META_SERVER_DISCONNECTED);
  callback_data->handled = TRUE;

  g_cond_signal (&callback_data->cond);
  g_mutex_unlock (&callback_data->mutex);
}

static void
test_nullify_connection (metaclient_fixture *fixture, gconstpointer user_data)
{
  gzochid_event_source *event_source = NULL;
  GzochidEventLoop *event_loop = gzochid_resolver_require_full
    (fixture->resolution_context, GZOCHID_TYPE_EVENT_LOOP, NULL);
  struct callback_data callback_data;

  g_object_get (fixture->metaclient, "event-source", &event_source, NULL);
  gzochid_event_attach
    (event_source, disconnected_event_handler, &callback_data);
  g_source_unref ((GSource *) event_source);
  
  g_mutex_init (&callback_data.mutex);
  g_cond_init (&callback_data.cond);
  callback_data.handled = FALSE;
  
  gzochid_event_loop_start (event_loop);

  g_main_loop_run (fixture->socket_server->main_loop);

  g_mutex_lock (&callback_data.mutex);  
  
  gzochid_metaclient_nullify_connection (fixture->metaclient);

  g_cond_wait_until
    (&callback_data.cond, &callback_data.mutex,
     g_get_monotonic_time () + 100000);

  g_mutex_unlock (&callback_data.mutex);
  
  gzochid_event_loop_stop (event_loop);

  g_assert (callback_data.handled);

  g_mutex_clear (&callback_data.mutex);
  g_cond_clear (&callback_data.cond);
  
  g_object_unref (event_loop);
}

static void
test_metaclient_container_present ()
{
  GError *err = NULL;
  GKeyFile *key_file = g_key_file_new ();

  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  GzochidResolutionContext *resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
  GzochidMetaClientContainer *container = NULL;
  GzochidMetaClient *metaclient = NULL;

  g_key_file_set_boolean (key_file, "metaserver", "client.enabled", TRUE);
  
  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  container = gzochid_resolver_require_full
    (resolution_context, GZOCHID_TYPE_META_CLIENT_CONTAINER, &err);
  
  g_assert_no_error (err);

  g_object_get (container, "metaclient", &metaclient, NULL);

  g_assert (metaclient != NULL);

  g_object_unref (metaclient);
  g_object_unref (container);
  g_object_unref (resolution_context);
  g_object_unref (configuration);
  
  g_key_file_unref (key_file);
}

static void
test_metaclient_container_absent ()
{
  GError *err = NULL;
  GKeyFile *key_file = g_key_file_new ();

  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  GzochidResolutionContext *resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
  GzochidMetaClientContainer *container = NULL;
  GzochidMetaClient *metaclient = NULL;

  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  container = gzochid_resolver_require_full
    (resolution_context, GZOCHID_TYPE_META_CLIENT_CONTAINER, &err);
  
  g_assert_no_error (err);

  g_object_get (container, "metaclient", &metaclient, NULL);

  g_assert (metaclient == NULL);

  g_object_unref (container);
  g_object_unref (resolution_context);
  g_object_unref (configuration);
  
  g_key_file_unref (key_file);
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
    ("/metaclient/login/simple", metaclient_fixture, NULL,
     metaclient_fixture_setup, test_login_simple, metaclient_fixture_teardown);
  g_test_add
    ("/metaclient/login/http-server-enabled", metaclient_fixture, NULL,
     metaclient_fixture_with_httpd_setup,
     test_login_http_server_enabled, metaclient_fixture_teardown);
  
  g_test_add
    ("/metaclient/nullify-connection/simple", metaclient_fixture, NULL,
     metaclient_fixture_setup, test_nullify_connection,
     metaclient_fixture_teardown);

  g_test_add_func ("/metaclient/container/present",
		   test_metaclient_container_present);
  g_test_add_func ("/metaclient/container/absent",
		   test_metaclient_container_absent);

  return g_test_run ();
}
