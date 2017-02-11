/* metaclient.c: Metaserver client for gzochid
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

#include <errno.h>
#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "channelclient.h"
#include "config.h"
#include "dataclient.h"
#include "event.h"
#include "event-app.h"
#include "httpd.h"
#include "meta-protocol.h"
#include "metaclient.h"
#include "metaclient-protocol.h"
#include "resolver.h"
#include "sessionclient.h"
#include "socket.h"

struct _GzochidMetaClient
{
  GObject parent_instance;

  GzochidConfiguration *configuration; /* The global configuration object. */

  /* The meta client configuration table; extracted from the global 
     configuration object and cached for convenience. */
  
  GHashTable *metaclient_configuration;

  /* The resolution context to which the metaclient belongs. */

  GzochidResolutionContext *resolution_context; 

  /*
    The channelclient instance to which the metaclient delegates handling of
    `GZOCHID_CHANNEL_PROTOCOL_*' opcodes.

    Note that because the metaclient and the channelclient have to share some
    non-injectable properties, this object is not constructed / managed by 
    gzochid's resolution context. The metaclient constructs it (see below) and
    "owns" it.
  */

  GzochidChannelClient *channelclient;
  
  /*
    The dataclient instance to which the metaclient delegates handling of 
    `GZOCHID_DATA_PROTOCOL_*' opcodes. 

    The sessionclient has a lifecycle similar to that of the channelclient.
  */
  
  GzochidDataClient *dataclient;

  /*
    The sessionclient instance to which the metaclient delegates handling of 
    `GZOCHID_SESSION_PROTOCOL_*' opcodes. 

    The sessionclient has a lifecycle similar to that of the channelclient.
  */

  GzochidSessionClient *sessionclient;
  
  /* The socket server for the meta client. */

  GzochidSocketServer *socket_server;

  /* An event source for posting meta server connection / disconnection 
     events. */
  
  gzochid_event_source *event_source;

  /* When the data client has been started, holds the base URL of the gzochid 
     admin HTTP console, if available; otherwise, a heap-allocated empty 
     string. */
  
  char *admin_server_base_url;
  
  /* The client's connection to the data server. */

  gzochid_reconnectable_socket *socket;

  char *hostname; /* Meta server hostname. */
  unsigned int port; /* Meta server port. */

  /* Interval between meta server connection attempts while in a disconnected 
     state. */

  guint connect_attempt_interval_seconds;
  
  /* The connection maintenance / metaclient task processing thread. */

  GThread *thread; 

  GMainLoop *main_loop; /* Main loop for metaclient task processing. */
  GMainContext *main_context; /* Main context for metaclient task processing. */
  
  /* Whether the connection maintenance thread should stay alive. */

  gboolean running; 
};

/* Boilerplate setup for the data client object. */

G_DEFINE_TYPE (GzochidMetaClient, gzochid_meta_client, G_TYPE_OBJECT);

enum gzochid_meta_client_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_RESOLUTION_CONTEXT,
    PROP_SOCKET_SERVER,
    PROP_EVENT_LOOP,
    PROP_EVENT_SOURCE,
    PROP_CHANNEL_CLIENT,
    PROP_DATA_CLIENT,
    PROP_SESSION_CLIENT,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_meta_client_get_property (GObject *object, guint property_id,
				  GValue *value, GParamSpec *pspec)
{
  GzochidMetaClient *client = GZOCHID_META_CLIENT (object);

  switch (property_id)
    {
    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, client->event_source);
      break;

    case PROP_CHANNEL_CLIENT:
      g_value_set_object (value, client->channelclient);
      break;

    case PROP_DATA_CLIENT:
      g_value_set_object (value, client->dataclient);
      break;

    case PROP_SESSION_CLIENT:
      g_value_set_object (value, client->sessionclient);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_meta_client_set_property (GObject *object, guint property_id,
				  const GValue *value, GParamSpec *pspec)
{
  GzochidMetaClient *self = GZOCHID_META_CLIENT (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;

    case PROP_RESOLUTION_CONTEXT:
      self->resolution_context = g_object_ref_sink (g_value_get_object (value));
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_EVENT_LOOP:

      /* Don't need to store a reference to the event loop, just attach the
	 event source to it. */
      
      gzochid_event_source_attach
	(g_value_get_object (value), self->event_source);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_meta_client_dispose (GObject *object)
{
  GzochidMetaClient *client = GZOCHID_META_CLIENT (object);

  g_object_unref (client->configuration);
  g_object_unref (client->channelclient);
  g_object_unref (client->dataclient);
  g_object_unref (client->resolution_context);
  g_object_unref (client->socket_server);

  g_main_context_unref (client->main_context);
  g_main_loop_unref (client->main_loop);

  G_OBJECT_CLASS (gzochid_meta_client_parent_class)->dispose (object);
}

static void
gzochid_meta_client_finalize (GObject *object)
{
  GzochidMetaClient *client = GZOCHID_META_CLIENT (object);

  if (client->admin_server_base_url != NULL)
    free (client->admin_server_base_url);

  g_hash_table_destroy (client->metaclient_configuration);

  /* Although the reconnectable socket may be handed out to other components
     (e.g., the dataclient) it is owned by the metaclient, so it should be
     destroyed by the metaclinet. */
  
  gzochid_reconnectable_socket_free (client->socket);
  
  g_source_destroy ((GSource *) client->event_source);
  g_source_unref ((GSource *) client->event_source);

  G_OBJECT_CLASS (gzochid_meta_client_parent_class)->finalize (object);
}

static void
gzochid_meta_client_constructed (GObject *gobject)
{
  GzochidMetaClient *client = GZOCHID_META_CLIENT (gobject);  
  GzochidGameServer *game_server = gzochid_resolver_require_full
    (client->resolution_context, GZOCHID_TYPE_GAME_SERVER, NULL);

  /* Extract and save the "metaserver" configuration group to use to look up
     connection details. */
  
  client->metaclient_configuration = gzochid_configuration_extract_group
    (client->configuration, "metaserver");

  /* Explicit construction of the channelclient with game server and 
     reconnectable socket pointer. */

  client->channelclient = g_object_new
    (GZOCHID_TYPE_CHANNEL_CLIENT,
     "game-server", game_server,
     "reconnectable-socket", client->socket,
     NULL);
  
  /* Explicit construction of the dataclient with main context and 
     reconnectable socket pointer. */
  
  client->dataclient = g_object_new
    (GZOCHID_TYPE_DATA_CLIENT,
     "configuration", client->configuration,
     "main-context", client->main_context,
     "reconnectable-socket", client->socket,
     NULL);

  /* Explicit construction of the sessionclient with game server and 
     reconnectable socket pointer. */

  client->sessionclient = g_object_new
    (GZOCHID_TYPE_SESSION_CLIENT,
     "game-server", game_server,
     "reconnectable-socket", client->socket,
     NULL);

  g_object_unref (game_server);
}

static void
gzochid_meta_client_class_init (GzochidMetaClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_meta_client_dispose;
  object_class->finalize = gzochid_meta_client_finalize;
  object_class->constructed = gzochid_meta_client_constructed;
  object_class->get_property = gzochid_meta_client_get_property;
  object_class->set_property = gzochid_meta_client_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_RESOLUTION_CONTEXT] = g_param_spec_object
    ("resolution-context", "resolution-context",
     "The global resolution context", GZOCHID_TYPE_RESOLUTION_CONTEXT,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "socket-server", "The global socket server",
     GZOCHID_TYPE_SOCKET_SERVER, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_EVENT_LOOP] = g_param_spec_object
    ("event-loop", "event-loop", "The global event loop",
     GZOCHID_TYPE_EVENT_LOOP, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "The meta client event source",
     G_TYPE_SOURCE, G_PARAM_READABLE);

  obj_properties[PROP_CHANNEL_CLIENT] = g_param_spec_object
    ("channel-client", "channelclient", "The channel client",
     GZOCHID_TYPE_CHANNEL_CLIENT, G_PARAM_READABLE);

  obj_properties[PROP_DATA_CLIENT] = g_param_spec_object
    ("data-client", "dataclient", "The data client", GZOCHID_TYPE_DATA_CLIENT,
     G_PARAM_READABLE);

  obj_properties[PROP_SESSION_CLIENT] = g_param_spec_object
    ("session-client", "sessionclient", "The session client",
     GZOCHID_TYPE_SESSION_CLIENT, G_PARAM_READABLE);
  
  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

/*
  A convenience function for prefixing a message payload with its size and
  opcode. Every message needs these things and the memory management is a
  bit tedious. 

  Returns a pointer to a newly-allocated buffer containing the complete message,
  which should be freed when no longer needed. The `formatted_len' argument, if
  provided, will be set to the length of this buffer. 
*/

static unsigned char *
format_message (guchar opcode, GBytes *payload, size_t *formatted_len)
{
  size_t len = 0;
  const unsigned char *data = g_bytes_get_data (payload, &len);
  unsigned char *buf = malloc (sizeof (unsigned char) * (len + 3));

  /* Prefix the message with its length. */
  
  gzochi_common_io_write_short (len, buf, 0);
  buf[2] = opcode;
  memcpy (buf + 3, data, len);

  if (formatted_len != NULL)
    *formatted_len = len + 3;
  
  return buf;
}

/* A `gzochid_reconnectable_socket_connected' callback implementation that sends
   a login message to the meta server on connection - i.e., before any outbound 
   messages are flushed. */

static void
on_connect (gpointer user_data)
{
  GzochidMetaClient *client = user_data;

  GBytes *login_message_payload = NULL;
  GByteArray *login_message_payload_arr = g_byte_array_new ();
  unsigned char *login_message_bytes = NULL;
  size_t login_message_len = 0;

  g_message
    ("Connected to meta server at %s:%d.", client->hostname, client->port);
  
  /* Create the message payload by concatenating the protocol version byte with
     the admin server base URL; the base URL will never be `NULL', though it 
     may be an empty string. */
	      
  g_byte_array_append
    (login_message_payload_arr,
     (unsigned char[]) { GZOCHID_METACLIENT_PROTOCOL_VERSION }, 1);
	      
  g_byte_array_append
    (login_message_payload_arr, (unsigned char *) client->admin_server_base_url,
     strlen (client->admin_server_base_url) + 1);

  login_message_payload = g_byte_array_free_to_bytes
    (login_message_payload_arr);
  
  /* Prefix the message with its length and opcode. */
  
  login_message_bytes = format_message
    (GZOCHID_META_PROTOCOL_LOGIN, login_message_payload, &login_message_len);

  gzochid_reconnectable_socket_write
    (client->socket, login_message_bytes, login_message_len);
	      
  g_bytes_unref (login_message_payload);
  free (login_message_bytes);
}

/* A no-op `gzochid_reconnectable_socket_disconnected' implementation. */

static void
on_disconnect (gpointer data)
{
}

static void
gzochid_meta_client_init (GzochidMetaClient *self)
{
  self->main_context = g_main_context_new ();
  self->main_loop = g_main_loop_new (self->main_context, FALSE);

  self->socket = gzochid_reconnectable_socket_new ();
  
  gzochid_reconnectable_socket_listen
    (self->socket, on_connect, self, on_disconnect, self);

  self->running = FALSE;
  self->event_source = gzochid_event_source_new ();
}

struct _GzochidMetaClientContainer
{
  GObject parent_object;
  
  /* The global configuration, for detecting the enablement of the meta 
     client. */

  GzochidConfiguration *configuration;

  /* The resolution context, for resolving the meta client. */
  
  GzochidResolutionContext *resolution_context; 

  /* Whether an attempt has been made to fetch the meta client. */

  gboolean metaclient_initialized;
  
  GzochidMetaClient *metaclient; /* The meta client instance, or `NULL'. */
};

G_DEFINE_TYPE (GzochidMetaClientContainer, gzochid_meta_client_container,
	       G_TYPE_OBJECT);

enum gzochid_meta_client_container_properties
  {
    CONTAINER_PROP_CONFIGURATION = 1,
    CONTAINER_PROP_RESOLUTION_CONTEXT,
    CONTAINER_PROP_META_CLIENT,
    CONTAINER_N_PROPERTIES
  };

static GParamSpec *container_obj_properties[CONTAINER_N_PROPERTIES] = { NULL };

static void
gzochid_meta_client_container_dispose (GObject *object)
{
  GzochidMetaClientContainer *self = GZOCHID_META_CLIENT_CONTAINER (object);

  g_object_unref (self->configuration);
  g_object_unref (self->resolution_context);

  if (self->metaclient_initialized && self->metaclient != NULL)
    g_object_unref (self->metaclient);
}

static void
gzochid_meta_client_container_set_property (GObject *object, guint property_id,
					    const GValue *value,
					    GParamSpec *pspec)
{
  GzochidMetaClientContainer *self = GZOCHID_META_CLIENT_CONTAINER (object);

  switch (property_id)
    {
    case CONTAINER_PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;

    case CONTAINER_PROP_RESOLUTION_CONTEXT:
      self->resolution_context = g_object_ref_sink (g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

/* A lazy accessor function that creates the actual meta client instance. This
   can't been done during the require / instantiation process (e.g., during
   `construct') because the resolver will leak a reference to itself. */

static GzochidMetaClient *
lazy_init_metaclient (GzochidMetaClientContainer *self)
{
  if (!self->metaclient_initialized)
    {
      GHashTable *metaserver_config = gzochid_configuration_extract_group
	(self->configuration, "metaserver");

      if (gzochid_config_to_boolean
	  (g_hash_table_lookup (metaserver_config, "client.enabled"), FALSE))
	self->metaclient = gzochid_resolver_require_full
	  (self->resolution_context, GZOCHID_TYPE_META_CLIENT, NULL);
      
      g_hash_table_destroy (metaserver_config);

      self->metaclient_initialized = TRUE;
    }

  return self->metaclient;
}

static void
gzochid_meta_client_container_get_property (GObject *object, guint property_id,
					    GValue *value, GParamSpec *pspec)
{
  GzochidMetaClientContainer *self = GZOCHID_META_CLIENT_CONTAINER (object);

  switch (property_id)
    {
    case CONTAINER_PROP_META_CLIENT:
      g_value_set_object (value, lazy_init_metaclient (self));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_meta_client_container_class_init
(GzochidMetaClientContainerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_meta_client_container_dispose;
  object_class->get_property = gzochid_meta_client_container_get_property;
  object_class->set_property = gzochid_meta_client_container_set_property;

  container_obj_properties[CONTAINER_PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  container_obj_properties[CONTAINER_PROP_RESOLUTION_CONTEXT] =
    g_param_spec_object ("resolution-context", "resolution-context",
			 "The global resolution context",
			 GZOCHID_TYPE_RESOLUTION_CONTEXT,
			 G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  container_obj_properties[CONTAINER_PROP_META_CLIENT] = g_param_spec_object
    ("metaclient", "meta-client", "The metaclient, if configured",
     GZOCHID_TYPE_META_CLIENT, G_PARAM_READABLE);

  g_object_class_install_properties
    (object_class, CONTAINER_N_PROPERTIES, container_obj_properties);
}

static void
gzochid_meta_client_container_init (GzochidMetaClientContainer *self)
{
  self->metaclient_initialized = FALSE;
}

/* End boilerplate. */

/* Attempts to connect to the specified hostname and port. Returns a new 
   `gzochid_client_socket' on success, `NULL' on failure (in which case the
   specified `GError' will be set, if specified). */

static gzochid_client_socket *
attempt_connect (GzochidMetaClient *client, char *hostname, unsigned int port,
		 GError **err)
{
  int sock;
  GIOChannel *channel = NULL;
  struct sockaddr_in name;
  struct hostent *hostinfo = NULL;
  char *connection_description = NULL;
  gzochid_client_socket *client_socket = NULL;
  int flag = 1;

  /* Create the client socket. */
  
  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    {
      g_set_error
	(err, GZOCHID_META_CLIENT_ERROR, GZOCHID_META_CLIENT_ERROR_SOCKET,
	 "Couldn't create socket: %s", strerror (errno));
      
      return NULL;
    }

  /* Resolve the meta server's address. */
  
  name.sin_family = AF_INET;
  name.sin_port = htons (port);
  hostinfo = gethostbyname (hostname);

  if (hostinfo == NULL)
    {
      g_set_error
	(err, GZOCHID_META_CLIENT_ERROR, GZOCHID_META_CLIENT_ERROR_NETWORK,
	 "Couldn't resolve %s: %s", hostname, strerror (errno));
      
      return NULL;
    }

  name.sin_addr = *(struct in_addr *) hostinfo->h_addr;

  /* Create the connection. */
  
  if (connect
      (sock, (struct sockaddr *) &name, sizeof (struct sockaddr_in)) < 0)
    {
      g_set_error
	(err, GZOCHID_META_CLIENT_ERROR, GZOCHID_META_CLIENT_ERROR_NETWORK,
	 "Couldn't connect to %s:%d: %s", hostname, port, strerror (errno));

      return NULL;
    }

  setsockopt (sock, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof (int));
#if defined (__APPLE__) && defined (__MACH__)
  setsockopt (sock, SOL_SOCKET, SO_NOSIGPIPE, (char *) &flag, sizeof (int));
#endif

  connection_description = g_strdup_printf ("%s:%d", hostname, port);

  channel = g_io_channel_unix_new (sock);
  
  g_io_channel_set_encoding (channel, NULL, NULL);
  g_io_channel_set_flags (channel, G_IO_FLAG_NONBLOCK, NULL);
  g_io_channel_set_buffered (channel, FALSE);
  
  client_socket = gzochid_client_socket_new
    (channel, connection_description, gzochid_metaclient_client_protocol,
     client);

  g_free (connection_description);

  /* Add the client socket to the socket server's main loop. */
  
  gzochid_client_socket_listen (client->socket_server, client_socket);

  return client_socket;
}

/* The body of the connection maintenance and lock release callback-invoking 
   thread. The algorithm is, in English: While there is no connection to the 
   meta server, attempt to connect. If the connection fails, wait a configured 
   number of seconds before trying again; if it succeeds, go to sleep and wait
   to be notified that the connection has died. */

static gpointer
handle_events (gpointer data)
{
  GzochidMetaClient *client = data;
  
  while (client->running)
    {
      /* Is the socket disconnected? */
      
      while (TRUE)
	{
	  GError *err = NULL;
	  gzochid_client_socket *socket = attempt_connect
	    (client, client->hostname, client->port, &err);

	  /* Did the connection attempt fail? */
	  
	  if (socket == NULL)
	    {
	      if (err != NULL)
		{
		  g_warning
		    ("Failed to connect to meta server at %s:%d: %s; "
		     "waiting %d seconds...", client->hostname, client->port,
		     err->message, client->connect_attempt_interval_seconds);
		  g_error_free (err);
		}
	      else g_warning
		     ("Failed to connect to meta server at %s:%d; "
		      "waiting %d seconds...", client->hostname, client->port,
		      client->connect_attempt_interval_seconds);

	      /* Sleep before trying again, for politeness. */
	      
	      g_usleep (client->connect_attempt_interval_seconds * 1000000);
	    }
	  else
	    {
	      gzochid_reconnectable_socket_connect (client->socket, socket);
	      break;
	    }
	}

      /* Once the socket's connected, run the main loop to handle any events
	 (such as lock release timeouts) are generated by the client in a
	 connected state. Nullifying the connection breaks out of this main loop
	 if it's running. */

      if (client->running)
	g_main_loop_run (client->main_loop);
    }

  return NULL;
}

/* Attempts to set the data ciient's meta server address from the address string
   given in the "metaclient" section of the server configuration. Returns `TRUE'
   if an address of the form "hostname:port" can be parsed out of the string,
   `FALSE" otherwise. */

static gboolean
extract_hostname_port (GzochidMetaClient *metaclient, GError **err)
{
  const char *hostname_port = g_hash_table_lookup
    (metaclient->metaclient_configuration, "server.address");

  if (hostname_port == NULL)
    {
      metaclient->hostname = strdup ("localhost");
      metaclient->port = 9001; /* The default port number. */

      return TRUE;
    }
  else
    {
      GMatchInfo *match_info = NULL;
      GRegex *address_regex = g_regex_new ("([^:]+):(\\d{1,5})", 0, 0, NULL);  
      gboolean matched = g_regex_match
	(address_regex, hostname_port, 0, &match_info);

      g_regex_unref (address_regex);
      
      if (matched)
	{
	  gchar *port_str = g_match_info_fetch (match_info, 2);

	  metaclient->hostname = g_match_info_fetch (match_info, 1);
	  metaclient->port = atoi (port_str);

	  g_free (port_str);
	  g_match_info_free (match_info);
	  
	  return TRUE;
	}
      else
	{
	  g_match_info_free (match_info);
	  
	  g_set_error
	    (err, GZOCHID_META_CLIENT_ERROR, GZOCHID_META_CLIENT_ERROR_ADDRESS,
	     "Invalid meta server address: %s", hostname_port);

	  return FALSE;
	}
    }
}

/*
  Set the admin server base URL; if the admin HTTP server is enabled, 
  retrieve a handle to it and ask it what its base URL is. Otherwise, set the
  admin server base URL to a zero-length string.

  Note that the server's base URL is only available after it has been started.
  For the reason, the data client should be started after the HTTP server (if 
  enabled). 
*/

static void
init_admin_server_base_url (GzochidMetaClient *client)
{
  GHashTable *admin_configuration = gzochid_configuration_extract_group
    (client->configuration, "admin");

  if (gzochid_config_to_boolean
      (g_hash_table_lookup (admin_configuration, "module.httpd.enabled"),
       FALSE))
    {
      GzochidHttpServer *http_server = gzochid_resolver_require_full
	(client->resolution_context, GZOCHID_TYPE_HTTP_SERVER, NULL);

      /* Make a copy of the base URL; we're not holding a reference to the HTTP
	 server, so it could be subsequently disposed / finalized. */
      
      client->admin_server_base_url =
	strdup (gzochid_http_server_get_base_url (http_server));

      g_object_unref (http_server);
    }
  else client->admin_server_base_url = strdup ("");
  
  g_hash_table_destroy (admin_configuration);
}

void
gzochid_metaclient_start (GzochidMetaClient *metaclient, GError **err)
{
  GError *local_err = NULL;

  if (!extract_hostname_port (metaclient, &local_err))
    {
      if (local_err != NULL)
	g_propagate_error (err, local_err);
      return;
    }

  init_admin_server_base_url (metaclient);

  metaclient->connect_attempt_interval_seconds = gzochid_config_to_int
    (g_hash_table_lookup (metaclient->metaclient_configuration,
			  "server.connect.interval.sec"), 5);

  /* Set the running flag to keep the maintenance loop going. */
  
  metaclient->running = TRUE;
  metaclient->thread = g_thread_new ("metaclient", handle_events, metaclient);
}

void
gzochid_metaclient_stop (GzochidMetaClient *metaclient)
{
  if (metaclient->thread != NULL)
    {
      metaclient->running = FALSE;
      g_main_loop_quit (metaclient->main_loop);

      /* Ensure that the maintenance thread has exited. */
      
      g_thread_join (metaclient->thread);

      g_free (metaclient->hostname);
      metaclient->hostname = NULL;
      metaclient->port = 0;

      /* Clean up the admin server base URL. */
      
      free (metaclient->admin_server_base_url);
      metaclient->admin_server_base_url = NULL;
    }
}

void
gzochid_metaclient_nullify_connection (GzochidMetaClient *metaclient)
{
  GzochidEvent *event = GZOCHID_EVENT
    (g_object_new (GZOCHID_TYPE_META_SERVER_EVENT,
		   "type", META_SERVER_DISCONNECTED,
		   NULL));

  gzochid_reconnectable_socket_disconnect (metaclient->socket);
  gzochid_event_dispatch (metaclient->event_source, event);
  
  g_object_unref (event);
      
  g_main_loop_quit (metaclient->main_loop);
}

GQuark
gzochid_meta_client_error_quark ()
{
  return g_quark_from_static_string ("gzochid-meta-client-error-quark");
}
