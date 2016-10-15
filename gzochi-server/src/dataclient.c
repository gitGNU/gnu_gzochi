/* dataclient.c: Data client for gzochid
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
#include <errno.h>
#include <glib.h>
#include <glib-object.h>
#include <gzochi-common.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "config.h"
#include "data-protocol.h"
#include "dataclient.h"
#include "dataclient-protocol.h"
#include "event.h"
#include "event-app.h"
#include "httpd.h"
#include "resolver.h"
#include "socket.h"

/* Captures callback configuration for a request issued through the data 
   client. */

struct _dataclient_callback_registration
{
  /* The expected opcode of the response. Used as a kind of check bit to ensure
     that the correct response is being processed on behalf of a request. */

  unsigned char expected_opcode; 
  
  /* The success callback. */

  gzochid_dataclient_success_callback success_callback; 

  gpointer success_data; /* Closure data for the success callback. */

  /* The failure callback. */
  
  gzochid_dataclient_failure_callback failure_callback;

  gpointer failure_data; /* Closure data for the failure callback. */
};

typedef struct _dataclient_callback_registration
dataclient_callback_registration;

/* Holds the state of active callbacks for value and oid requests. */

struct _dataclient_callback_queue
{  
  GMutex mutex; /* Mutex to protect the list of registrations */

  /* The oids callback function. */

  gzochid_dataclient_oids_callback oids_callback; 

  gpointer oids_callback_data; /* Closure data for the oids callback. */  

  /* List of `dataclient_callback_registration' objects. */

  GList *callback_registrations; 
};

typedef struct _dataclient_callback_queue dataclient_callback_queue;

/* Boilerplate setup for the data client object. */

/* The data client object. */

struct _GzochidDataClient
{
  GObject parent_instance;

  GzochidConfiguration *configuration; /* The global configuration object. */

  /* The data client configuration table; extracted from the global 
     configuration object and cached for convenience. */

  GHashTable *dataclient_configuration; 

  /* The global resolution context; used for getting a (temporary) handle to
     the container's embedded HTTP server (if available) to extract the base
     URL to send to the meta server as part of the login request. */
  
  GzochidResolutionContext *resolution_context;
  
  /* A map of application names to `dataclient_callback_queue' objects. */
  
  GHashTable *application_callback_queues; 

  GMutex queue_mutex; /* Synchronizes interactions with the callback queues. */
   
  /* The socket server for the data client. */

  GzochidSocketServer *socket_server;

  /* An event source for posting meta server connection / disconnection 
     events. */
  
  gzochid_event_source *event_source;

  /* When the data client has been started, holds the base URL of the gzochid 
     admin HTTP console, if available; otherwise, a heap-allocated empty 
     string. */
  
  char *admin_server_base_url;
  
  /* The client's current connection to the data server. */

  gzochid_client_socket *client_socket;

  /* A buffer of bytes to be sent to the data server pending the availability of
     a connected client socket. */
  
  GByteArray *outbound_messages;
  
  char *hostname; /* Meta server hostname. */
  unsigned int port; /* Meta server port. */

  /* Interval between meta server connection attempts while in a disconnected 
     state. */

  guint connect_attempt_interval_seconds;
  
  GThread *thread; /* The connection maintenance thread. */
  
  /* Whether the connection maintenance thread should stay alive. */

  gboolean running; 

  GMutex mutex; /* Mutex protecting the client socket. */
  GCond cond; /* Condition variable coordinating connection maintenance. */
};

/* Boilerplate setup for the data client object. */

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

enum gzochid_data_client_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_RESOLUTION_CONTEXT,
    PROP_SOCKET_SERVER,
    PROP_CONNECTION_DESCRIPTION,
    PROP_EVENT_LOOP,
    PROP_EVENT_SOURCE,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_data_client_get_property (GObject *object, guint property_id,
				  GValue *value, GParamSpec *pspec)
{
  GzochidDataClient *data_client = GZOCHID_DATA_CLIENT (object);

  switch (property_id)
    {
    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, data_client->event_source);
      break;

    case PROP_CONNECTION_DESCRIPTION:      
      if (data_client->client_socket != NULL)
	g_value_set_static_string
	  (value, gzochid_client_socket_get_connection_description
	   (data_client->client_socket));
      else g_value_set_static_string (value, NULL);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_data_client_set_property (GObject *object, guint property_id,
				  const GValue *value, GParamSpec *pspec)
{
  GzochidDataClient *self = GZOCHID_DATA_CLIENT (object);

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
gzochid_data_client_constructed (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);  

  /* Extract and save the "metaserver" configuration group to use to look up
     connection details. */
  
  client->dataclient_configuration = gzochid_configuration_extract_group
    (client->configuration, "metaserver");  
}

static void
gzochid_data_client_finalize (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);
  
  g_hash_table_destroy (client->dataclient_configuration);
  g_hash_table_destroy (client->application_callback_queues);

  if (client->admin_server_base_url != NULL)
    free (client->admin_server_base_url);
  
  g_byte_array_unref (client->outbound_messages);

  g_source_destroy ((GSource *) client->event_source);
  g_source_unref ((GSource *) client->event_source);
    
  g_mutex_clear (&client->queue_mutex);
  g_mutex_clear (&client->mutex);
  g_cond_clear (&client->cond);
}

static void
gzochid_data_client_dispose (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);

  g_object_unref (client->configuration);
  g_object_unref (client->resolution_context);
  g_object_unref (client->socket_server);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = gzochid_data_client_constructed;
  object_class->dispose = gzochid_data_client_dispose;
  object_class->finalize = gzochid_data_client_finalize;
  object_class->get_property = gzochid_data_client_get_property;
  object_class->set_property = gzochid_data_client_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "gzochi data client configuration",
     "Set the gzochi server configuration", GZOCHID_TYPE_CONFIGURATION,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_RESOLUTION_CONTEXT] = g_param_spec_object
    ("resolution-context", "gzochi data client resolution context",
     "Set the gzochi server resolution context",
     GZOCHID_TYPE_RESOLUTION_CONTEXT,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);
  
  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "Socket server", "Set the socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_CONNECTION_DESCRIPTION] = g_param_spec_string
    ("connection-description", "Connection description",
     "Get the connection description", NULL, G_PARAM_READABLE);

  obj_properties[PROP_EVENT_LOOP] = g_param_spec_object
    ("event-loop", "Event loop", "Set the event loop", GZOCHID_TYPE_EVENT_LOOP,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);
  
  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "Event source", "Get the event source", G_TYPE_SOURCE,
     G_PARAM_READABLE);
  
  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

/* Create and return a new callback registration object with the specified
   expected opcode and success and failure callback (with associated user data
   pointers. This object should freed via `free' when no longer needed. */

static dataclient_callback_registration *
create_callback
(unsigned char expected_opcode,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data)
{
  dataclient_callback_registration *registration =
    malloc (sizeof (dataclient_callback_registration));

  registration->expected_opcode = expected_opcode;
  
  registration->success_callback = success_callback;
  registration->success_data = success_data;

  registration->failure_callback = failure_callback;
  registration->failure_data = failure_data;

  return registration;
}

/* Frees the callback queue structure, including all pending callbacks. */

static void
free_callback_queue (dataclient_callback_queue *queue)
{
  g_mutex_clear (&queue->mutex);
  g_list_free_full (queue->callback_registrations, free);
  free (queue);
}

/*
  Returns the callback queue structure associated with the specified gzochi 
  game application name, creating one if necessary.

  Note that in order to support safe concurrent access to the queue's internal
  fields, the callback queue is returned with its mutex locked. Callers must
  unlock release the queue when they are done using it via a call to 
  `release_callback_queue'.
*/

static dataclient_callback_queue *
acquire_callback_queue (GzochidDataClient *client, char *app)
{
  dataclient_callback_queue *queue = NULL;
  
  g_mutex_lock (&client->queue_mutex);

  if (g_hash_table_contains (client->application_callback_queues, app))   
    queue = g_hash_table_lookup (client->application_callback_queues, app);
  else
    {
      queue = malloc (sizeof (dataclient_callback_queue));

      g_mutex_init (&queue->mutex);

      queue->oids_callback = NULL;
      queue->oids_callback_data = NULL;
      queue->callback_registrations = NULL;

      g_hash_table_insert
	(client->application_callback_queues, strdup (app), queue);
    }

  g_mutex_lock (&queue->mutex);  
  g_mutex_unlock (&client->queue_mutex);

  return queue;
}

/* Releases the specified callback queue structure. */

static void
release_callback_queue (dataclient_callback_queue *queue)
{
  g_mutex_unlock (&queue->mutex);
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  self->application_callback_queues = g_hash_table_new_full
    (g_str_hash, g_str_equal, free, (GDestroyNotify) free_callback_queue);

  self->outbound_messages = g_byte_array_new ();

  self->running = FALSE;

  self->event_source = gzochid_event_source_new ();
  
  g_mutex_init (&self->queue_mutex);
  g_mutex_init (&self->mutex);
  g_cond_init (&self->cond);
}

/* End boilerplate. */

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

/* Attempts to connect to the specified hostname and port. Returns a new 
   `gzochid_client_socket' on success, `NULL' on failure (in which case the
   specified `GError' will be set, if specified). */

static gzochid_client_socket *
attempt_connect (GzochidDataClient *client, char *hostname, unsigned int port,
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
	(err, GZOCHID_DATA_CLIENT_ERROR, GZOCHID_DATA_CLIENT_ERROR_SOCKET,
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
	(err, GZOCHID_DATA_CLIENT_ERROR, GZOCHID_DATA_CLIENT_ERROR_NETWORK,
	 "Couldn't resolve %s: %s", hostname, strerror (errno));
      
      return NULL;
    }

  name.sin_addr = *(struct in_addr *) hostinfo->h_addr;

  /* Create the connection. */
  
  if (connect
      (sock, (struct sockaddr *) &name, sizeof (struct sockaddr_in)) < 0)
    {
      g_set_error
	(err, GZOCHID_DATA_CLIENT_ERROR, GZOCHID_DATA_CLIENT_ERROR_NETWORK,
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
    (channel, connection_description, gzochid_dataclient_client_protocol,
     client);

  g_free (connection_description);

  /* Add the client socket to the socket server's main loop. */
  
  gzochid_client_socket_listen (client->socket_server, client_socket);

  return client_socket;
}

/* The body of the connection maintenance thread. The algorithm is, in English:
   While there is no connection to the meta server, attempt to connect. If the
   connection fails, wait a configured number of seconds before trying again; if
   it succeeds, go to sleep and wait to be notified that the connection has 
   died. */

static gpointer
ensure_connection (gpointer data)
{
  GzochidDataClient *client = data;

  g_mutex_lock (&client->mutex);

  while (client->running)
    {
      /* Is the socket disconnected? */
      
      while (client->client_socket == NULL)
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
	      GBytes *login_message_payload = NULL;
	      GByteArray *login_message_payload_arr = g_byte_array_new ();
	      unsigned char *login_message_bytes = NULL;
	      size_t login_message_len = 0;
	      
	      client->client_socket = socket;
	      g_message ("Connected to meta server at %s:%d.", client->hostname,
			 client->port);

	      /* Create the message payload by concatenating the protocol
		 version byte with the admin server base URL; the base URL will
		 never be `NULL', though it may be an empty string. */
	      
	      g_byte_array_append
		(login_message_payload_arr,
		 (unsigned char[]) { GZOCHID_DATACLIENT_PROTOCOL_VERSION }, 1);
	      
	      g_byte_array_append
		(login_message_payload_arr,
		 (unsigned char *) client->admin_server_base_url,
		 strlen (client->admin_server_base_url) + 1);

	      login_message_payload = g_byte_array_free_to_bytes
		(login_message_payload_arr);

	      /* Prefix the message with its length and opcode. */
	      
	      login_message_bytes = format_message
		(GZOCHID_DATA_PROTOCOL_LOGIN, login_message_payload,
		 &login_message_len);

	      /* Enqueue it to be sent; can't use `write_message' here because
		 we already hold the client mutex. */
	      
	      gzochid_client_socket_write
		(client->client_socket, login_message_bytes, login_message_len);
	      
	      g_bytes_unref (login_message_payload);
	      free (login_message_bytes);
	      
	      if (client->outbound_messages->len > 0)
		{
		  gzochid_client_socket_write
		    (client->client_socket, client->outbound_messages->data,
		     client->outbound_messages->len);

		  g_byte_array_set_size (client->outbound_messages, 0);
		}
	      
	      /* Notify any waiting clients that the connection is open for
		 business. */
	      
	      g_cond_broadcast (&client->cond);
	    }
	}

      /* Wait for an event. */
      
      g_cond_wait (&client->cond, &client->mutex);
    }

  g_mutex_unlock (&client->mutex);  
  return NULL;
}

/* Attempts to set the data ciient's meta server address from the address string
   given in the "dataclient" section of the server configuration. Returns `TRUE'
   if an address of the form "hostname:port" can be parsed out of the string,
   `FALSE" otherwise. */

static gboolean
extract_hostname_port (GzochidDataClient *dataclient, GError **err)
{
  const char *hostname_port = g_hash_table_lookup
    (dataclient->dataclient_configuration, "server.address");

  if (hostname_port == NULL)
    {
      dataclient->hostname = strdup ("localhost");
      dataclient->port = 9001; /* The default port number. */

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

	  dataclient->hostname = g_match_info_fetch (match_info, 1);
	  dataclient->port = atoi (port_str);

	  g_free (port_str);
	  g_match_info_free (match_info);
	  
	  return TRUE;
	}
      else
	{
	  g_match_info_free (match_info);
	  
	  g_set_error
	    (err, GZOCHID_DATA_CLIENT_ERROR, GZOCHID_DATA_CLIENT_ERROR_ADDRESS,
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
init_admin_server_base_url (GzochidDataClient *client)
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
gzochid_dataclient_start (GzochidDataClient *dataclient, GError **err)
{
  GError *local_err = NULL;

  if (!extract_hostname_port (dataclient, &local_err))
    {
      if (local_err != NULL)
	g_propagate_error (err, local_err);
      return;
    }

  init_admin_server_base_url (dataclient);
  
  dataclient->connect_attempt_interval_seconds = gzochid_config_to_int
    (g_hash_table_lookup (dataclient->dataclient_configuration,
			  "server.connect.interval.sec"), 5);

  /* Set the running flag to keep the maintenance loop going. */
  
  dataclient->running = TRUE;
  dataclient->thread = g_thread_new
    ("dataclient-conn", ensure_connection, dataclient);
}

void
gzochid_dataclient_stop (GzochidDataClient *dataclient)
{
  if (dataclient->thread != NULL)
    {
      g_mutex_lock (&dataclient->mutex);

      /* Unset the running flag and wake up the maintenance thread so that it
	 can exit. */
      
      dataclient->running = FALSE;
      g_cond_signal (&dataclient->cond);

      g_mutex_unlock (&dataclient->mutex);

      /* Ensure that the maintenance thread has exited. */
      
      g_thread_join (dataclient->thread);

      g_free (dataclient->hostname);
      dataclient->hostname = NULL;
      dataclient->port = 0;

      /* Clean up the admin server base URL. */
      
      free (dataclient->admin_server_base_url);
      dataclient->admin_server_base_url = NULL;
    }
}

void
gzochid_dataclient_nullify_connection (GzochidDataClient *dataclient)
{
  GzochidEvent *event = GZOCHID_EVENT
    (g_object_new (GZOCHID_TYPE_META_SERVER_EVENT,
		   "type", META_SERVER_DISCONNECTED,
		   NULL));
  
  g_mutex_lock (&dataclient->mutex);

  dataclient->client_socket = NULL;
  gzochid_event_dispatch (dataclient->event_source, event);
  
  g_cond_signal (&dataclient->cond);
  g_mutex_unlock (&dataclient->mutex);

  g_object_unref (event);
}

/*
  Convenience function to write the specified opcode and byte payload to the
  connected metaserver.

  This function does not attempt to recover from cases in which a message is 
  only partially sent before the socket is disconnected. Under these 
  circumstances, clients should make no assumptions about the state of submitted
  changesets and should purge all internal state. */

static void
write_message (GzochidDataClient *client, guchar opcode, GBytes *payload)
{
  size_t len = 0;
  unsigned char *buf = format_message (opcode, payload, &len);
  
  g_mutex_lock (&client->mutex);
  
  if (client->client_socket != NULL)

    /* Queue up the bytes to sent on the socket. */
    
    gzochid_client_socket_write (client->client_socket, buf, len);
  else

    /* If the client is known to be disconnected, queue up the message bytes to
       be sent on reconnnect. */
    
    g_byte_array_append (client->outbound_messages, buf, len);
  
  g_mutex_unlock (&client->mutex);
  
  free (buf);
}

void
gzochid_dataclient_received_oids (GzochidDataClient *client,
				  gzochid_data_reserve_oids_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  /* If somebody's waiting for oids, let them know. */
  
  if (queue->oids_callback != NULL)
    {
      queue->oids_callback (response->block, queue->oids_callback_data);

      /* Null out the oids callback after receipt. */
      
      queue->oids_callback = NULL;
      queue->oids_callback_data = NULL;
    }

  release_callback_queue (queue);
}

void
gzochid_dataclient_reserve_oids (GzochidDataClient *client, char *app,
				 gzochid_dataclient_oids_callback callback,
				 gpointer user_data)
{
  GBytes *message_bytes = g_bytes_new (app, strlen (app) + 1);
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_OIDS, message_bytes);
  g_bytes_unref (message_bytes);

  /* The client is responsible for synchronizing / serializing calls to this
     function. */
  
  assert (queue->oids_callback == NULL);
  queue->oids_callback = callback;
  queue->oids_callback_data = user_data;

  release_callback_queue (queue);
}

/* Convenience function to handle the processing of a message received in
   response to a value or sequential key request, and representing a successful
   or unsuccessful fulfillment of the request. Some error checking is performed
   to ensure that responses message are received in the same order in which the
   requests were submitted. */

static void
process_queued_callback (dataclient_callback_queue *queue, unsigned char opcode,
			 gzochid_data_response *response)
{
  GList *callback_link = queue->callback_registrations;
  dataclient_callback_registration *callbacks = callback_link->data;

  queue->callback_registrations = g_list_delete_link
    (queue->callback_registrations, queue->callback_registrations);

  /* Check that the message opcode is the same as the opcode expected by the
     callback registration at the head of the queue. */
  
  if (opcode != callbacks->expected_opcode)
    g_warning
      ("Received response %d for %s/%s; expected response %d.", opcode,
       response->app, response->store, callbacks->expected_opcode);
  else if (response->success)
    callbacks->success_callback (response->data, callbacks->success_data);
  else callbacks->failure_callback (response->timeout, callbacks->failure_data);

  free (callbacks);      
}

void
gzochid_dataclient_received_value (GzochidDataClient *client,
				   gzochid_data_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  if (queue->callback_registrations == NULL)
    g_warning
      ("Received value for %s/%s but no callbacks registered.", response->app,
       response->store);
  else process_queued_callback
	 (queue, GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE, response);

  release_callback_queue (queue);
}

void
gzochid_dataclient_request_value
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gboolean for_write, gzochid_dataclient_success_callback success_callback,
 gpointer success_data, gzochid_dataclient_failure_callback failure_callback,
 gpointer failure_data)
{
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  GByteArray *payload = g_byte_array_new ();
  const unsigned char *key_bytes = NULL;
  size_t payload_len = 0, key_len = 0;
  GBytes *payload_bytes = NULL;

  /* Serialize the value request message. */
  
  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);
  g_byte_array_append
    (payload, (unsigned char *) &(unsigned char[]) { for_write ? 1 : 0 }, 1);

  key_bytes = g_bytes_get_data (key, &key_len);

  /* Grow the byte array by 2 bytes. */
  
  payload_len = payload->len;
  g_byte_array_set_size (payload, payload_len + 2);

  /* Write the key length directly to the buffer. */
  
  gzochi_common_io_write_short (key_len, payload->data, payload_len);
  
  g_byte_array_append (payload, key_bytes, key_len);
  
  payload_bytes = g_byte_array_free_to_bytes (payload);
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_VALUE, payload_bytes);
  g_bytes_unref (payload_bytes);

  /* Add a callback registration to the queue. */
  
  queue->callback_registrations = g_list_append
    (queue->callback_registrations,
     create_callback (GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE,
		      success_callback, success_data,
		      failure_callback, failure_data));

  release_callback_queue (queue);
}

void
gzochid_dataclient_received_next_key (GzochidDataClient *client,
				      gzochid_data_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  if (queue->callback_registrations == NULL)
    g_warning
      ("Received key range response for %s/%s but no callbacks registered.",
       response->app, response->store);
  else process_queued_callback
	 (queue, GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE, response);

  release_callback_queue (queue);
}

void
gzochid_dataclient_request_next_key
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data)
{
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  GByteArray *payload = g_byte_array_new ();
  GBytes *payload_bytes = NULL;
  
  /* Serialize the key request message. */

  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);

  if (key != NULL)
    {
      size_t payload_len = 0, key_len = 0;
      const unsigned char *key_bytes = g_bytes_get_data (key, &key_len);

      /* Grow the byte array by 2 bytes. */
      
      payload_len = payload->len;
      g_byte_array_set_size (payload, payload_len + 2);

      /* Write the key length directly to the buffer. */
  
      gzochi_common_io_write_short (key_len, payload->data, payload_len);

      g_byte_array_append (payload, key_bytes, key_len);
    }
  else g_byte_array_append
	 (payload, (unsigned char *) &(unsigned char[]) { 0, 0 }, 2);

  payload_bytes = g_byte_array_free_to_bytes (payload);
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY, payload_bytes);
  g_bytes_unref (payload_bytes);
  
  /* Add a callback registration to the queue. */

  queue->callback_registrations = g_list_append
    (queue->callback_registrations,
     create_callback (GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE,
		      success_callback, success_data,
		      failure_callback, failure_data));

  release_callback_queue (queue);
}

void
gzochid_dataclient_submit_changeset
(GzochidDataClient *client, char *app, GArray *changes)
{
  gzochid_data_changeset *changeset = gzochid_data_changeset_new (app, changes);
  GByteArray *payload = g_byte_array_new ();
  GBytes *payload_bytes = NULL;

  /* Serialize the changeset submission message. */
  
  gzochid_data_protocol_changeset_write (changeset, payload);

  payload_bytes = g_byte_array_free_to_bytes (payload); 
  write_message (client, GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET, payload_bytes);
  g_bytes_unref (payload_bytes);

  gzochid_data_changeset_free (changeset);
}

GQuark
gzochid_data_client_error_quark ()
{
  return g_quark_from_static_string ("gzochid-data-client-error-quark");
}
