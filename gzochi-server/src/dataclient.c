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

#include <errno.h>
#include <glib.h>
#include <glib-object.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "config.h"
#include "dataclient.h"
#include "dataclient-protocol.h"
#include "socket.h"

/* Boilerplate setup for the data client object. */

/* The data client object. */

struct _GzochidDataClient
{
  GObject parent_instance;

  GHashTable *configuration; /* The data client configuration table. */

  /* The socket server for the data client. */

  GzochidSocketServer *socket_server;

  /* The client's current connection to the data server. */

  gzochid_client_socket *client_socket;

  char *hostname; /* Meta server hostname. */
  unsigned int port; /* Meta server port. */

  /* Interval between meta server connection attempts while in a disconnected 
     state. */

  guint connect_attempt_interval_seconds;
  
  GThread *thread; /* The connection maintenance thread. */
  GMutex mutex; /* Mutex protecting the client socket. */
  GCond cond; /* Condition variable coordinating connection maintenance. */
};

/* Boilerplate setup for the data client object. */

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

enum gzochid_data_client_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_SOCKET_SERVER,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_data_client_set_property (GObject *object, guint property_id,
				  const GValue *value, GParamSpec *pspec)
{
  GzochidDataClient *self = GZOCHID_DATA_CLIENT (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = gzochid_configuration_extract_group
	(GZOCHID_CONFIGURATION (g_value_get_object (value)), "dataclient");
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_data_client_finalize (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);

  g_hash_table_destroy (client->configuration);

  g_mutex_clear (&client->mutex);
  g_cond_clear (&client->cond);
}

static void
gzochid_data_client_dispose (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);

  g_object_unref (client->socket_server);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_data_client_dispose;
  object_class->finalize = gzochid_data_client_finalize;
  object_class->set_property = gzochid_data_client_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "gzochi data client configuration",
     "Set the gzochi server configuration", GZOCHID_TYPE_CONFIGURATION,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "Socket server", "Set the socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  g_mutex_init (&self->mutex);
  g_cond_init (&self->cond);
}

/* End boiletplate. */

/* Attempts to connect to the specified hostname and port. Returns a new 
   `gzochid_client_socket' on success, `NULL' on failure (in which case the
   specified `GError' will be set, if specified). */

static gzochid_client_socket *
attempt_connect (GzochidDataClient *client, char *hostname, unsigned int port,
		 GError **err)
{
  int sock;
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
  
  client_socket = gzochid_client_socket_new
    (g_io_channel_unix_new (sock), connection_description,
     gzochid_dataclient_client_protocol, client);

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

  while (TRUE)
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
	      client->client_socket = socket;
	      g_info ("Connected to meta server at %s:%d.", client->hostname,
		      client->port);

	      /* Notify any waiting clients that the connection is open for
		 business. */
	      
	      g_cond_broadcast (&client->cond);
	    }
	}

      /* Wait for an event. */
      
      g_cond_wait (&client->cond, &client->mutex);
    }

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
    (dataclient->configuration, "server.address");

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

      if (g_regex_match (address_regex, hostname_port, 0, &match_info))
	{
	  dataclient->hostname = g_match_info_fetch (match_info, 1);
	  dataclient->port = atoi (g_match_info_fetch (match_info, 2));

	  return TRUE;
	}
      else
	{
	  g_set_error
	    (err, GZOCHID_DATA_CLIENT_ERROR, GZOCHID_DATA_CLIENT_ERROR_ADDRESS,
	     "Invalid meta server address: %s", hostname_port);

	  return FALSE;
	}
    }
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

  dataclient->connect_attempt_interval_seconds = gzochid_config_to_int
    (g_hash_table_lookup (dataclient->configuration,
			  "server.connect.interval.sec"), 5);
  
  dataclient->thread = g_thread_new
    ("dataclient-conn", ensure_connection, dataclient);
}

void
gzochid_dataclient_stop (GzochidDataClient *dataclient)
{
}

void
gzochid_dataclient_nullify_connection (GzochidDataClient *dataclient)
{
  g_mutex_lock (&dataclient->mutex);

  dataclient->client_socket = NULL;

  g_cond_signal (&dataclient->cond);
  g_mutex_unlock (&dataclient->mutex);
}

GQuark
gzochid_data_client_error_quark ()
{
  return g_quark_from_static_string ("gzochid-data-client-error-quark");
}
