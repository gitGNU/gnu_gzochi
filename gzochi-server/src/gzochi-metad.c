/* gzochi-metad.c: Main server bootstrapping routines for gzochi-metad
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

#include <config.h>
#include <getopt.h>
#include <glib.h>
#include <glib-object.h>
#include <libintl.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "channelserver.h"
#include "config.h"
#include "dataserver.h"
#include "event.h"
#include "httpd-meta.h"
#include "httpd.h"
#include "log.h"
#include "metaserver-protocol.h"
#include "resolver.h"
#include "sessionserver.h"
#include "socket.h"

#define _(String) gettext (String)

#define Q(x) #x
#define QUOTE(x) Q(x)

#ifndef GZOCHI_METAD_CONF_LOCATION
#define GZOCHI_METAD_CONF_LOCATION "/etc/gzochi-metad.conf"
#endif /* GZOCHI_METAD_CONF_LOCATION */

#define GZOCHI_METAD_TYPE_ROOT_CONTEXT gzochi_metad_root_context_get_type ()

/* Boilerplate setup for the gzochi-metad root context. */

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochi_metad_root_context_get_type (void);

struct _GzochiMetadRootContextClass
{
  GObjectClass parent_class;
};

typedef struct _GzochiMetadRootContextClass GzochiMetadRootContextClass;

/* The root context object. */

struct _GzochiMetadRootContext
{
  GObject parent_instance;

  GzochidConfiguration *configuration; /* The global configuration. */  
  GzochidHttpServer *http_server; /* The admin web console. */

  /* When the data server has been started, holds the base URL of the gzochid 
     admin HTTP console, if available; otherwise, a heap-allocated empty 
     string. */

  char *admin_server_base_url;
  
  GzochidEventLoop *event_loop; /* The global event loop. */

  /* An event source for client connect / disconnect events. */

  gzochid_event_source *event_source; 

  /* The meta server global socket server. */

  GzochidSocketServer *socket_server;

  gzochid_server_socket *server_socket; /* The meta server's server socket. */

  /* The meta server channel server. */
  
  GzochiMetadChannelServer *channel_server;
  
  GzochiMetadDataServer *data_server; /* The meta server data server. */

  /* The meta server session server. */

  GzochiMetadSessionServer *session_server; 

  /* The port on which the server listens. This may be zero to indicate the 
     server should listen on any available port, and will be set to the port 
     assigned by the operating system. */
  
  int port;
};

typedef struct _GzochiMetadRootContext GzochiMetadRootContext;

static inline GzochiMetadRootContext *
GZOCHI_METAD_ROOT_CONTEXT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_root_context_get_type (), GzochiMetadRootContext);
}

G_DEFINE_TYPE (GzochiMetadRootContext, gzochi_metad_root_context,
	       G_TYPE_OBJECT);

enum gzochi_metad_root_context_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_HTTP_SERVER,
    PROP_ADMIN_SERVER_BASE_URL,
    PROP_EVENT_LOOP,
    PROP_EVENT_SOURCE,
    PROP_SOCKET_SERVER,
    PROP_CHANNEL_SERVER,
    PROP_DATA_SERVER,
    PROP_SESSION_SERVER,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
root_context_get_property (GObject *object, guint property_id, GValue *value,
			   GParamSpec *pspec)
{
  GzochiMetadRootContext *self = GZOCHI_METAD_ROOT_CONTEXT (object);

  switch (property_id)
    {
    case PROP_ADMIN_SERVER_BASE_URL:
      g_value_set_static_string (value, self->admin_server_base_url);
      break;

    case PROP_EVENT_SOURCE:
      g_value_set_boxed (value, self->event_source);
      break;

    case PROP_CHANNEL_SERVER:
      g_value_set_object (value, self->channel_server);
      break;
      
    case PROP_DATA_SERVER:
      g_value_set_object (value, self->data_server);
      break;

    case PROP_SESSION_SERVER:
      g_value_set_object (value, self->session_server);
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
  GzochiMetadRootContext *self = GZOCHI_METAD_ROOT_CONTEXT (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_HTTP_SERVER:
      self->http_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_EVENT_LOOP:
      self->event_loop = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_CHANNEL_SERVER:
      self->channel_server = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_DATA_SERVER:
      self->data_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_SESSION_SERVER:
      self->session_server = g_object_ref (g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
root_context_constructed (GObject *object)
{
  GzochiMetadRootContext *root_context = GZOCHI_METAD_ROOT_CONTEXT (object);
  GHashTable *meta_configuration = gzochid_configuration_extract_group
    (root_context->configuration, "meta");
    
  root_context->port = gzochid_config_to_int
    (g_hash_table_lookup (meta_configuration, "server.port"), 9001);

  g_hash_table_destroy (meta_configuration);
}

static void
root_context_dispose (GObject *object)
{
  GzochiMetadRootContext *root_context = GZOCHI_METAD_ROOT_CONTEXT (object);

  g_object_unref (root_context->configuration);

  G_OBJECT_CLASS (gzochi_metad_root_context_parent_class)->dispose (object);
}

static void
root_context_finalize (GObject *object)
{
  GzochiMetadRootContext *root_context = GZOCHI_METAD_ROOT_CONTEXT (object);
  
  g_source_destroy ((GSource *) root_context->event_source);
  g_source_unref ((GSource *) root_context->event_source);

  G_OBJECT_CLASS (gzochi_metad_root_context_parent_class)->finalize (object);
}

static void
gzochi_metad_root_context_class_init (GzochiMetadRootContextClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = root_context_constructed;
  object_class->dispose = root_context_dispose;
  object_class->finalize = root_context_finalize;
  object_class->get_property = root_context_get_property;
  object_class->set_property = root_context_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);
  
  obj_properties[PROP_HTTP_SERVER] = g_param_spec_object
    ("http-server", "httpd", "The admin HTTP server", GZOCHID_TYPE_HTTP_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_ADMIN_SERVER_BASE_URL] = g_param_spec_string
    ("admin-server-base-url", "base-url", "The admin server base URL", NULL,
     G_PARAM_READABLE);
  
  obj_properties[PROP_EVENT_LOOP] = g_param_spec_object
    ("event-loop", "event-loop", "The global event loop",
     GZOCHID_TYPE_EVENT_LOOP, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);
  
  obj_properties[PROP_EVENT_SOURCE] = g_param_spec_boxed
    ("event-source", "event-source", "The global event source",
     G_TYPE_SOURCE, G_PARAM_READABLE);

  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "socket-server", "The global socket server",
     GZOCHID_TYPE_SOCKET_SERVER, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_CHANNEL_SERVER] = g_param_spec_object
    ("channel-server", "channel-server", "The channel server",
     GZOCHI_METAD_TYPE_CHANNEL_SERVER,
     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_DATA_SERVER] = g_param_spec_object
    ("data-server", "data-server", "The data server",
     GZOCHI_METAD_TYPE_DATA_SERVER, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_SESSION_SERVER] = g_param_spec_object
    ("session-server", "session-server", "The session server",
     GZOCHI_METAD_TYPE_SESSION_SERVER,
     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_root_context_init (GzochiMetadRootContext *self)
{
  self->event_source = gzochid_event_source_new ();
  self->server_socket = gzochid_server_socket_new
    ("Meta server", gzochi_metad_metaserver_server_protocol, self);
  self->port = 0;
}

static const struct option longopts[] =
  {
    { "config", required_argument, NULL, 'c' },
    { "help", no_argument, NULL, 'h' },
    { "version", no_argument, NULL, 'v' },
    { NULL, 0, NULL, 0 }
  };

/* Prints the version and copyright statement to standard output. */

static void
print_version (void)
{
  printf ("gzochi-metad (gzochi) %s\n", VERSION);

  puts ("");
  printf (_("\
Copyright (C) %s Julian Graham\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"),
	  "2017");
}

/* Prints help and configuration options to standard output. */

static void
print_help (const char *program_name)
{
  printf (_("\
Usage: %s [OPTION]...\n"), program_name);
  
  puts ("");
  fputs (_("\
  -c, --config        full path to gzochi-metad.conf\n\
  -h, --help          display this help and exit\n\
  -v, --version       display version information and exit\n"), stdout);

  puts ("");
  printf (_("\
Report bugs to: %s\n"), PACKAGE_BUGREPORT);
#ifdef PACKAGE_PACKAGER_BUG_REPORTS
  printf (_("Report %s bugs to: %s\n"), PACKAGE_PACKAGER,
          PACKAGE_PACKAGER_BUG_REPORTS);
#endif /* PACKAGE_PACKAGER_BUG_REPORTS */

#ifdef PACKAGE_URL
  printf (_("%s home page: <%s>\n"), PACKAGE_NAME, PACKAGE_URL);
#else
  printf (_("%s home page: <http://www.nongnu.org/%s/>\n"),
          PACKAGE_NAME, PACKAGE);
#endif /* PACKAGE_URL */
}

/* Set the logging threshold. */

static void
initialize_logging (GKeyFile *key_file)
{
  GHashTable *log_config =
    gzochid_config_keyfile_extract_config (key_file, "log");

  if (g_hash_table_contains (log_config, "priority.threshold")) 
    {
      char *threshold = g_hash_table_lookup (log_config, "priority.threshold");

      if (g_ascii_strncasecmp (threshold, "EMERG", 5) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_ERROR);
      else if (g_ascii_strncasecmp (threshold, "ALERT", 5) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_ERROR);
      else if (g_ascii_strncasecmp (threshold, "CRIT", 4) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_ERROR);
      else if (g_ascii_strncasecmp (threshold, "ERR", 3) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_CRITICAL);
      else if (g_ascii_strncasecmp (threshold, "WARNING", 7) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_WARNING);
      else if (g_ascii_strncasecmp (threshold, "NOTICE", 6) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_MESSAGE);
      else if (g_ascii_strncasecmp (threshold, "INFO", 4) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_INFO);
      else if (g_ascii_strncasecmp (threshold, "DEBUG", 5) == 0)
        gzochid_install_log_handler (G_LOG_LEVEL_DEBUG);
    }
  else gzochid_install_log_handler (G_LOG_LEVEL_INFO);

  g_hash_table_destroy (log_config);
}

/* Configure and start the admin web console if it's enabled. */

static void
initialize_httpd (GzochidHttpServer *http_server,
		  gzochid_event_source *event_source, GKeyFile *key_file)
{
  GHashTable *admin_config =
    gzochid_config_keyfile_extract_config (key_file, "admin");

  if (gzochid_config_to_boolean
      (g_hash_table_lookup (admin_config, "module.httpd.enabled"), FALSE))
    {
      GError *err = NULL;
      int port = gzochid_config_to_int
	(g_hash_table_lookup (admin_config, "module.httpd.port"), 8800);

      gzochid_httpd_meta_register_handlers (http_server, event_source);
      gzochid_http_server_start (http_server, port, &err);

      if (err != NULL)
	{
	  g_critical
	    ("Failed to start admin HTTP server: %s; exiting...", err->message);
	  exit (EXIT_FAILURE);
	}
    }
  
  g_hash_table_destroy (admin_config);
}

int 
main (int argc, char *argv[])
{
  GError *err = NULL;
  
  const char *program_name = argv[0];
  const char *conf_path = NULL;
  int optc = 0;

  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = NULL;
  GzochidResolutionContext *resolution_context = NULL;
  GzochiMetadRootContext *root_context = NULL;
  
  setlocale (LC_ALL, "");

  while ((optc = getopt_long (argc, argv, "c:hv", longopts, NULL)) != -1)
    switch (optc)
      {
      case 'c':
	conf_path = strdup (optarg);
	break;
      case 'v':
	print_version ();
	exit (EXIT_SUCCESS);
	break;
      case 'h':
	print_help (program_name);
	exit (EXIT_SUCCESS);
	break;
      case '?': exit (EXIT_FAILURE);
      }

  if (conf_path == NULL)
    {
      const char *env = getenv ("GZOCHI_METAD_CONF_LOCATION");
      conf_path = env ? env : QUOTE(GZOCHI_METAD_CONF_LOCATION);
    }

  g_message ("Reading configuration from %s", conf_path);
  g_key_file_load_from_file (key_file, conf_path, G_KEY_FILE_NONE, &err);
  
  if (err != NULL)
    {
      /*
	The server has reasonable defaults for everything in gzochi-metad.conf,
	so this condition is not fatal.
      */

      g_warning 
        ("Failed to load server configuration file %s: %s", conf_path,
	 err->message);
      g_clear_error (&err);
    }

  initialize_logging (key_file);
  
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  resolution_context = g_object_new (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
  configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, "path", conf_path, NULL);
  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  
  root_context = gzochid_resolver_require_full
    (resolution_context, GZOCHI_METAD_TYPE_ROOT_CONTEXT, &err);

  if (err != NULL)
    {
      g_error ("Failed to bootstrap gzochi-metad: %s", err->message);
      exit (1);
    }

  gzochid_event_loop_start (root_context->event_loop);
  initialize_httpd
    (root_context->http_server, root_context->event_source, key_file);

  if (root_context->http_server != NULL)
    root_context->admin_server_base_url =
      strdup (gzochid_http_server_get_base_url (root_context->http_server));
  else root_context->admin_server_base_url = strdup ("");

  gzochi_metad_dataserver_start (root_context->data_server);
  
  gzochid_server_socket_listen
    (root_context->socket_server, root_context->server_socket,
     root_context->port);
  
  g_main_loop_run (root_context->socket_server->main_loop);
  
  return 0;
}
