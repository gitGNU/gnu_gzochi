/* gzochi-metad.c: Main server bootstrapping routines for gzochi-metad
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

#include "config.h"
#include "dataserver.h"
#include "httpd.h"
#include "httpd-meta.h"
#include "log.h"
#include "resolver.h"
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
  GObject parent_intance;

  GzochidHttpServer *http_server; /* The admin web console. */

  /* The meta server global socket server. */

  GzochidSocketServer *socket_server;
  
  GzochiMetadDataServer *data_server; /* The meta server data server. */
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
    PROP_HTTP_SERVER = 1,
    PROP_SOCKET_SERVER,
    PROP_DATA_SERVER,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
root_context_set_property (GObject *object, guint property_id,
			   const GValue *value, GParamSpec *pspec)
{
  GzochiMetadRootContext *self = GZOCHI_METAD_ROOT_CONTEXT (object);

  switch (property_id)
    {
    case PROP_HTTP_SERVER:
      self->http_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_DATA_SERVER:
      self->data_server = g_object_ref (g_value_get_object (value));
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

  object_class->set_property = root_context_set_property;
  obj_properties[PROP_HTTP_SERVER] = g_param_spec_object
    ("http-server", "Admin HTTP server", "Set the admin HTTP server",
     GZOCHID_TYPE_HTTP_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "Socket server", "Set the socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_DATA_SERVER] = g_param_spec_object
    ("data-server", "Data server", "Set the data server",
     GZOCHI_METAD_TYPE_DATA_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_root_context_init (GzochiMetadRootContext *self)
{
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
	  "2016");
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
initialize_httpd (GzochidHttpServer *http_server, GKeyFile *key_file)
{
  GHashTable *admin_config =
    gzochid_config_keyfile_extract_config (key_file, "admin");

  if (gzochid_config_to_boolean
      (g_hash_table_lookup (admin_config, "module.httpd.enabled"), FALSE))
    {
      int port = gzochid_config_to_int
	(g_hash_table_lookup (admin_config, "module.httpd.port"), 8800);

      gzochid_httpd_meta_register_handlers (http_server);
      gzochid_http_server_start (http_server, port, NULL);
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
  GzochidResolutionContext *resolution_context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
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

  g_info ("Reading configuration from %s", conf_path);
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
  
  configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  
  root_context = gzochid_resolver_require_full
    (resolution_context, GZOCHI_METAD_TYPE_ROOT_CONTEXT, &err);

  if (err != NULL)
    {
      g_error ("Failed to bootstrap gzochi-metad: %s", err->message);
      exit (1);
    }

  initialize_httpd (root_context->http_server, key_file);
  gzochi_metad_dataserver_start (root_context->data_server);
  g_main_loop_run (root_context->socket_server->main_loop);
  
  return 0;
}
