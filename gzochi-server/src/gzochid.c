/* gzochid.c: Main server bootstrapping routines for gzochid
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
#include <locale.h>
#include <libintl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include "admin.h"
#include "config.h"
#include "dataclient.h"
#include "game.h"
#include "guile.h"
#include "gzochid.h"
#include "log.h"
#include "threads.h"

#define _(String) gettext (String)

#define Q(x) #x
#define QUOTE(x) Q(x)

#ifndef GZOCHID_CONF_LOCATION
#define GZOCHID_CONF_LOCATION "/etc/gzochid.conf"
#endif /* GZOCHID_CONF_LOCATION */

G_DEFINE_TYPE (GzochidRootContext, gzochid_root_context, G_TYPE_OBJECT);

enum gzochid_root_context_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_SOCKET_SERVER,
    PROP_RESOLUTION_CONTEXT,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
root_context_set_property (GObject *object, guint property_id,
			   const GValue *value, GParamSpec *pspec)
{
  GzochidRootContext *self = GZOCHID_ROOT_CONTEXT (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_RESOLUTION_CONTEXT:
      self->resolution_context = g_object_ref (g_value_get_object (value));
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_root_context_class_init (GzochidRootContextClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = root_context_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "Configuration", "Set the configuration",
     GZOCHID_TYPE_CONFIGURATION,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "Socket server", "Set the socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_RESOLUTION_CONTEXT] = g_param_spec_object
    ("resolution-context", "Resolution context", "Set the resolution context",
     GZOCHID_TYPE_RESOLUTION_CONTEXT,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_root_context_init (GzochidRootContext *self)
{
  self->admin_context = (gzochid_context *) gzochid_admin_context_new ();
  self->game_server = (gzochid_context *) gzochid_game_context_new ();
}

static const struct option longopts[] =
  {
    { "config", required_argument, NULL, 'c' },
    { "help", no_argument, NULL, 'h' },
    { "version", no_argument, NULL, 'v' },
    { NULL, 0, NULL, 0 }
  };

static void *
initialize_guile_inner (void *ptr)
{
  gzochid_guile_init ();
  return NULL;
}

static void 
initialize_guile ()
{
  scm_with_guile (initialize_guile_inner, NULL);
}

static void
print_help (const char *program_name)
{
  printf (_("\
Usage: %s [OPTION]...\n"), program_name);
  
  puts ("");
  fputs (_("\
  -c, --config        full path to gzochid.conf\n\
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

static void
print_version (void)
{
  printf ("gzochid (gzochi) %s\n", VERSION);

  puts ("");
  printf (_("\
Copyright (C) %s Julian Graham\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"),
	  "2016");
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

static void
root_context_start (GzochidRootContext *root_context)
{
  GHashTable *game_config = gzochid_configuration_extract_group
    (root_context->configuration, "game");
  
  gzochid_game_context_init 
    ((gzochid_game_context *) root_context->game_server, root_context);
  gzochid_context_until (root_context->game_server, GZOCHID_GAME_STATE_RUNNING);

  g_hash_table_unref (game_config);
  
  if (root_context->admin_context != NULL)
    {  
      gzochid_admin_context_init 
	((gzochid_admin_context *) root_context->admin_context, root_context);
      gzochid_context_until
	(root_context->admin_context, GZOCHID_ADMIN_STATE_RUNNING);
    }

  if (root_context->data_client != NULL)
    {
      GError *err = NULL;
      
      gzochid_dataclient_start (root_context->data_client, &err);

      if (err != NULL)
	{
	  g_critical
	    ("Failed to start data client: %s; exiting...", err->message);
	  exit (EXIT_FAILURE);
	}
    }
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
  GzochidRootContext *root_context = NULL;

  GHashTable *metaserver_config = NULL;
  
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
      const char *env = getenv ("GZOCHID_CONF_LOCATION");
      conf_path = env ? env : QUOTE(GZOCHID_CONF_LOCATION);
    }

  g_info ("Reading configuration from %s", conf_path);
  g_key_file_load_from_file (key_file, conf_path, G_KEY_FILE_NONE, &err);

  if (err != NULL)
    {
      /* 
	 The server has reasonable defaults for everything in gzochid.conf, so
	 this condition is not fatal.
      */

      g_warning 
	("Failed to load server configuration file %s: %s", conf_path,
	 err->message);
      g_error_free (err);
    }

  initialize_logging (key_file);

  configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  gzochid_resolver_provide (resolution_context, G_OBJECT (configuration), NULL);
  
  initialize_guile ();
  root_context = gzochid_resolver_require_full
    (resolution_context, GZOCHID_TYPE_ROOT_CONTEXT, &err);

  metaserver_config = gzochid_configuration_extract_group
    (configuration, "metaserver");

  if (gzochid_config_to_boolean
      (g_hash_table_lookup (metaserver_config, "client.enabled"), FALSE))
    root_context->data_client = gzochid_resolver_require_full
      (root_context->resolution_context, GZOCHID_TYPE_DATA_CLIENT, NULL);
    
  if (err != NULL)
    {
      g_error ("Failed to bootstrap gzochid: %s", err->message);
      exit (1);
    }

  root_context->gzochid_conf_path = conf_path;  
  root_context_start (root_context);
  
  g_main_loop_run (root_context->socket_server->main_loop);
  
  return 0;
}
