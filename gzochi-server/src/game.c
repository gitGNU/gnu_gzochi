/* game.c: Game context management routines for gzochid
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
#include <fcntl.h>
#include <glib.h>
#include <glib-object.h>
#include <glib/gstdio.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "app.h"
#include "auth_int.h"
#include "channel.h"
#include "config.h"
#include "descriptor.h"
#include "durable-task.h"
#include "game.h"
#include "game-protocol.h"
#include "gzochid.h"
#include "gzochid-storage.h"
#include "metaclient.h"
#include "scheme.h"
#include "scheme-task.h"
#include "socket.h"
#include "storage.h"
#include "storage-dataclient.h"
#include "storage-mem.h"
#include "task.h"
#include "threads.h"

#define GAME_DESCRIPTOR_XML "game.xml"

#define DEFAULT_TX_TIMEOUT_MS 100

#define SERVER_FS_APPS_DEFAULT "/var/gzochid/deploy"
#define SERVER_FS_DATA_DEFAULT "/var/gzochid/data"

#ifndef GZOCHID_STORAGE_ENGINE_DIR
#define GZOCHID_STORAGE_ENGINE_DIR "./storage"
#endif /* GZOCHID_STORAGE_ENGINE_DIR */

/* Boilerplate setup for the game server object. */

/* The game server object. */

struct _GzochidGameServer
{
  GObject parent_instance; /* The parent struct, for casting. */
  
  GThreadPool *pool; /* Thread pool for task queue. */

  /* Non-durable queue of tasks pending execution on behalf of running 
     applications. */

  gzochid_task_queue *task_queue; 
  
  int port; /* Port on which the game server listens for connections. */
  char *apps_dir; /* Directory to scan for application deployments. */

  /* Directory to provide to durable storage engine bootstrap. */

  char *work_dir;
  
  struct timeval tx_timeout; /* The default timeout for transactional tasks. */

  /* Map of application name to `gzochid_application_context'. */

  GHashTable *applications; 

  /* The storage engine loaded by the game manager. */

  gzochid_storage_engine *storage_engine; 

  gzochid_server_socket *server_socket; /* The game protocol server socket. */

  /* Components that are injected by the type resolver. */

  GzochidConfiguration *configuration; /* The global configuration object. */
  
  /* The authentication plugin registry. */

  GzochidAuthPluginRegistry *auth_plugin_registry; 

  /* The metaclient container. */

  GzochidMetaClientContainer *metaclient_container;

  GzochidSocketServer *socket_server; /* The game server socket server. */
  GzochidEventLoop *event_loop; /* The global event loop. */
};

G_DEFINE_TYPE (GzochidGameServer, gzochid_game_server, G_TYPE_OBJECT);

enum gzochid_game_server_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_SOCKET_SERVER,
    PROP_EVENT_LOOP,
    PROP_META_CLIENT_CONTAINER,
    PROP_AUTH_PLUGIN_REGISTRY,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
game_server_constructed (GObject *obj)
{
  GzochidGameServer *self = GZOCHID_GAME_SERVER (obj);

  GHashTable *config = gzochid_configuration_extract_group
    (self->configuration, "game");
  int max_threads = gzochid_config_to_int 
    (g_hash_table_lookup (config, "thread_pool.max_threads"), 4);
  long tx_timeout_ms = gzochid_config_to_long 
    (g_hash_table_lookup (config, "tx.timeout"), DEFAULT_TX_TIMEOUT_MS);
       
  self->pool = gzochid_thread_pool_new (self, max_threads, TRUE, NULL);
  self->task_queue = gzochid_schedule_task_queue_new (self->pool);

  self->port = gzochid_config_to_int
    (g_hash_table_lookup (config, "server.port"), 8001);

  if (g_hash_table_contains (config, "server.fs.apps"))
    self->apps_dir = strdup (g_hash_table_lookup (config, "server.fs.apps"));
  else self->apps_dir = strdup (SERVER_FS_APPS_DEFAULT);

  if (g_hash_table_contains (config, "server.fs.data"))
    self->work_dir = strdup (g_hash_table_lookup (config, "server.fs.data"));
  else self->work_dir = strdup (SERVER_FS_DATA_DEFAULT);

  self->tx_timeout.tv_sec = tx_timeout_ms / 1000;
  self->tx_timeout.tv_usec = (tx_timeout_ms % 1000) * 1000;

  g_hash_table_destroy (config);
}

static void
game_server_dispose (GObject *obj)
{
  GzochidGameServer *self = GZOCHID_GAME_SERVER (obj);

  g_object_unref (self->auth_plugin_registry);
  g_object_unref (self->configuration);
  g_object_unref (self->event_loop);
  g_object_unref (self->socket_server);

  G_OBJECT_CLASS (gzochid_game_server_parent_class)->dispose (obj);
}

static void
game_server_finalize (GObject *obj)
{
  GzochidGameServer *self = GZOCHID_GAME_SERVER (obj);

  g_hash_table_destroy (self->applications);

  G_OBJECT_CLASS (gzochid_game_server_parent_class)->finalize (obj);
}

static void
game_server_set_property (GObject *obj, guint property_id, const GValue *value,
			  GParamSpec *pspec)
{
  GzochidGameServer *self = GZOCHID_GAME_SERVER (obj);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    case PROP_EVENT_LOOP:
      self->event_loop = g_object_ref (g_value_get_object (value));
      break;

    case PROP_META_CLIENT_CONTAINER:
      self->metaclient_container = g_object_ref (g_value_get_object (value));
      break;

    case PROP_AUTH_PLUGIN_REGISTRY:
      self->auth_plugin_registry = g_object_ref (g_value_get_object (value));
      break;
    }
}

static void
gzochid_game_server_class_init (GzochidGameServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = game_server_constructed;
  object_class->dispose = game_server_dispose;
  object_class->finalize = game_server_finalize;
  object_class->set_property = game_server_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "socket-server", "The client-facing socket server",
     GZOCHID_TYPE_SOCKET_SERVER, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_EVENT_LOOP] = g_param_spec_object
    ("event-loop", "event-loop", "The global event loop",
     GZOCHID_TYPE_EVENT_LOOP, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_META_CLIENT_CONTAINER] = g_param_spec_object
    ("metaclient-container", "metaclient-container", "The metaclient container",
     GZOCHID_TYPE_META_CLIENT_CONTAINER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  obj_properties[PROP_AUTH_PLUGIN_REGISTRY] = g_param_spec_object
    ("auth-plugin-registry", "plugin-registry",
     "The authentication plugin registry", GZOCHID_TYPE_AUTH_PLUGIN_REGISTRY,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

void
gzochid_game_server_init (GzochidGameServer *self)
{
  self->applications = g_hash_table_new (g_str_hash, g_str_equal);
}

/* Bootstrap the storage engine to be used by all hosted applications. If the
   gzochid application server is running in distributed mode, the "dataclient"
   storage engine is used; otherwise the engine named in the `gzochid.conf' file
   is loaded. If the requested storage engine cannot be loaded, the server will
   fall back to a non-durable "in-memory" storage engine. */

static void
initialize_storage (GzochidGameServer *server)
{
  GHashTable *config = gzochid_configuration_extract_group
    (server->configuration, "game");

  GzochidMetaClient *metaclient = NULL;
  
  g_object_get (server->metaclient_container, "metaclient", &metaclient, NULL);

  if (metaclient != NULL)
    {
      char *conf_storage_engine = g_hash_table_lookup
	(config, "storage.engine");

      if (conf_storage_engine != NULL)
	g_message
	  ("Meta server client configuration detected; ignoring configured "
	   "storage engine '%s'.", conf_storage_engine);
      else g_message
	     ("Meta server client configuration detected; using data client "
	      "storage engine.");

      server->storage_engine = calloc (1, sizeof (gzochid_storage_engine));
      server->storage_engine->interface =
	&gzochid_storage_engine_interface_dataclient;

      g_object_unref (metaclient);
    }
  else if (g_hash_table_contains (config, "storage.engine"))
    {
      char *dir = NULL;
      char *env = getenv ("GZOCHID_STORAGE_ENGINE_DIR");
      
      if (env != NULL)
	dir = env;
      else 
	{
	  char *conf_dir = g_hash_table_lookup (config, "storage.engine.dir");
	  dir = conf_dir == NULL ? GZOCHID_STORAGE_ENGINE_DIR : conf_dir;
	}

      server->storage_engine = gzochid_storage_load_engine 
	(dir, g_hash_table_lookup (config, "storage.engine"));
    }
  else g_message 
	 ("No durable storage engine configured; memory engine will be used.");

  if (server->storage_engine == NULL)
    {
      g_message
	("Using in-memory storage for application data. THIS CONFIGURATION IS "
	 "NOT SAFE FOR PRODUCTION USE.");

      server->storage_engine = calloc (1, sizeof (gzochid_storage_engine));
      server->storage_engine->interface = &gzochid_storage_engine_interface_mem;
    }

  g_hash_table_destroy (config);
}

/* Adds a few core `gzochid_application_task_serialization' objects to the 
   global task serialization registry. */

static void 
initialize_application_task_serializations ()
{
  gzochid_task_initialize_serialization_registry ();

  gzochid_task_register_serialization (&gzochid_scheme_task_serialization);
  gzochid_task_register_serialization
    (&gzochid_client_received_message_task_serialization);
  gzochid_task_register_serialization
    (&gzochid_channel_operation_task_serialization);
  gzochid_task_register_serialization
    (&gzochid_task_chain_bootstrap_task_serialization);
}

/* Initialize the application corresponding to the specified 
   `GzochidApplicationDescriptor'. */

static void 
initialize_application (GzochidGameServer *server, const char *dir,
			GzochidApplicationDescriptor *descriptor)
{
  gzochid_application_context *application_context =
    gzochid_application_context_new ();
  
  application_context->deployment_root = strdup (dir);
  application_context->load_paths = g_list_prepend 
    (g_list_copy (descriptor->load_paths), strdup (dir));
  
  gzochid_application_context_init
    (application_context, descriptor, server->metaclient_container,
     server->auth_plugin_registry, server->storage_engine->interface,
     server->work_dir, server->task_queue, server->tx_timeout);

  g_hash_table_insert
    (server->applications, descriptor->name, application_context);

  gzochid_event_source_attach
    (server->event_loop, application_context->event_source);
}

/* Attempt to bootstrap an application from the specified directory by searching
   it for a game descriptor file. If one is found, load it for use with 
   `initialize_application'. */

static void 
scan_app_dir (GzochidGameServer *server, const char *dir)
{
  FILE *descriptor_file = NULL;
  char *descriptor_filename = g_strconcat (dir, "/", GAME_DESCRIPTOR_XML, NULL);

  GzochidApplicationDescriptor *descriptor = NULL;

  if (!g_file_test (descriptor_filename, G_FILE_TEST_IS_REGULAR))
    {
      g_warning 
	("%s does not exist or is not a regular file.", descriptor_filename);
      g_free (descriptor_filename);
      return;
    }

  descriptor_file = fopen (descriptor_filename, "r");
  descriptor = gzochid_config_parse_application_descriptor (descriptor_file);
  fclose (descriptor_file);

  if (descriptor == NULL)
    g_warning
      ("Failed to parse application descriptor %s; skipping.",
       descriptor_filename);
  else if (g_hash_table_contains (server->applications, descriptor->name))
    g_warning
      ("Application in %s with name '%s' already exists; skipping.", 
       descriptor_filename, descriptor->name);
  else initialize_application (server, dir, descriptor);

  g_free (descriptor_filename);
}

/*
  Returns the absolute path to the application deployment directory. If the
  directory name specified in `gzochid.conf' is already absolute, return it;
  else return the relative path qualified by the directory containing 
  `gzochid.conf' itself.

  The returned string should be freed via `g_free' when no longer needed.
 */

static gchar *
qualify_apps_dir (GzochidGameServer *server)
{
  if (g_path_is_absolute (server->apps_dir))
    return g_strdup (server->apps_dir);
  else
    {
      gchar *basedir = NULL, *conf_path = NULL, *path = NULL;

      g_object_get (server->configuration, "path", &conf_path, NULL);

      basedir = g_path_get_dirname (conf_path);
      path = g_strconcat (basedir, "/", server->apps_dir, NULL);;
      
      g_free (basedir);
      g_free (conf_path);
      
      return path;
    }
}

/* Searches the application directory for folders containing game application 
   descriptor files, bootstrapping any discovered applications. */

static void 
scan_apps_dir (GzochidGameServer *server)
{
  char *apps_dir = qualify_apps_dir (server);
  GDir *dir = g_dir_open (apps_dir, O_RDONLY, NULL);
  const char *name = g_dir_read_name (dir);

  while (name != NULL)
    {
      char *qname = g_strconcat (apps_dir, "/", name, NULL);
      if (g_file_test (qname, G_FILE_TEST_IS_DIR))
	scan_app_dir (server, qname);
      free (qname);

      name = g_dir_read_name (dir);
    }
  
  g_dir_close (dir);
  g_free (apps_dir);
}

/* Bootstrap all applications that can be found as subfolders of the configured
   application deployment directory. */

static void 
initialize_apps (GzochidGameServer *server)
{
  if (!g_file_test (server->work_dir, G_FILE_TEST_EXISTS))
    {
      g_message 
	("Work directory %s does not exist; creating...", server->work_dir);
      if (g_mkdir_with_parents (server->work_dir, 493) != 0)
	{
	  g_critical ("Unable to create work directory %s.", server->work_dir);
	  exit (EXIT_FAILURE);
	}
    }
  else if (!g_file_test (server->work_dir, G_FILE_TEST_IS_DIR))
    {
      g_critical ("%s is not a directory.", server->work_dir);
      exit (EXIT_FAILURE);
    }
  
  if (!g_file_test (server->apps_dir, G_FILE_TEST_EXISTS))
    {
      g_critical ("Application directory %s does not exist.", server->apps_dir);
      exit (EXIT_FAILURE);
    }
  else if (!g_file_test (server->apps_dir, G_FILE_TEST_IS_DIR))
    {
      g_critical ("%s is not a directory.", server->apps_dir);
      exit (EXIT_FAILURE);
    }

  scan_apps_dir (server);
}

void
gzochid_game_server_start (GzochidGameServer *server, GError **err)
{
  gzochid_scheme_initialize_bindings ();
  gzochid_scheme_task_initialize_bindings ();

  initialize_application_task_serializations ();

  gzochid_schedule_task_queue_start (server->task_queue);

  initialize_storage (server);
  initialize_apps (server);
  
  server->server_socket = gzochid_server_socket_new
    ("Game server", gzochid_game_server_protocol,
     gzochid_game_protocol_create_closure
     (server, server->task_queue, server->tx_timeout));

  gzochid_server_socket_listen
    (server->socket_server, server->server_socket, server->port);
}

gzochid_application_context *
gzochid_game_server_lookup_application (GzochidGameServer *server,
					const char *name)
{
  return g_hash_table_lookup (server->applications, name);
}

GList *
gzochid_game_server_get_applications (GzochidGameServer *server)
{
  return g_hash_table_get_values (server->applications);
}
