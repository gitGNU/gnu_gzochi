/* game.c: Game context management routines for gzochid
 * Copyright (C) 2015 Julian Graham
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
#include <glib/gstdio.h>
#include <libguile.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "app.h"
#include "auth_int.h"
#include "config.h"
#include "context.h"
#include "descriptor.h"
#include "fsm.h"
#include "game.h"
#include "gzochid-storage.h"
#include "log.h"
#include "scheme.h"
#include "scheme-task.h"
#include "socket.h"
#include "storage.h"
#include "storage-mem.h"
#include "task.h"
#include "threads.h"

#define GAME_DESCRIPTOR_XML "game.xml"

#define DEFAULT_TX_TIMEOUT_MS 100

#define SERVER_FS_APPS_DEFAULT "/var/gzochid/deploy"
#define SERVER_FS_DATA_DEFAULT "/var/gzochid/data"

#ifndef GZOCHID_AUTH_PLUGIN_DIR
#define GZOCHID_AUTH_PLUGIN_DIR "./auth"
#endif /* GZOCHID_AUTH_PLUGIN_DIR */

static void 
initialize_application (gzochid_game_context *context, const char *dir,
			gzochid_application_descriptor *descriptor)
{
  gzochid_application_context *application_context =
    gzochid_application_context_new ();
  
  application_context->deployment_root = strdup (dir);
  application_context->load_paths = g_list_prepend 
    (g_list_copy (descriptor->load_paths), strdup (dir));

  gzochid_application_context_init
    (application_context, (gzochid_context *) context, descriptor);
  g_source_attach ((GSource *) application_context->event_source, 
		   g_main_loop_get_context (context->event_loop));
}

static void 
scan_app_dir (gzochid_game_context *context, const char *dir)
{
  FILE *descriptor_file = NULL;
  char *descriptor_filename = g_strconcat (dir, "/", GAME_DESCRIPTOR_XML, NULL);

  gzochid_application_descriptor *descriptor = NULL;

  if (!g_file_test (descriptor_filename, G_FILE_TEST_IS_REGULAR))
    {
      gzochid_warning 
	("%s does not exist or is not a regular file.", descriptor_filename);
      g_free (descriptor_filename);
      return;
    }

  descriptor_file = fopen (descriptor_filename, "r");
  descriptor = gzochid_config_parse_application_descriptor (descriptor_file);
  fclose (descriptor_file);

  if (g_hash_table_contains (context->applications, descriptor->name))
    gzochid_warning
      ("Application in %s with name '%s' already exists; skipping.", 
       descriptor_file, descriptor->name);
  else initialize_application (context, dir, descriptor);

  g_free (descriptor_filename);
}

static void 
scan_apps_dir (gzochid_game_context *context)
{
  GDir *dir = g_dir_open (context->apps_dir, O_RDONLY, NULL);
  const char *name = g_dir_read_name (dir);
  while (name != NULL)
    {
      char *qname = g_strconcat (context->apps_dir, "/", name, NULL);
      if (g_file_test (qname, G_FILE_TEST_IS_DIR))
	scan_app_dir (context, qname);
      free (qname);

      name = g_dir_read_name (dir);
    }
  g_dir_close (dir);
}

static void 
initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_GAME_STATE_RUNNING);
}

static void 
initialize_auth (int from_state, int to_state, gpointer user_data)
{
  gzochid_auth_init (user_data);
}

static void 
initialize_server (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = user_data;

  context->server = gzochid_socket_server_context_new ();
  gzochid_socket_server_context_init 
    (context->server, (gzochid_context *) context, context->port);
  gzochid_context_until 
    ((gzochid_context *) context->server, GZOCHID_SOCKET_SERVER_STATE_RUNNING);
}

static void 
initialize_apps (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = user_data;
  
  if (!g_file_test (context->work_dir, G_FILE_TEST_EXISTS))
    {
      gzochid_notice 
	("Work directory %s does not exist; creating...", 
	 context->work_dir);
      if (g_mkdir_with_parents (context->work_dir, 493) != 0)
	gzochid_err ("Unable to create work directory %s.", context->work_dir);
    }
  else if (!g_file_test (context->work_dir, G_FILE_TEST_IS_DIR))
    gzochid_err ("%s is not a directory.", context->work_dir);
  
  if (!g_file_test (context->apps_dir, G_FILE_TEST_EXISTS))
    gzochid_err 
      ("Application directory %s does not exist.", context->apps_dir); 
  else if (!g_file_test (context->apps_dir, G_FILE_TEST_IS_DIR))
    gzochid_err ("%s is not a directory.", context->apps_dir); 

  scan_apps_dir (context);
}

static void
initialize_scheme_api (int from_state, int to_state, gpointer user_data)
{
  gzochid_scheme_initialize_bindings ();
  gzochid_scheme_task_initialize_bindings ();
}

static void 
initialize_serialization_registry (int from_state, int to_state, 
				   gpointer user_data)
{
  gzochid_task_initialize_serialization_registry ();
}

static void 
initialize_scheme_task_serialization (int from_state, int to_state,
				      gpointer user_data)
{
  gzochid_task_register_serialization (&gzochid_scheme_task_serialization);
}

static void 
initialize_client_received_message_task_serialization (int from_state, 
						       int to_state, 
						       gpointer user_data)
{
  gzochid_register_client_received_message_task_serialization ();
}

static void 
initialize_complete (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = user_data;
  gzochid_thread_pool_push (context->pool, initialize_async, NULL, NULL);  
}

static gpointer 
event_loop (gpointer data)
{
  gzochid_game_context *context = data;
  g_main_loop_run (context->event_loop);
  return NULL;
}

static void 
initialize_event_loop (int from_state, int to_state, gpointer user_data)
{
  g_thread_new ("event-loop", event_loop, user_data);
}

gzochid_game_context *
gzochid_game_context_new (void)
{
  return calloc (1, sizeof (gzochid_game_context));
}

void 
gzochid_game_context_free (gzochid_game_context *context)
{
  gzochid_context_free ((gzochid_context *) context);

  g_hash_table_destroy (context->applications);
  g_hash_table_destroy (context->auth_plugins);

  free (context);
} 

void 
gzochid_game_context_init (gzochid_game_context *context, 
			   gzochid_context *parent, GHashTable *config)
{
  long tx_timeout_ms = 0;
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("game", GZOCHID_GAME_STATE_INITIALIZING, "INITIALIZING");
  GMainContext *event_context = g_main_context_new ();

  gzochid_fsm_add_state (fsm, GZOCHID_GAME_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_GAME_STATE_STOPPED, "STOPPED");

  context->event_loop = g_main_loop_new (event_context, FALSE);

  context->pool = gzochid_thread_pool_new 
    (context, 
     gzochid_config_to_int 
     (g_hash_table_lookup (config, "thread_pool.max_threads"), 4), 
     TRUE, NULL);
  context->task_queue = gzochid_schedule_task_queue_new (context->pool);

  gzochid_schedule_task_queue_start (context->task_queue);

  context->port = gzochid_config_to_int
    (g_hash_table_lookup (config, "server.port"), 8001);

  if (g_hash_table_contains (config, "server.fs.apps"))
    context->apps_dir = strdup (g_hash_table_lookup (config, "server.fs.apps"));
  else context->apps_dir = strdup (SERVER_FS_APPS_DEFAULT);

  if (g_hash_table_contains (config, "server.fs.data"))
    context->work_dir = strdup (g_hash_table_lookup (config, "server.fs.data"));
  else context->work_dir = strdup (SERVER_FS_DATA_DEFAULT);

  if (g_hash_table_contains (config, "server.auth.plugin.dir"))
    context->auth_plugin_dir = 
      strdup (g_hash_table_lookup (config, "server.auth.plugin.dir"));
  else context->auth_plugin_dir = GZOCHID_AUTH_PLUGIN_DIR;

    context->storage_engine = gzochid_storage_load_engine 
      (g_hash_table_lookup (config, "storage.engine"));
  else gzochid_info 
	 ("No durable storage engine configured; memory engine will be used.");

  if (context->storage_engine == NULL)
    {
      gzochid_info ("\
Using in-memory storage for application data. THIS CONFIGURATION IS NOT SAFE \
FOR PRODUCTION USE.");

      context->storage_engine = calloc (1, sizeof (gzochid_storage_engine));
      context->storage_engine->interface = 
	&gzochid_storage_engine_interface_mem;
    }

  tx_timeout_ms = gzochid_config_to_long 
    (g_hash_table_lookup (config, "tx.timeout"), DEFAULT_TX_TIMEOUT_MS);
  context->tx_timeout.tv_sec = tx_timeout_ms / 1000;
  context->tx_timeout.tv_usec = (tx_timeout_ms % 1000) * 1000;

  gzochid_fsm_add_transition 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, GZOCHID_GAME_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_GAME_STATE_RUNNING, GZOCHID_GAME_STATE_STOPPED);

  gzochid_fsm_on_enter
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_event_loop, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_server, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_serialization_registry, 
     context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_scheme_api, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, 
     initialize_client_received_message_task_serialization, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, 
     initialize_scheme_task_serialization, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_auth, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_apps, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_complete, context);
  
  context->applications = g_hash_table_new (g_str_hash, g_str_equal);
  context->auth_plugins = g_hash_table_new (g_str_hash, g_str_equal);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}

void 
gzochid_game_context_register_application (gzochid_game_context *context, 
					   char *name, 
					   gzochid_application_context *app)
{
  g_hash_table_insert (context->applications, name, app);
}

void 
gzochid_game_context_unregister_application (gzochid_game_context *context, 
					     char *name)
{
  g_hash_table_remove (context->applications, name);
}

gzochid_application_context *
gzochid_game_context_lookup_application (gzochid_game_context *context, 
					 char *name)
{
  return g_hash_table_lookup (context->applications, name);
}

GList *
gzochid_game_context_get_applications (gzochid_game_context *context)
{
  return g_hash_table_get_values (context->applications);
}
