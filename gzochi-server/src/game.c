/* game.c: Game context management routines for gzochid
 * Copyright (C) 2013 Julian Graham
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
#include <syslog.h>

#include "app.h"
#include "auth_int.h"
#include "config.h"
#include "context.h"
#include "fsm.h"
#include "game.h"
#include "log.h"
#include "scheme.h"
#include "socket.h"
#include "task.h"
#include "threads.h"

#define GAME_DESCRIPTOR_XML "game.xml"

#define DEFAULT_TX_TIMEOUT_MS 100

static void scan_app_dir (gzochid_game_context *context, char *dir)
{
  char *descriptor_file = g_strconcat (dir, "/", GAME_DESCRIPTOR_XML, NULL);

  gzochid_application_descriptor *descriptor = NULL;
  gzochid_application_context *application_context = NULL;

  if (!g_file_test (descriptor_file, G_FILE_TEST_IS_REGULAR))
    {
      gzochid_warning 
	("%s does not exist or is not a regular file.", descriptor_file);
      return;
    }

  application_context = gzochid_application_context_new ();
  
  descriptor = gzochid_config_parse_application_descriptor (descriptor_file);
  gzochid_application_context_init 
    (application_context, (gzochid_context *) context, descriptor);
}

static void scan_apps_dir (gzochid_game_context *context)
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

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_GAME_STATE_RUNNING);
}

static void initialize_auth (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = (gzochid_game_context *) user_data;

  gzochid_auth_init (context);
}

static void initialize_server (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = (gzochid_game_context *) user_data;

  context->server = gzochid_socket_server_context_new ();
  gzochid_socket_server_context_init 
    (context->server, (gzochid_context *) context, context->port);
  gzochid_context_until 
    ((gzochid_context *) context->server, GZOCHID_SOCKET_SERVER_STATE_RUNNING);
}

static void initialize_apps (int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = (gzochid_game_context *) user_data;
  
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

static void initialize_scheme_api
(int from_state, int to_state, gpointer user_data)
{
  gzochid_scheme_initialize_bindings ();
}

static void initialize_serialization_registry
(int from_state, int to_state, gpointer user_data)
{
  gzochid_task_initialize_serialization_registry ();
}

static void initialize_client_received_message_task_serialization
(int from_state, int to_state, gpointer user_data)
{
  gzochid_register_client_received_message_task_serialization ();
}

static void initialize_complete
(int from_state, int to_state, gpointer user_data)
{
  gzochid_game_context *context = (gzochid_game_context *) user_data;
  gzochid_thread_pool_push (context->pool, initialize_async, NULL, NULL);  
}

gzochid_game_context *gzochid_game_context_new (void)
{
  return calloc (1, sizeof (gzochid_game_context));
}

void gzochid_game_context_free (gzochid_game_context *context)
{
  gzochid_context_free ((gzochid_context *) context);

  g_hash_table_destroy (context->applications);
  g_hash_table_destroy (context->auth_plugins);

  free (context);
} 

void gzochid_game_context_init
(gzochid_game_context *context, gzochid_context *parent, GHashTable *config)
{
  long tx_timeout_ms = 0;
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("game", GZOCHID_GAME_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_GAME_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_GAME_STATE_STOPPED, "STOPPED");

  context->pool = gzochid_thread_pool_new 
    (context, 
     gzochid_config_to_int 
     (g_hash_table_lookup (config, "thread_pool.max_threads"), 4), 
     TRUE, NULL);
  context->task_queue = gzochid_schedule_task_queue_new (context->pool);

  gzochid_schedule_task_queue_start (context->task_queue);

  context->port = gzochid_config_to_int
    (g_hash_table_lookup (config, "server.port"), 0);
  context->apps_dir = g_hash_table_lookup (config, "server.fs.apps");
  context->work_dir = g_hash_table_lookup (config, "server.fs.data");

  tx_timeout_ms = gzochid_config_to_long 
    (g_hash_table_lookup (config, "tx.timeout"), DEFAULT_TX_TIMEOUT_MS);
  context->tx_timeout.tv_sec = tx_timeout_ms / 1000;
  context->tx_timeout.tv_usec = (tx_timeout_ms % 1000) * 1000;

  gzochid_fsm_add_transition 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, GZOCHID_GAME_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_GAME_STATE_RUNNING, GZOCHID_GAME_STATE_STOPPED);

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
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_auth, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_apps, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_GAME_STATE_INITIALIZING, initialize_complete, context);
  
  context->applications = g_hash_table_new (g_str_hash, g_str_equal);
  context->auth_plugins = g_hash_table_new (g_str_hash, g_str_equal);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}

void gzochid_game_context_register_application
(gzochid_game_context *context, char *name, gzochid_application_context *app)
{
  g_hash_table_insert (context->applications, name, app);
}

void gzochid_game_context_unregister_application
(gzochid_game_context *context, char *name)
{
  g_hash_table_remove (context->applications, name);
}

gzochid_application_context *gzochid_game_context_lookup_application 
(gzochid_game_context *context, char *name)
{
  return g_hash_table_lookup (context->applications, name);
}

GList *gzochid_game_context_get_applications (gzochid_game_context *context)
{
  return g_hash_table_get_values (context->applications);
}
