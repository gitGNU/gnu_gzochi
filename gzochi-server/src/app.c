/* app.c: Application context routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "app-task.h"
#include "context.h"
#include "event.h"
#include "game.h"
#include "guile.h"
#include "gzochid-auth.h"
#include "oids-dataclient.h"
#include "oids-storage.h"
#include "scheme-task.h"
#include "session.h"
#include "storage-dataclient.h"

static GPrivate thread_application_context_key;
static GPrivate thread_identity_key;

gzochid_application_context *
gzochid_application_context_new (void)
{
  gzochid_application_context *context = calloc 
    (1, sizeof (gzochid_application_context));

  context->oids_to_clients = g_hash_table_new (g_int64_hash, g_int64_equal);
  context->clients_to_oids = g_hash_table_new (g_direct_hash, g_direct_equal);

  g_mutex_init (&context->free_oids_lock);
  g_mutex_init (&context->client_mapping_lock);

  context->channel_oids_to_local_session_oids = g_hash_table_new_full
    (g_int64_hash, g_int64_equal, free, (GDestroyNotify) g_sequence_free);

  g_mutex_init (&context->channel_mapping_lock);
  
  context->event_source = gzochid_event_source_new ();
  context->stats = calloc (1, sizeof (gzochid_application_stats));
  return context;
}

void 
gzochid_application_context_free (gzochid_application_context *app_context)
{
  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_context_free (context);

  g_hash_table_destroy (app_context->oids_to_clients);
  g_hash_table_destroy (app_context->clients_to_oids);
  
  g_list_free (app_context->free_oid_blocks);

  g_mutex_clear (&app_context->free_oids_lock);
  g_mutex_clear (&app_context->client_mapping_lock);

  g_hash_table_destroy (app_context->channel_oids_to_local_session_oids);

  g_mutex_clear (&app_context->channel_mapping_lock);

  g_source_destroy ((GSource *) app_context->event_source);
  g_source_unref ((GSource *) app_context->event_source);
  free (app_context->stats);

  free (context);
}

void *
gzochid_with_application_context (gzochid_application_context *context, 
				  gzochid_auth_identity *identity,
				  void *(*worker) (gpointer), gpointer data)
{
  gpointer ret = NULL;
  gboolean private_needs_context = 
    g_private_get (&thread_application_context_key) == NULL;
  SCM application_root_fluid = 
    scm_variable_ref
    (scm_c_module_lookup 
     (scm_c_resolve_module("gzochi app"), "%gzochi:application-root"));

  if (private_needs_context)
    {
      g_private_set (&thread_application_context_key, context);
      g_private_set (&thread_identity_key, identity);

      assert (context->deployment_root != NULL);
      
      scm_fluid_set_x
	(application_root_fluid, 
	 scm_from_locale_string (context->deployment_root));
    }
  
  ret = worker (data);

  if (private_needs_context)
    {
      g_private_set (&thread_application_context_key, NULL);
      g_private_set (&thread_identity_key, NULL);

      scm_fluid_set_x (application_root_fluid, SCM_UNSPECIFIED);
    }

  return ret;
}

gzochid_application_context *
gzochid_get_current_application_context (void)
{
  return (gzochid_application_context *)
    g_private_get (&thread_application_context_key);
}

gzochid_auth_identity *
gzochid_get_current_identity (void)
{
  return (gzochid_auth_identity *) g_private_get (&thread_identity_key);
}

G_LOCK_DEFINE_STATIC (load_path);

static void 
initialize_auth (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  app_context->identity_cache = gzochid_auth_identity_cache_new ();
  
  if (app_context->descriptor->auth_type != NULL)
    {
      gzochid_auth_plugin *plugin = gzochid_auth_plugin_registry_lookup
	(game_context->auth_plugin_registry,
	 app_context->descriptor->auth_type);

      if (plugin != NULL)
	{
	  GError *error = NULL;

	  g_debug 
	    ("Initializing auth plugin '%s' for application '%s'.",
	     app_context->descriptor->auth_type,
	     app_context->descriptor->name);

	  app_context->auth_data = plugin->info->initialize 
	    (app_context->descriptor->auth_properties, &error);
	  if (error != NULL)
	    g_critical
	      ("Auth plugin '%s' failed to initialize "
	       "for application '%s': %s",
	       app_context->descriptor->auth_type,
	       app_context->descriptor->name,
	       error->message);
	  
	  g_clear_error (&error);
	  app_context->authenticator = plugin->info->authenticate;
	}
      else g_critical 
	     ("Unknown auth type '%s' for application '%s'.", 
	      app_context->descriptor->auth_type,
	      app_context->descriptor->name);
    }
  else
    {
      g_message 
	("No auth plugin specified for application '%s'; " 
	 "pass-thru authentication will be used.", 
	 app_context->descriptor->name);
      app_context->authenticator = gzochid_auth_function_pass_thru;
    }
}

static void 
initialize_data (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  char *data_dir = g_strconcat 
    (game_context->work_dir, "/", app_context->descriptor->name, NULL);
  char *meta_db = g_strconcat (data_dir, "/meta", NULL);
  char *oids_db = g_strconcat (data_dir, "/oids", NULL);
  char *names_db = g_strconcat (data_dir, "/names", NULL);

  gzochid_storage_context *storage_context = NULL;

  if (game_context->storage_engine->handle != NULL)
    {
      if (!g_file_test (data_dir, G_FILE_TEST_EXISTS))
	{
	  g_message 
	    ("Work directory %s does not exist; creating...", data_dir);
	  if (g_mkdir_with_parents (data_dir, 493) != 0)
	    {
	      g_critical ("Unable to create work directory %s.", data_dir);
	      exit (EXIT_FAILURE);
	    }
	}
      else if (!g_file_test (data_dir, G_FILE_TEST_IS_DIR))
	{
	  g_critical ("%s is not a directory.", data_dir);
	  exit (EXIT_FAILURE);
	}
    }
  
  storage_context = app_context->storage_engine_interface
    ->initialize (data_dir);

  if (storage_context == NULL)
    {
      g_critical ("Unable to initialize storage. Exiting.");
      exit (EXIT_FAILURE);
    }

  if (app_context->storage_engine_interface ==
      &gzochid_storage_engine_interface_dataclient)
    {
      GzochidMetaClient *metaclient = NULL;
      GzochidDataClient *dataclient = NULL;

      g_object_get (game_context->root_context->metaclient_container,
		    "metaclient", &metaclient,
		    NULL);
      
      assert (metaclient != NULL);

      /* Grab the metaclient's managed data client to inject it into storage
	 context. */
      
      g_object_get (metaclient, "data-client", &dataclient, NULL);
      
      gzochid_dataclient_storage_context_set_dataclient
	(storage_context, dataclient);
      
      g_object_unref (dataclient);
      g_object_unref (metaclient);
    }
  
  app_context->storage_context = storage_context;
  app_context->meta = app_context->storage_engine_interface->open 
    (storage_context, meta_db, GZOCHID_STORAGE_CREATE);
  app_context->oids = app_context->storage_engine_interface->open 
    (storage_context, oids_db, GZOCHID_STORAGE_CREATE);
  app_context->names = app_context->storage_engine_interface->open 
    (storage_context, names_db, GZOCHID_STORAGE_CREATE);

  /* If we're using the dataclient storage engine, we need to use the dataclient
     oid allocation strategy. */
  
  if (app_context->storage_engine_interface ==
      &gzochid_storage_engine_interface_dataclient)
    {
      GzochidMetaClient *metaclient = NULL;
      GzochidDataClient *dataclient = NULL;

      g_object_get (game_context->root_context->metaclient_container,
		    "metaclient", &metaclient,
		    NULL);

      assert (metaclient != NULL);
      
      /* Grab the metaclient's managed data client to inject into the oid
	 strategy. */
      
      g_object_get (metaclient, "data-client", &dataclient, NULL);

      app_context->oid_strategy = gzochid_dataclient_oid_strategy_new
	(dataclient, app_context->descriptor->name);

      g_object_unref (dataclient);
      g_object_unref (metaclient);
    }
  else app_context->oid_strategy = gzochid_storage_oid_strategy_new
	 (app_context->storage_engine_interface, app_context->storage_context,
	  app_context->meta);

  free (data_dir);
  free (meta_db);
  free (oids_db);
  free (names_db);
}

static void *
initialize_load_paths_guile_worker (void *data)
{
  GList *load_path_ptr = data;

  while (load_path_ptr != NULL)
    {
      gzochid_guile_add_to_load_path (load_path_ptr->data);
      load_path_ptr = load_path_ptr->next;
    }

  return NULL;
}

static void 
initialize_load_paths (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  
  G_LOCK (load_path);
  scm_with_guile (initialize_load_paths_guile_worker, app_context->load_paths);
  G_UNLOCK (load_path);
}

static void 
initialize_complete (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gzochid_game_context_register_application 
    (game_context, app_context->descriptor->name, app_context);

  gzochid_fsm_to_state (context->fsm, GZOCHID_APPLICATION_STATE_RUNNING);
}

static void 
initialize_async_transactional (gpointer data)
{
  GError *err = NULL;
  gzochid_application_context *context = data;
  gboolean initialized = gzochid_data_binding_exists 
    (context, "s.initializer", &err);

  gzochid_auth_identity *system_identity = gzochid_auth_system_identity ();

  assert (err == NULL);

  if (!initialized)
    {
      gzochid_application_task *application_task =
	gzochid_application_task_new
	(context, system_identity,
	 gzochid_scheme_application_initialized_worker,
	 context->descriptor->properties);

      gzochid_task task;

      task.worker = gzochid_application_task_thread_worker;
      task.data = application_task;
      gettimeofday (&task.target_execution_time, NULL);

      gzochid_schedule_execute_task (&task);
    }
  else 
    {
      gzochid_sweep_client_sessions (context, &err);

      if (err != NULL)
	{
	  g_warning
	    ("Failed to sweep sessions for application '%s'; stopping: %s.",
	     context->descriptor->name, err->message);
	  
	  g_error_free (err);
	  
	  gzochid_fsm_to_state
	    (((gzochid_context *) context)->fsm,
	     GZOCHID_APPLICATION_STATE_STOPPED);
	}
      else gzochid_restart_tasks (context);
    }
}

static gboolean
ready_async (gzochid_application_context *context)
{
  GError *tmp_err = NULL;

  if (context->descriptor->ready != NULL)
    gzochid_scheme_application_ready
      (context, gzochid_auth_system_identity (), &tmp_err);
  
  if (tmp_err != NULL)
    {
      g_clear_error (&tmp_err);
      return FALSE;
    }
  else return TRUE;
}

static void *
run_async_guile (void *data)
{
  if (!ready_async (data))
    return NULL;

  gzochid_application_transaction_execute 
    (data, initialize_async_transactional, data);

  return NULL;
}

static void 
run_async (gpointer data, gpointer user_data)
{
  scm_with_guile (run_async_guile, data);
}

static void 
run (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  if (from_state != GZOCHID_APPLICATION_STATE_INITIALIZING)
    return;

  gzochid_thread_pool_push (game_context->pool, run_async, app_context, NULL);
}

static void 
stop (int from_state, int to_state, gpointer user_data)
{
  gzochid_application_context *context = user_data;

  if (context->meta != NULL)
    context->storage_engine_interface->close_store (context->meta);
  if (context->oids != NULL)
    context->storage_engine_interface->close_store (context->oids);
  if (context->names != NULL)
    context->storage_engine_interface->close_store (context->names);  
}

static void 
update_stats (GzochidEvent *event, gpointer data)
{
  gzochid_stats_update_from_event (data, event);
}

void 
gzochid_application_context_init (gzochid_application_context *context, 
				  gzochid_context *parent, 
				  GzochidApplicationDescriptor *descriptor,
				  gzochid_storage_engine_interface *iface)
{
  char *fsm_name = g_strconcat ("app/", descriptor->name, NULL);
  gzochid_fsm *fsm = gzochid_fsm_new 
    (fsm_name, GZOCHID_APPLICATION_STATE_INITIALIZING, "INITIALIZING");

  free (fsm_name);
  
  gzochid_fsm_add_state (fsm, GZOCHID_APPLICATION_STATE_PAUSED, "PAUSED");
  gzochid_fsm_add_state (fsm, GZOCHID_APPLICATION_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_APPLICATION_STATE_STOPPED, "STOPPED");

  gzochid_fsm_add_transition 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, 
     GZOCHID_APPLICATION_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_RUNNING, 
     GZOCHID_APPLICATION_STATE_STOPPED);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_RUNNING, GZOCHID_APPLICATION_STATE_PAUSED);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_PAUSED, GZOCHID_APPLICATION_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_PAUSED, GZOCHID_APPLICATION_STATE_STOPPED);

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_auth, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_data, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_load_paths, 
     context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_complete, 
     context);
  gzochid_fsm_on_enter (fsm, GZOCHID_APPLICATION_STATE_RUNNING, run, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_APPLICATION_STATE_STOPPED, stop, context);

  context->authenticator = gzochid_auth_function_pass_thru;
  context->descriptor = g_object_ref (descriptor);
  context->storage_engine_interface = iface;
  
  gzochid_event_attach (context->event_source, update_stats, context->stats);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}
