/* app.c: Application context routines for gzochid
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

#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "app.h"
#include "auth.h"
#include "auth_int.h"
#include "context.h"
#include "event.h"
#include "fsm.h"
#include "game.h"
#include "guile.h"
#include "log.h"
#include "protocol.h"
#include "scheme.h"
#include "session.h"
#include "storage.h"
#include "task.h"
#include "threads.h"
#include "tx.h"
#include "util.h"

#define GZOCHID_APPLICATION_MAX_ATTEMPTS_DEFAULT 3

static GStaticPrivate thread_application_context_key = G_STATIC_PRIVATE_INIT;
static GStaticPrivate thread_identity_key = G_STATIC_PRIVATE_INIT;

G_LOCK_DEFINE_STATIC (load_path);

typedef struct _gzochid_event_transaction_context
{
  gzochid_application_context *app_context;
  struct timeval start_time;
} gzochid_event_transaction_context;

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_APPLICATION_STATE_RUNNING);
}

static void initialize_auth (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  if (app_context->descriptor->auth_type != NULL)
    {
      if (g_hash_table_contains 
	  (game_context->auth_plugins, app_context->descriptor->auth_type))
	{
	  GError *error = NULL;
	  gzochid_auth_plugin *plugin = g_hash_table_lookup 
	    (game_context->auth_plugins, app_context->descriptor->auth_type);

	  gzochid_debug 
	    ("Initializing auth plugin '%s' for application '%'.",
	     app_context->descriptor->auth_type,
	     app_context->descriptor->name);

	  app_context->auth_data = plugin->info->initialize 
	    (app_context->descriptor->auth_properties, &error);
	  if (error != NULL)
	    gzochid_err
	      ("Auth plugin '%s' failed to initialize "
	       "for application '%s': %s",
	       app_context->descriptor->auth_type,
	       app_context->descriptor->name,
	       error->message);
	  
	  g_clear_error (&error);
	  app_context->authenticator = plugin->info->authenticate;
	}
      else gzochid_err 
	     ("Unknown auth type '%s' for application '%s'.", 
	      app_context->descriptor->auth_type,
	      app_context->descriptor->name);
    }
  else 
    {
      gzochid_info 
	("No auth plugin specified for application '%s'; " 
	 "pass-thru authentication will be used.", 
	 app_context->descriptor->name);
      app_context->authenticator = gzochid_auth_function_pass_thru;
    }
}

static void initialize_data (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  char *data_dir = g_strconcat 
    (game_context->work_dir, "/", app_context->descriptor->name, NULL);
  char *meta_db = g_strconcat (data_dir, "/meta", NULL);
  char *oids_db = g_strconcat (data_dir, "/oids", NULL);
  char *names_db = g_strconcat (data_dir, "/names", NULL);

  gzochid_storage_context *storage_context = NULL;

  if (!g_file_test (data_dir, G_FILE_TEST_EXISTS))
    {
      gzochid_notice 
	("Work directory %s does not exist; creating...", data_dir);
      if (g_mkdir_with_parents (data_dir, 493) != 0)
	gzochid_err ("Unable to create work directory %s.", data_dir);
    }
  else if (!g_file_test (data_dir, G_FILE_TEST_IS_DIR))
    gzochid_err ("%s is not a directory.", data_dir);

  storage_context = gzochid_storage_initialize (data_dir);

  app_context->storage_context = storage_context;
  app_context->meta = gzochid_storage_open (storage_context, meta_db);
  app_context->oids = gzochid_storage_open (storage_context, oids_db);
  app_context->names = gzochid_storage_open (storage_context, names_db);

  free (data_dir);
  free (meta_db);
  free (oids_db);
  free (names_db);
}

static void initialize_load_paths_guile_worker
(gpointer data, gpointer user_data)
{
  GList *load_path_ptr = (GList *) data;

  while (load_path_ptr != NULL)
    {
      gzochid_guile_add_to_load_path ((char *) load_path_ptr->data);
      load_path_ptr = load_path_ptr->next;
    }
}

static void initialize_load_paths
(int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  
  G_LOCK (load_path);
  gzochid_guile_run 
    (initialize_load_paths_guile_worker, app_context->descriptor->load_paths);
  G_UNLOCK (load_path);
}

static void initialize_complete 
(int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gzochid_game_context_register_application 
    (game_context, app_context->descriptor->name, app_context);

  gzochid_guile_thread_pool_push 
    (game_context->pool, initialize_async, user_data, NULL);
}

gzochid_application_task *gzochid_deserialize_application_task 
(gzochid_application_context *context, 
 gzochid_application_task_serialization *serialization, GString *in)
{
  gzochid_auth_identity *identity = 
    gzochid_auth_identity_deserializer (context, in);
  gzochid_application_worker worker = 
    serialization->worker_serialization->deserializer (context, in);
  gpointer data = serialization->data_serialization->deserializer (context, in);
  return gzochid_application_task_new (context, identity, worker, data);
}

void gzochid_serialize_application_task
(gzochid_application_context *context,
 gzochid_application_task_serialization *serialization, 
 gzochid_application_task *task, GString *out)
{
  gzochid_auth_identity_serializer (context, task->identity, out);
  serialization->worker_serialization->serializer (context, task->worker, out);
  serialization->data_serialization->serializer (context, task->data, out);
}

static gzochid_event_transaction_context *create_transaction_context 
(gzochid_application_context *app_context)
{
  gzochid_event_transaction_context *tx_context = 
    malloc (sizeof (gzochid_event_transaction_context));

  tx_context->app_context = app_context;
  gettimeofday (&tx_context->start_time, NULL);

  return tx_context;
}

static int event_prepare (gpointer data)
{
  return TRUE;
}

static void event_commit (gpointer data)
{
  gzochid_event_transaction_context *tx_context = 
    (gzochid_event_transaction_context *) data;
  gzochid_application_transaction_event *event = 
    malloc (sizeof (gzochid_application_transaction_event));
  gzochid_application_event *base_event = (gzochid_application_event *) event;
  struct timeval now;

  gettimeofday (&now, NULL);
  base_event->type = TRANSACTION_COMMIT;
  gettimeofday (&base_event->timestamp, NULL);
  timersub (&now, &tx_context->start_time, &event->duration);

  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source, base_event);
  free (tx_context);
}

static void event_rollback (gpointer data)
{
  gzochid_event_transaction_context *tx_context = 
    (gzochid_event_transaction_context *) data;

  gzochid_application_transaction_event *event = 
    malloc (sizeof (gzochid_application_transaction_event));
  gzochid_application_event *base_event = (gzochid_application_event *) event;
  struct timeval now;

  gettimeofday (&now, NULL);
  base_event->type = TRANSACTION_ROLLBACK;
  gettimeofday (&base_event->timestamp, NULL);
  timersub (&now, &tx_context->start_time, &event->duration);

  gzochid_application_event_dispatch 
    (tx_context->app_context->event_source, base_event);
  free (tx_context);
}

static gzochid_transaction_participant event_participant = 
  { "event", event_prepare, event_commit, event_rollback };

static void join_transaction (gzochid_application_context *context)
{
  if (!gzochid_transaction_active ()
      || gzochid_transaction_context (&event_participant) == NULL)
    {
      gzochid_event_transaction_context *tx_context =
	create_transaction_context (context); 
      gzochid_transaction_join (&event_participant, tx_context);
    }
}

static void event_func_wrapper (gpointer data)
{
  gpointer *args = (gpointer *) data;
  gzochid_application_context *context = 
    (gzochid_application_context *) args[0];
  void (*func) (gpointer) = (void (*) (gpointer)) args[1];
  gpointer func_data = args[2];

  gzochid_application_event *event = 
    malloc (sizeof (gzochid_application_event));

  event->type = TRANSACTION_START;
  gettimeofday (&event->timestamp, NULL);

  join_transaction (context);

  gzochid_application_event_dispatch (context->event_source, event);
  func (func_data);  
}

gzochid_transaction_result gzochid_application_transaction_execute 
(gzochid_application_context *context, void (*func) (gpointer), gpointer data)
{
  gpointer args[3];

  args[0] = context;
  args[1] = func;
  args[2] = data;

  return gzochid_transaction_execute (event_func_wrapper, args);
}

gzochid_transaction_result gzochid_application_transaction_execute_timed 
(gzochid_application_context *context, void (*func) (gpointer), gpointer data,
 struct timeval timeout)
{
  gpointer args[3];

  args[0] = context;
  args[1] = func;
  args[2] = data;

  return gzochid_transaction_execute_timed (event_func_wrapper, args, timeout);
}

static gzochid_transactional_application_task_execution *execution_new
(gzochid_application_task *task, gzochid_application_task *cleanup_task,
 struct timeval *timeout)
{
  gzochid_transactional_application_task_execution *execution = 
    malloc (sizeof (gzochid_transactional_application_task_execution));

  execution->task = task;
  execution->cleanup_task = cleanup_task;

  if (timeout != NULL)
    {
      execution->timeout = malloc (sizeof (struct timeval));
      execution->timeout->tv_sec = timeout->tv_sec;
      execution->timeout->tv_usec = timeout->tv_usec;
    }
  else execution->timeout = NULL;

  execution->attempts = 0;
  execution->result = GZOCHID_TRANSACTION_PENDING;

  return execution;
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_execution_new 
(gzochid_application_task *task, gzochid_application_task *cleanup_task)
{
  return execution_new (task, NULL, NULL);
}

gzochid_transactional_application_task_execution *
gzochid_transactional_application_task_timed_execution_new 
(gzochid_application_task *task, gzochid_application_task *cleanup_task,
 struct timeval timeout)
{
  return execution_new (task, cleanup_task, &timeout);
}

void gzochid_transactional_application_task_execution_free
(gzochid_transactional_application_task_execution *execution)
{
  if (execution->timeout != NULL)
    free (execution->timeout);
  free (execution);
}

static void transactional_task_worker (gpointer data)
{
  void **args = (void **) data;
  
  gzochid_application_context *context = 
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];
  gzochid_application_task *task = (gzochid_application_task *) args[2];
  
  task->worker (context, identity, task->data);
}

void gzochid_application_transactional_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  void *args[3];
  gzochid_transactional_application_task_execution *execution = 
    (gzochid_transactional_application_task_execution *) data;

  args[0] = context;
  args[1] = identity;
  args[2] = execution->task;

  if (execution->timeout != NULL)
    execution->result = gzochid_application_transaction_execute_timed 
      (context, transactional_task_worker, args, *execution->timeout);
  else execution->result = gzochid_application_transaction_execute 
	 (context, transactional_task_worker, args);

  execution->attempts++;
}

void gzochid_application_resubmitting_transactional_task_worker 
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_transactional_application_task_execution *execution = 
    (gzochid_transactional_application_task_execution *) data;

  gzochid_application_transactional_task_worker 
    (app_context, identity, execution);
  
  if (execution->result != GZOCHID_TRANSACTION_SUCCESS)
    {
      struct timeval now;
      gzochid_context *context = (gzochid_context *) app_context;
      gzochid_game_context *game_context = 
	(gzochid_game_context *) context->parent;
      gzochid_application_task *application_task = NULL;
      gzochid_task *task = NULL;

      if (gzochid_application_should_retry (execution))
	{	  	  
	  execution->result = GZOCHID_TRANSACTION_PENDING;
	  
	  application_task = gzochid_application_task_new
	    (app_context, identity,
	     gzochid_application_resubmitting_transactional_task_worker, 
	     execution);	  
	}
      else application_task = execution->cleanup_task;

      if (application_task != NULL)
	{
	  gettimeofday (&now, NULL);
	  
	  task = gzochid_task_new
	    (gzochid_application_task_thread_worker, application_task, now);
	  
	  gzochid_schedule_submit_task (game_context->task_queue, task);
	}
    }
}

void gzochid_application_task_worker (gpointer data)
{
  gzochid_application_task *task = (gzochid_application_task *) data;
  task->worker (task->context, task->identity, task->data);
}

void gzochid_application_task_thread_worker (gpointer data, gpointer user_data)
{
  gzochid_application_task_worker (data);
}

gboolean gzochid_application_should_retry 
(gzochid_transactional_application_task_execution *execution)
{
  return execution->result == GZOCHID_TRANSACTION_SHOULD_RETRY
    && execution->attempts < GZOCHID_APPLICATION_MAX_ATTEMPTS_DEFAULT;
}

static void run_async_transactional (gpointer data)
{
  gzochid_application_context *context = (gzochid_application_context *) data;
  SCM persisted_callback = (SCM) gzochid_data_get_binding 
    (context, "s.initializer", &gzochid_scheme_data_serialization, NULL);

  gzochid_auth_identity *system_identity = calloc 
    (1, sizeof (gzochid_auth_identity));
  system_identity->name = "[SYSTEM]";

  if (persisted_callback == NULL)
    {
      gzochid_application_task application_task;
      gzochid_task task;

      application_task.worker = gzochid_scheme_application_initialized_worker;
      application_task.context = context;
      application_task.identity = system_identity;
      application_task.data = context->descriptor->properties;

      task.worker = gzochid_application_task_thread_worker;
      task.data = &application_task;
      gettimeofday (&task.target_execution_time, NULL);

      gzochid_schedule_execute_task (&task);
    }
  else 
    {
      gzochid_sweep_client_sessions (context);
      gzochid_restart_tasks (context);
    }
}

static void run_async (gpointer data, gpointer user_data)
{
  gzochid_application_transaction_execute 
    ((gzochid_application_context *) data, run_async_transactional, data);
}

static void run (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  if (from_state != GZOCHID_APPLICATION_STATE_INITIALIZING)
    return;

  gzochid_guile_thread_pool_push 
    (game_context->pool, run_async, app_context, NULL);
}

static void stop (int from_state, int to_state, gpointer user_data)
{
  gzochid_application_context *context = 
    (gzochid_application_context *) user_data;

  if (context->meta != NULL)
    gzochid_storage_close (context->meta);
  if (context->oids != NULL)
    gzochid_storage_close (context->oids);
  if (context->names != NULL)
    gzochid_storage_close (context->names);  
}

static void serialize_callback 
(gzochid_application_context *context, gpointer data, GString *out)
{
  gzochid_application_callback *callback = 
    (gzochid_application_callback *) data;

  gzochid_util_serialize_string (callback->procedure, out);
  gzochid_util_serialize_list 
    (callback->module, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);
  gzochid_util_serialize_mpz (callback->scm_oid, out);
}

static gpointer deserialize_callback
(gzochid_application_context *context, GString *in)
{
  gzochid_application_callback *callback = 
    malloc (sizeof (gzochid_application_callback));

  callback->procedure = gzochid_util_deserialize_string (in);
  callback->module = gzochid_util_deserialize_list 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string);

  mpz_init (callback->scm_oid);
  gzochid_util_deserialize_mpz (in, callback->scm_oid);
  
  return callback;
}

static void finalize_callback 
(gzochid_application_context *context, gpointer data)
{
  gzochid_application_callback *callback = 
    (gzochid_application_callback *) data;

  free (callback->procedure);
  g_list_free_full (callback->module, free);
  mpz_clear (callback->scm_oid);

  free (callback);
}

gzochid_io_serialization gzochid_application_callback_serialization = 
  { serialize_callback, deserialize_callback, finalize_callback };

gzochid_application_callback *gzochid_application_callback_new
(char *procedure, GList *module, mpz_t scm_oid)
{
  gzochid_application_callback *callback = calloc
    (1, sizeof (gzochid_application_callback));

  callback->module = module;
  callback->procedure = procedure;

  mpz_init (callback->scm_oid);
  mpz_set (callback->scm_oid, scm_oid);

  return callback;
}

void gzochid_application_callback_free (gzochid_application_callback *callback)
{
  free (callback);
}

gzochid_application_context *gzochid_application_context_new (void)
{
  gzochid_application_context *context = calloc 
    (1, sizeof (gzochid_application_context));

  context->oids_to_clients = g_hash_table_new (g_str_hash, g_str_equal);
  context->clients_to_oids = g_hash_table_new (g_direct_hash, g_direct_equal);

  context->free_oids_lock = g_mutex_new ();
  context->client_mapping_lock = g_mutex_new ();

  context->event_source = gzochid_application_event_source_new ();
  context->stats = calloc (1, sizeof (gzochid_application_stats));
  return context;
}

void gzochid_application_context_free (gzochid_application_context *app_context)
{
  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_context_free (context);

  g_mutex_free (app_context->free_oids_lock);
  g_list_free (app_context->free_oid_blocks);

  gzochid_application_event_source_free (app_context->event_source);
  free (app_context->stats);

  free (context->fsm->name);
  free (context);
}

static void update_stats (gzochid_application_event *event, gpointer data)
{
  gzochid_stats_update_from_event ((gzochid_application_stats *) data, event);
}

void gzochid_application_context_init 
(gzochid_application_context *context, gzochid_context *parent, 
 gzochid_application_descriptor *descriptor)
{
  char *fsm_name = g_strconcat ("app/", descriptor->name, NULL);
  gzochid_fsm *fsm = gzochid_fsm_new 
    (fsm_name, GZOCHID_APPLICATION_STATE_INITIALIZING, "INITIALIZING");

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
  context->descriptor = descriptor;
  
  gzochid_application_event_attach
    (context->event_source, update_stats, context->stats);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}

void gzochid_application_client_logged_in
(gzochid_application_context *context, gzochid_protocol_client *client)
{
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;
  gzochid_client_session *session = 
    gzochid_client_session_new (client->identity);

  gzochid_application_task transactional_task;
  gzochid_application_task application_task;
  gzochid_transactional_application_task_execution *execution =
    gzochid_transactional_application_task_timed_execution_new 
    (&transactional_task, NULL, game_context->tx_timeout);

  gzochid_task task;

  char *session_oid_str = NULL;
  mpz_t session_oid;
  
  mpz_init (session_oid);
  gzochid_client_session_persist (context, session, session_oid);
  session_oid_str = mpz_get_str (NULL, 16, session_oid);

  g_mutex_lock (context->client_mapping_lock);
  g_hash_table_insert (context->oids_to_clients, session_oid_str, client);
  g_hash_table_insert (context->clients_to_oids, client, session_oid_str);
  g_mutex_unlock (context->client_mapping_lock);

  transactional_task.worker = gzochid_scheme_application_logged_in_worker;
  transactional_task.context = context;
  transactional_task.identity = client->identity;
  transactional_task.data = session_oid_str;

  application_task.worker = gzochid_application_transactional_task_worker;
  application_task.context = context;
  application_task.identity = client->identity;
  application_task.data = execution;

  task.worker = gzochid_application_task_thread_worker;
  task.data = &application_task;
  gettimeofday (&task.target_execution_time, NULL);

  while (TRUE)
    {
      gzochid_schedule_run_task (game_context->task_queue, &task);
      if (!gzochid_application_should_retry (execution))
	{
	  if (execution->result != GZOCHID_TRANSACTION_SUCCESS)	
	    {
	      gzochid_info
		("Disconnecting session '%s'; failed login transaction.", 
		 session_oid_str);

	      g_mutex_lock (context->client_mapping_lock);
	      g_hash_table_remove (context->oids_to_clients, session_oid_str);
	      g_hash_table_remove (context->clients_to_oids, client);
	      g_mutex_unlock (context->client_mapping_lock);

	      gzochid_protocol_client_disconnect (client);
	    }
	    break;
	}
    }

  gzochid_transactional_application_task_execution_free (execution);
}

void gzochid_application_client_disconnected
(gzochid_application_context *context, gzochid_protocol_client *client)
{
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;
  char *session_oid_str = NULL;

  g_mutex_lock (context->client_mapping_lock);
  session_oid_str = g_hash_table_lookup (context->clients_to_oids, client);
  if (session_oid_str == NULL)
    {
      g_mutex_unlock (context->client_mapping_lock);
      return;
    }
  else 
    {
      gzochid_task *callback_task = NULL;
      gzochid_task *cleanup_task = NULL;

      struct timeval n;

      gettimeofday (&n, NULL);
      
      callback_task = gzochid_task_make_transactional_application_task
	(context, client->identity, 
	 gzochid_scheme_application_disconnected_worker, session_oid_str, n);
      cleanup_task = gzochid_task_make_transactional_application_task
	(context, client->identity, gzochid_client_session_disconnected_worker,
	 session_oid_str, n);

      g_hash_table_remove (context->clients_to_oids, client);
      g_hash_table_remove (context->oids_to_clients, session_oid_str);
      
      gzochid_schedule_submit_task (game_context->task_queue, callback_task);
      gzochid_schedule_submit_task (game_context->task_queue, cleanup_task);

      g_mutex_unlock (context->client_mapping_lock);
    }
}

gzochid_application_worker_serialization 
received_message_worker_serialization = { NULL, NULL };

gzochid_io_serialization received_message_data_serialization = 
  { NULL, NULL, NULL };

gzochid_application_task_serialization 
gzochid_client_received_message_task_serialization = 
  { 
    "received-message",
    &received_message_worker_serialization, 
    &received_message_data_serialization 
  };

void gzochid_application_session_received_message
(gzochid_application_context *context, gzochid_protocol_client *client, 
 unsigned char *msg, short len)
{
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;

  char *session_oid_str = NULL;
  void *data[3];

  g_mutex_lock (context->client_mapping_lock);
  session_oid_str = g_hash_table_lookup (context->clients_to_oids, client);
  g_mutex_unlock (context->client_mapping_lock);

  if (session_oid_str == NULL)
    return;
  else 
    {
      gzochid_application_task transactional_task;
      gzochid_application_task application_task;
      gzochid_transactional_application_task_execution *execution =
	gzochid_transactional_application_task_timed_execution_new
	(&transactional_task, NULL, game_context->tx_timeout);
      gzochid_task task;

      data[0] = session_oid_str;
      data[1] = msg;
      data[2] = &len;
      
      transactional_task.worker = 
	gzochid_scheme_application_received_message_worker;
      transactional_task.context = context;
      transactional_task.identity = client->identity;
      transactional_task.data = data;
      
      application_task.worker = gzochid_application_transactional_task_worker;
      application_task.context = context;
      application_task.identity = client->identity;
      application_task.data = execution;
      
      task.worker = gzochid_application_task_thread_worker;
      task.data = &application_task;
      gettimeofday (&task.target_execution_time, NULL);

      while (TRUE)
	{
	  gzochid_schedule_run_task (game_context->task_queue, &task);
	  if (!gzochid_application_should_retry (execution))
	    break;
	}

      gzochid_transactional_application_task_execution_free (execution);
    }
}

void gzochid_application_channel_message_received
(gzochid_application_context *context, gzochid_protocol_client *client, 
 char *channel, unsigned char *msg, short len)
{
}

void *gzochid_with_application_context 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 void *(*worker) (gpointer), gpointer data)
{
  gpointer ret = NULL;
  gboolean private_needs_context = 
    g_static_private_get (&thread_application_context_key) == NULL;
  SCM application_root_fluid = 
    scm_variable_ref
    (scm_c_module_lookup 
     (scm_c_resolve_module("gzochi app"), "%gzochi:application-root"));

  if (private_needs_context)
    {
      g_static_private_set (&thread_application_context_key, context, NULL);
      g_static_private_set (&thread_identity_key, identity, NULL);
      
      scm_fluid_set_x
	(application_root_fluid, 
	 scm_from_locale_string (context->descriptor->deployment_root));
    }
  
  ret = worker (data);

  if (private_needs_context)
    {
      g_static_private_set (&thread_application_context_key, NULL, NULL);
      g_static_private_set (&thread_identity_key, NULL, NULL);

      scm_fluid_set_x (application_root_fluid, SCM_UNSPECIFIED);
    }

  return ret;
}

gzochid_application_context *gzochid_get_current_application_context (void)
{
  return (gzochid_application_context *) 
    g_static_private_get (&thread_application_context_key);
}

gzochid_auth_identity *gzochid_get_current_identity (void)
{
  return (gzochid_auth_identity *) g_static_private_get (&thread_identity_key);
}

void gzochid_register_client_received_message_task_serialization (void)
{
  gzochid_task_register_serialization 
    (&gzochid_client_received_message_task_serialization);
}
