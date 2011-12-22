/* app.c: Application context routines for gzochid
 * Copyright (C) 2011 Julian Graham
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
#include <stdlib.h>

#include "app.h"
#include "auth.h"
#include "context.h"
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

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_APPLICATION_STATE_RUNNING);
}

static void initialize_data (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_application_context *app_context = 
    (gzochid_application_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  char *data_dir = g_strconcat 
    (game_context->work_dir, "/", app_context->descriptor->name, NULL);
  char *meta_db = g_strconcat (data_dir, "/meta.db", NULL);
  char *oids_db = g_strconcat (data_dir, "/oids.db", NULL);
  char *names_db = g_strconcat (data_dir, "/names.db", NULL);

  if (!g_file_test (data_dir, G_FILE_TEST_EXISTS))
    {
      gzochid_notice 
	("Work directory %s does not exist; creating...", data_dir);
      if (g_mkdir_with_parents (data_dir, 493) != 0)
	gzochid_err ("Unable to create work directory %s.", data_dir);
    }
  else if (!g_file_test (data_dir, G_FILE_TEST_IS_DIR))
    gzochid_err ("%s is not a directory.", data_dir);

  app_context->meta = gzochid_storage_open (meta_db);
  app_context->oids = gzochid_storage_open (oids_db);
  app_context->names = gzochid_storage_open (names_db);

  free (data_dir);
  free (meta_db);
  free (oids_db);
  free (names_db);
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

static void initialized_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  GHashTable *properties = (GHashTable *) data;
  gzochid_data_managed_reference *callback_reference = NULL;
  SCM callback = gzochid_scheme_create_callback 
    (context->descriptor->initialized, NULL);

  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, callback);

  gzochid_scheme_invoke 
    (context,
     identity,
     "gzochi:execute-initialized",
     g_list_append
     (g_list_append
      (g_list_append (NULL, "gzochi"), "private"), "app"),
     scm_list_2 (callback,
		 gzochid_scheme_ghashtable_to_hashtable 
		 (properties, 
		  gzochid_scheme_string_hash,
		  gzochid_scheme_string_equiv,
		  (SCM (*) (gpointer)) scm_from_locale_string,
		  (SCM (*) (gpointer)) scm_from_locale_string)),
     SCM_BOOL_F);

  gzochid_data_set_binding_to_oid 
    (context, "s.initializer", callback_reference->oid);
}

static void transactional_task_worker (gpointer data)
{
  void **args = (void **) data;
  
  gzochid_application_context *context = 
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];
  gzochid_transactional_application_task *task = 
    (gzochid_transactional_application_task *) args[2];
  
  task->worker (context, identity, task->data);
}

static void gzochid_application_transactional_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  void *args[3];
  
  args[0] = context;
  args[1] = identity;
  args[2] = data;

  gzochid_transaction_execute (transactional_task_worker, args);  
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

static void run_async_transactional (gpointer data)
{
  gzochid_application_context *context = (gzochid_application_context *) data;
  SCM persisted_callback = (SCM) gzochid_data_get_binding 
    (context, "s.initializer", &gzochid_scheme_data_serialization);

  gzochid_auth_identity *system_identity = calloc 
    (1, sizeof (gzochid_auth_identity));
  system_identity->name = "[SYSTEM]";

  if (persisted_callback == NULL)
    {
      gzochid_game_context *game_context = 
	(gzochid_game_context *) ((gzochid_context *) context)->parent;

      gzochid_transactional_application_task transactional_task;
      gzochid_application_task application_task;
      gzochid_task task;

      transactional_task.worker = initialized_worker;
      transactional_task.data = context->descriptor->properties;
      
      application_task.worker = gzochid_application_transactional_task_worker;
      application_task.context = context;
      application_task.identity = system_identity;
      application_task.data = &transactional_task;

      task.worker = gzochid_application_task_thread_worker;
      task.data = &application_task;
      gettimeofday (&task.target_execution_time, NULL);

      gzochid_schedule_run_task (game_context->task_queue, &task);
    }
  else gzochid_restart_tasks (context);
}

static void run_async (gpointer data, gpointer user_data)
{
  gzochid_transaction_execute (run_async_transactional, data);
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

gzochid_io_serialization gzochid_application_callback_serialization = 
  { serialize_callback, deserialize_callback };

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

  return context;
}

void gzochid_application_context_free (gzochid_application_context *app_context)
{
  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_context_free (context);

  g_mutex_free (app_context->free_oids_lock);
  g_list_free (app_context->free_oid_blocks);

  free (context->fsm->name);
  free (context);
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
    (fsm, GZOCHID_APPLICATION_STATE_RUNNING, GZOCHID_APPLICATION_STATE_STOPPED);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_RUNNING, GZOCHID_APPLICATION_STATE_PAUSED);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_PAUSED, GZOCHID_APPLICATION_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_APPLICATION_STATE_PAUSED, GZOCHID_APPLICATION_STATE_STOPPED);

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_data, context);
  gzochid_fsm_on_enter 
    (fsm, GZOCHID_APPLICATION_STATE_INITIALIZING, initialize_complete, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_APPLICATION_STATE_RUNNING, run, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_APPLICATION_STATE_STOPPED, stop, context);

  context->authenticator = gzochid_auth_function_pass_thru;
  context->descriptor = descriptor;
  
  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}

static gzochid_application_callback *scm_to_callback 
(gzochid_application_context *context, SCM scm_callback)
{
  GList *module = gzochid_scheme_callback_module (scm_callback);
  char *procedure = gzochid_scheme_callback_procedure (scm_callback);
  gzochid_data_managed_reference *reference = 
    gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, scm_callback);

  mpz_t oid;
  
  mpz_init (oid);
  mpz_set (oid, reference->oid);      
    
  return gzochid_application_callback_new (procedure, module, oid);
}

typedef struct _gzochid_persistence_task_data
{
  gpointer data;
  gzochid_io_serialization *serialization;
  mpz_t oid;
} gzochid_persistence_task_data;

void gzochid_persistence_task_worker
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_persistence_task_data *persistence_task = 
    (gzochid_persistence_task_data *) data;

  gzochid_data_managed_reference *reference = 
    gzochid_data_create_reference 
    (context, persistence_task->serialization, persistence_task->data);
  
  mpz_set (persistence_task->oid, reference->oid);
}

gzochid_persistence_task_data *gzochid_persistence_task_data_new
(gpointer data, gzochid_io_serialization *serialization)
{
  gzochid_persistence_task_data *task = 
    malloc (sizeof (gzochid_persistence_task_data));
  
  task->data = data;
  task->serialization = serialization;
  mpz_init (task->oid);

  return task;
}

void gzochid_persistence_task_data_free (gzochid_persistence_task_data *task)
{
  mpz_clear (task->oid);
  free (task);
}

static void client_logged_in_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  mpz_t session_oid;
  char *session_oid_str = (char *) data;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;

  SCM scm_session = SCM_BOOL_F;
  gzochid_data_managed_reference *session_scm_reference = NULL;

  SCM cb = SCM_BOOL_F;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM handler = SCM_BOOL_F;

  mpz_init (session_oid);
  mpz_set_str (session_oid, session_oid_str, 16);
  session_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, session_oid);
  mpz_clear (session_oid);

  gzochid_data_dereference (session_reference);
  session = (gzochid_client_session *) session_reference->obj;

  scm_session = gzochid_scheme_create_client_session 
    (session, session_reference->oid);
  session_scm_reference = 
    gzochid_data_create_reference
    (context, &gzochid_scheme_data_serialization, scm_session);

  cb = gzochid_scheme_create_callback (context->descriptor->logged_in, NULL);
  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, cb);

  handler = gzochid_scheme_invoke 
    (context,
     identity,
     "gzochi:execute-logged-in",
     g_list_append
     (g_list_append
      (g_list_append (NULL, "gzochi"), "private"), "app"),
     scm_list_2 ((SCM) callback_reference->obj, 
		 (SCM) session_scm_reference->obj), 
     SCM_BOOL_F);
  
  if (scm_is_false (handler))
    gzochid_client_session_send_login_failure (context, session);
  else
    {
      gzochid_client_session_handler *session_handler = 
	malloc (sizeof (gzochid_client_session_handler));

      session_handler->received_message = 
	scm_to_callback
	(context, gzochid_scheme_handler_received_message (handler));
      session_handler->disconnected =
	scm_to_callback 
	(context, gzochid_scheme_handler_disconnected (handler));

      session->handler = session_handler;

      gzochid_data_mark (context, session);
      gzochid_client_session_send_login_success (context, session);
    }
}

void gzochid_application_client_logged_in
(gzochid_application_context *context, gzochid_protocol_client *client)
{
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;

  gzochid_client_session *session = 
    gzochid_client_session_new (client->identity);

  char *oid_str = NULL;
  
  gzochid_transactional_application_task transactional_task;
  gzochid_application_task application_task;
  gzochid_task task;

  gzochid_persistence_task_data *persistence_task = 
    gzochid_persistence_task_data_new 
    (session, &gzochid_client_session_serialization);

  transactional_task.worker = gzochid_persistence_task_worker;
  transactional_task.data = persistence_task;

  application_task.worker = gzochid_application_transactional_task_worker;
  application_task.context = context;
  application_task.identity = client->identity;
  application_task.data = &transactional_task;

  task.worker = gzochid_application_task_thread_worker;
  task.data = &application_task;
  gettimeofday (&task.target_execution_time, NULL);

  gzochid_schedule_run_task (game_context->task_queue, &task);  

  oid_str = mpz_get_str (NULL, 16, persistence_task->oid);
  gzochid_persistence_task_data_free (persistence_task);

  g_hash_table_insert (context->oids_to_clients, oid_str, client);
  g_hash_table_insert (context->clients_to_oids, client, oid_str);

  transactional_task.worker = client_logged_in_worker;
  transactional_task.data = oid_str;

  application_task.worker = gzochid_application_transactional_task_worker;
  application_task.context = context;
  application_task.identity = client->identity;
  application_task.data = &transactional_task;

  task.worker = gzochid_application_task_thread_worker;
  task.data = &application_task;
  gettimeofday (&task.target_execution_time, NULL);

  gzochid_schedule_run_task (game_context->task_queue, &task);
}

static void client_disconnected_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;

  GList *gpa = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  mpz_t session_oid;
  mpz_init (session_oid);
  mpz_set_str (session_oid, (char *) ptr, 16);

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);
  gzochid_data_dereference (session_reference);
  session = (gzochid_client_session *) session_reference->obj;

  callback_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization, 
     session->handler->disconnected->scm_oid);
  gzochid_data_dereference (callback_reference);
      
  gzochid_scheme_invoke
    (context, 
     identity,
     "gzochi:execute-disconnected", gpa,
     scm_list_1 ((SCM) callback_reference->obj),
     SCM_BOOL_F);

  g_list_free (gpa);
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
      gzochid_transactional_application_task *transactional_task =
	gzochid_transactional_application_task_new
	(client_disconnected_worker, session_oid_str);

      gzochid_application_task *application_task =
	gzochid_application_task_new
	(context, client->identity, 
	 gzochid_application_transactional_task_worker, transactional_task);

      gzochid_task *task = NULL;
      struct timeval now;
      gettimeofday (&now, NULL);

      g_hash_table_remove (context->clients_to_oids, client);
      g_hash_table_remove (context->oids_to_clients, session_oid_str);
      
      task = gzochid_task_new 
	(gzochid_application_task_thread_worker, application_task, now);
      
      gzochid_schedule_submit_task (game_context->task_queue, task);
      g_mutex_unlock (context->client_mapping_lock);
    }
}

static void client_received_message_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM bv = SCM_BOOL_F;
  GList *gpa = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  unsigned char *message = NULL;
  short *message_len_ptr = NULL;
  void **data = (void **) ptr;

  mpz_t session_oid;
  mpz_init (session_oid);
  mpz_set_str (session_oid, (char *) data[0], 16);

  message = (unsigned char *) data[1];
  message_len_ptr = (short *) data[2];

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);
  gzochid_data_dereference (session_reference);
  session = (gzochid_client_session *) session_reference->obj;

  bv = gzochid_scheme_create_bytevector (message, *message_len_ptr);
  callback_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization, 
     session->handler->received_message->scm_oid);
  gzochid_data_dereference (callback_reference);
      
  gzochid_scheme_invoke
    (context, 
     identity,
     "gzochi:execute-received-message", gpa,
     scm_list_2 ((SCM) callback_reference->obj, bv),
     SCM_BOOL_F);

  g_list_free (gpa);
}

gzochid_application_worker_serialization received_message_worker_serialization =
  { NULL, NULL };

gzochid_io_serialization received_message_data_serialization = 
  { NULL, NULL };

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
      gzochid_transactional_application_task transactional_task;
      gzochid_application_task application_task;
      gzochid_task task;

      data[0] = session_oid_str;
      data[1] = msg;
      data[2] = &len;
      
      transactional_task.worker = client_received_message_worker;
      transactional_task.data = data;
      
      application_task.worker = gzochid_application_transactional_task_worker;
      application_task.context = context;
      application_task.identity = client->identity;
      application_task.data = &transactional_task;
      
      task.worker = gzochid_application_task_thread_worker;
      task.data = &application_task;
      gettimeofday (&task.target_execution_time, NULL);

      gzochid_schedule_run_task (game_context->task_queue, &task);
    }
}

void gzochid_application_channel_message_received
(gzochid_application_context *context, gzochid_protocol_client *client, 
 char *channel, unsigned char *msg, short len)
{
}
