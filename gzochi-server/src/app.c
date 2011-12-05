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
  mpz_t oid;

  void **ptrs = (void **) data;
  char *oid_str = (char *) ptrs[0];
  GHashTable *properties = (GHashTable *) ptrs[1];

  gzochid_data_managed_reference *callback_reference = NULL;

  mpz_init (oid);
  mpz_set_str (oid, oid_str, 16);
  
  callback_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_scheme_data_serialization, oid);
  gzochid_data_dereference (callback_reference);

  gzochid_scheme_invoke 
    (context,
     identity,
     "gzochi:execute-initialized",
     g_list_append
     (g_list_append
      (g_list_append (NULL, "gzochi"), "private"), "app"),
     scm_list_2 ((SCM) callback_reference->obj,
		 gzochid_scheme_ghashtable_to_hashtable 
		 (properties, 
		  gzochid_scheme_string_hash,
		  gzochid_scheme_string_equiv,
		  (SCM (*) (gpointer)) scm_from_locale_string,
		  (SCM (*) (gpointer)) scm_from_locale_string)),
     SCM_BOOL_F);

  gzochid_data_set_binding_to_oid (context, "s.initializer", oid);
  mpz_clear (oid);
}

static void initialized_worker_serializer 
(gzochid_application_context *context, gzochid_application_worker worker, 
 GString *out)
{
}

static gzochid_application_worker initialized_worker_deserializer
(gzochid_application_context *context, GString *in)
{
  return initialized_worker;
}

static void initialized_data_serializer
(gzochid_application_context *context, void *ptr, GString *out)
{
  void **data = (void **) ptr;
  gzochid_util_serialize_string ((char *) data[0], out);
  gzochid_util_serialize_hash_table 
    ((GHashTable *) data[1], 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, 
     out);
}

static void *initialized_data_deserializer
(gzochid_application_context *context, GString *in)
{
  void **data = malloc (sizeof (void *) * 2);
  data[0] = gzochid_util_deserialize_string (in);
  data[1] = gzochid_util_deserialize_hash_table 
    (in, g_str_hash, g_str_equal,
     (gpointer (*) (GString *)) gzochid_util_deserialize_string, 
     (gpointer (*) (GString *)) gzochid_util_deserialize_string);
  return data;
}

static gzochid_data_worker_serialization initialized_worker_serialization =
  { initialized_worker_serializer, initialized_worker_deserializer };
static gzochid_io_serialization initialized_data_serialization =
  { initialized_data_serializer, initialized_data_deserializer };
gzochid_task_serialization gzochid_initialized_task_serialization = 
  { &initialized_worker_serialization, &initialized_data_serialization };

static gzochid_task *initialized_callback_to_task 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_application_callback *callback, 
 GHashTable *properties)
{
  SCM cb = gzochid_scheme_create_callback (callback, NULL);
  gzochid_task *task = calloc (1, sizeof (gzochid_task));
  void **data = malloc (sizeof (void *) * 2);

  gzochid_data_managed_reference *cb_ref = gzochid_data_create_reference_sync 
    (context, identity, &gzochid_scheme_data_serialization, cb);  

  data[0] = mpz_get_str (NULL, 16, cb_ref->oid);
  data[1] = properties;

  task->worker = initialized_worker;
  task->data = data;

  return task;
}

static void run_async_transactional (gpointer data)
{
  gzochid_application_context *context = (gzochid_application_context *) data;
  SCM persisted_callback = (gzochid_application_callback *) 
    gzochid_data_get_binding 
    (context, "s.initializer", &gzochid_scheme_data_serialization);

  gzochid_auth_identity *system_identity = calloc 
    (1, sizeof (gzochid_auth_identity));
  system_identity->name = "[SYSTEM]";

  if (persisted_callback == NULL)
    {
      gzochid_task *task = initialized_callback_to_task
	(context, system_identity, context->descriptor->initialized, 
	 context->descriptor->properties);

      gzochid_run_durable_task
	(context, system_identity, task, 
	 &gzochid_initialized_task_serialization);
    }
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

static void client_logged_in_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_client_session *session = NULL;
  SCM handler = SCM_BOOL_F;

  char **oids = (char **) data;

  mpz_t cb_oid, session_oid;

  gzochid_data_managed_reference *callback_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *session_scm_reference = NULL;

  mpz_init (cb_oid);
  mpz_init (session_oid);

  mpz_set_str (cb_oid, oids[0], 16);
  mpz_set_str (session_oid, oids[1], 16);
  
  callback_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_scheme_data_serialization, cb_oid);
  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);

  gzochid_data_dereference (session_reference);
  session = (gzochid_client_session *) session_reference->obj;

  session_scm_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization, session->scm_oid);

  gzochid_data_dereference (callback_reference);
  gzochid_data_dereference (session_scm_reference);

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

  mpz_clear (cb_oid);
  mpz_clear (session_oid);

  free (oids);
}

static gzochid_task *logged_in_callback_to_task 
(gzochid_application_context *context, gzochid_application_callback *callback, 
 gzochid_data_managed_reference *reference)
{
  SCM cb = gzochid_scheme_create_callback (callback, NULL);
  gzochid_data_managed_reference *cb_ref = NULL;
  gzochid_task *task = calloc (1, sizeof (gzochid_task));
  char **oids = malloc (sizeof (char *) * 2);
  gzochid_client_session *session = NULL;

  gzochid_data_dereference (reference);
  session = (gzochid_client_session *) reference->obj;

  cb_ref = gzochid_data_create_reference_sync 
    (context, session->identity, &gzochid_scheme_data_serialization, cb);  

  oids[0] = mpz_get_str (NULL, 16, cb_ref->oid);
  oids[1] = mpz_get_str (NULL, 16, reference->oid);

  task->worker = client_logged_in_worker;
  task->data = oids;

  return task;
}

static void client_logged_in_worker_serializer 
(gzochid_application_context *context, gzochid_application_worker worker, 
 GString *out)
{
}

static gzochid_application_worker client_logged_in_worker_deserializer
(gzochid_application_context *context, GString *in)
{
  return client_logged_in_worker;
}

static void client_logged_in_data_serializer
(gzochid_application_context *context, void *ptr, GString *out)
{
  char **data = (char **) ptr;
  gzochid_util_serialize_string (data[0], out);
  gzochid_util_serialize_string (data[1], out);
}

static void *client_logged_in_data_deserializer
(gzochid_application_context *context, GString *in)
{
  char **data = malloc (sizeof (char *) * 2);
  data[0] = gzochid_util_deserialize_string (in);
  data[1] = gzochid_util_deserialize_string (in);
  return data;
}

static gzochid_data_worker_serialization client_logged_in_worker_serialization =
  { client_logged_in_worker_serializer, client_logged_in_worker_deserializer };

static gzochid_io_serialization client_logged_in_data_serialization =
  { client_logged_in_data_serializer, client_logged_in_data_deserializer };

gzochid_task_serialization gzochid_client_logged_in_task_serialization = 
  { 
    &client_logged_in_worker_serialization, 
    &client_logged_in_data_serialization 
  };

static void client_logged_in (gpointer data)
{
  void **args = (void **) data;

  gzochid_application_context *context = 
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];
  gzochid_client_session *session = gzochid_client_session_new (identity);
  
  SCM scm_session = SCM_BOOL_F;
  gzochid_data_managed_reference *reference = NULL;
  gzochid_data_managed_reference *scm_reference = NULL;
  gzochid_task *task = NULL;

  reference = gzochid_data_create_reference
    (context, &gzochid_client_session_serialization, session);
  scm_session = gzochid_scheme_create_client_session (session, reference->oid);
  scm_reference = gzochid_data_create_reference
    (context, &gzochid_scheme_data_serialization, scm_session);

  mpz_set (session->scm_oid, scm_reference->oid);
  task = logged_in_callback_to_task
    (context, context->descriptor->logged_in, reference);

  gzochid_schedule_durable_task
    (context, identity, task, &gzochid_client_logged_in_task_serialization);

  args[2] = reference;
}

static void application_client_logged_in
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  gzochid_transaction_execute (client_logged_in, data);  
}

void gzochid_application_client_logged_in
(gzochid_application_context *context, gzochid_protocol_client *client)
{
  void *data[3];
  gzochid_application_work_unit *unit = 
    gzochid_application_work_unit_new (application_client_logged_in, data);
  gzochid_data_managed_reference *reference = NULL;
  char *oid_str = NULL;
  
  data[0] = context;
  data[1] = client->identity;
  data[2] = NULL;

  g_mutex_lock (unit->lock);
  gzochid_application_schedule_work_unit (context, client->identity, unit);
  g_cond_wait (unit->cond, unit->lock);
  g_mutex_unlock (unit->lock);
  
  reference = (gzochid_data_managed_reference *) data[2];

  oid_str = mpz_get_str (NULL, 16, reference->oid);
  g_mutex_lock (context->client_mapping_lock);
  g_hash_table_insert (context->oids_to_clients, oid_str, client);
  g_hash_table_insert (context->clients_to_oids, client, oid_str);
  g_mutex_unlock (context->client_mapping_lock);

  gzochid_application_work_unit_free (unit);
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

static void client_disconnected_worker_serializer 
(gzochid_application_context *context, gzochid_application_worker worker, 
 GString *out)
{
}

static gzochid_application_worker client_disconnected_worker_deserializer
(gzochid_application_context *context, GString *in)
{
  return client_disconnected_worker;
}

static void string_serializer
(gzochid_application_context *context, void *ptr, GString *out)
{
  gzochid_util_serialize_string ((char *) ptr, out);
}

static void *string_deserializer
(gzochid_application_context *context, GString *in)
{
  return gzochid_util_deserialize_string (in);
}

static gzochid_data_worker_serialization 
client_disconnected_worker_serialization =
  { 
    client_disconnected_worker_serializer, 
    client_disconnected_worker_deserializer 
  };

static gzochid_io_serialization client_disconnected_data_serialization =
  { string_serializer, string_deserializer };

gzochid_task_serialization gzochid_client_disconnected_task_serialization = 
  { 
    &client_disconnected_worker_serialization, 
    &client_disconnected_data_serialization 
  };

static void client_disconnected (gpointer data)
{
  void **args = (void **) data;
  gzochid_task *task = calloc (1, sizeof (gzochid_task));

  gzochid_application_context *context =
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];

  task->worker = client_disconnected_worker;
  task->data = args[2];

  gzochid_schedule_durable_task
    (context, identity, task, &gzochid_client_disconnected_task_serialization);
}

void gzochid_application_client_disconnected
(gzochid_application_context *context, gzochid_protocol_client *client)
{
  char *session_oid_str = NULL;
  void *data[3];

  g_mutex_lock (context->client_mapping_lock);
  session_oid_str = g_hash_table_lookup (context->clients_to_oids, client);

  if (session_oid_str == NULL)
    {
      g_mutex_unlock (context->client_mapping_lock);
      return;
    }

  data[0] = context;
  data[1] = client->identity;
  data[2] = session_oid_str;

  gzochid_transaction_execute (client_disconnected, data);  
  g_mutex_unlock (context->client_mapping_lock);
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

static void client_received_message_worker_serializer 
(gzochid_application_context *context, gzochid_application_worker worker, 
 GString *out)
{
}

static gzochid_application_worker client_received_message_worker_deserializer
(gzochid_application_context *context, GString *in)
{
  return client_received_message_worker;
}

static void client_received_message_data_serializer
(gzochid_application_context *context, void *ptr, GString *out)
{
  char **data = (char **) ptr;
  short *len = (short *) data[2];

  gzochid_util_serialize_string (data[0], out);
  gzochid_util_serialize_bytes (data[1], *len, out);
}

static void *client_received_message_data_deserializer
(gzochid_application_context *context, GString *in)
{
  int len = 0;
  void **data = malloc (sizeof (void *) * 3);
  short *slen = malloc (sizeof (short));

  data[0] = gzochid_util_deserialize_string (in);
  data[1] = gzochid_util_deserialize_bytes (in, &len);

  *slen = (short) len;

  data[2] = slen;
  return data;
}

static gzochid_data_worker_serialization 
client_received_message_worker_serialization =
  { 
    client_received_message_worker_serializer, 
    client_received_message_worker_deserializer 
  };

static gzochid_io_serialization client_received_message_data_serialization =
  { 
    client_received_message_data_serializer, 
    client_received_message_data_deserializer 
  };

gzochid_task_serialization gzochid_client_received_message_task_serialization = 
  { 
    &client_received_message_worker_serialization, 
    &client_received_message_data_serialization 
  };

static void client_received_message (gpointer data)
{
  void **args = (void **) data;
  void **task_data = malloc (sizeof (void *) * 3);
  gzochid_task *task = calloc (1, sizeof (gzochid_task));

  gzochid_application_context *context =
    (gzochid_application_context *) args[0];
  gzochid_auth_identity *identity = (gzochid_auth_identity *) args[1];
  char *oid_str = (char *) args[2];
  char *message = (char *) args[3];
  short *message_len = (short *) args[4];

  char *task_message = malloc (sizeof (char) * *message_len);
  short *task_message_len = malloc (sizeof (short));

  task_message = memcpy (task_message, message, *message_len);
  *task_message_len = *message_len;

  task_data[0] = oid_str;
  task_data[1] = task_message;
  task_data[2] = task_message_len;

  task->worker = client_received_message_worker;
  task->data = task_data;

  gzochid_schedule_durable_task
    (context, identity, task, 
     &gzochid_client_received_message_task_serialization);
}

void gzochid_application_session_received_message
(gzochid_application_context *context, gzochid_protocol_client *client, 
 unsigned char *msg, short len)
{
  char *session_oid_str = NULL;
  void *data[5];

  g_mutex_lock (context->client_mapping_lock);
  session_oid_str = g_hash_table_lookup (context->clients_to_oids, client);

  if (session_oid_str == NULL)
    {
      g_mutex_unlock (context->client_mapping_lock);
      return;
    }

  data[0] = context;
  data[1] = client->identity;
  data[2] = session_oid_str;
  data[3] = msg;
  data[4] = &len;

  gzochid_transaction_execute (client_received_message, data);  
  g_mutex_unlock (context->client_mapping_lock);
}

void gzochid_application_channel_message_received
(gzochid_application_context *context, gzochid_protocol_client *client, 
 char *channel, unsigned char *msg, short len)
{
}

typedef struct _work_unit_wrapper
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  gzochid_application_work_unit *unit;
} work_unit_wrapper;

static void execute_work_unit (gpointer data, gpointer user_data)
{
  work_unit_wrapper *wrapper = (work_unit_wrapper *) data;

  g_mutex_lock (wrapper->unit->lock);
  wrapper->unit->worker 
    (wrapper->context, wrapper->identity, wrapper->unit->data);
  g_cond_broadcast (wrapper->unit->cond);
  g_mutex_unlock (wrapper->unit->lock);

  free (wrapper);
}

gzochid_application_work_unit *gzochid_application_work_unit_new 
(gzochid_application_worker worker, gpointer data)
{
  gzochid_application_work_unit *unit = 
    calloc (1, sizeof (gzochid_application_work_unit));

  unit->lock = g_mutex_new ();
  unit->cond = g_cond_new ();
  unit->worker = worker;
  unit->data = data;

  return unit;
}

void gzochid_application_work_unit_free (gzochid_application_work_unit *unit)
{
  g_mutex_free (unit->lock);
  g_cond_free (unit->cond);
  free (unit);
}

void gzochid_application_schedule_work_unit
(gzochid_application_context *app_context, gzochid_auth_identity *identity, 
 gzochid_application_work_unit *unit)
{
  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  work_unit_wrapper *wrapper = malloc (sizeof (work_unit_wrapper));

  wrapper->context = app_context;
  wrapper->identity = identity;
  wrapper->unit = unit;
  
  gzochid_guile_thread_pool_push 
    (game_context->pool, execute_work_unit, wrapper, NULL);
}
