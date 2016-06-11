/* session.c: Client session management routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "auth_int.h"
#include "data.h"
#include "event.h"
#include "game.h"
#include "game-protocol.h"
#include "gzochid-auth.h"
#include "io.h"
#include "reloc.h"
#include "scheme-task.h"
#include "session.h"
#include "task.h"
#include "tx.h"
#include "txlog.h"
#include "util.h"

#define SESSION_PREFIX "s.session."

enum gzochid_client_session_operation
  {
    GZOCHID_CLIENT_SESSION_OP_DISCONNECT,
    GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS,
    GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE,
    GZOCHID_CLIENT_SESSION_OP_MESSAGE
  };

struct _gzochid_client_session_pending_operation
{
  enum gzochid_client_session_operation type;
  mpz_t target_session;
};

typedef struct _gzochid_client_session_pending_operation
gzochid_client_session_pending_operation;

struct _gzochid_client_session_pending_message_operation
{
  gzochid_client_session_pending_operation base;

  unsigned char *message;
  short len;
};

typedef struct _gzochid_client_session_pending_message_operation
gzochid_client_session_pending_message_operation;

struct _gzochid_client_session_transaction_context
{
  gzochid_application_context *context;
  
  gboolean login_failed;
  gzochid_client_session_pending_operation *login_operation;

  GList *operations;
};

typedef struct _gzochid_client_session_transaction_context
gzochid_client_session_transaction_context;

GQuark
gzochid_session_error_quark ()
{
  return g_quark_from_static_string ("gzochid-session-error-quark");
}

static gzochid_client_session_transaction_context *
create_transaction_context (gzochid_application_context *context)
{
  gzochid_client_session_transaction_context *tx_context = 
    calloc (1, sizeof (gzochid_client_session_transaction_context));

  tx_context->context = context;

  return tx_context;
}

static gzochid_client_session_pending_operation *
create_disconnect_operation (mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_DISCONNECT;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;
}

static gzochid_client_session_pending_operation *
create_login_success_operation (mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;  
}

static gzochid_client_session_pending_operation *
create_login_failure_operation (mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;  
}

static gzochid_client_session_pending_operation *
create_message_operation (mpz_t target_session, unsigned char *msg, short len)
{
  gzochid_client_session_pending_message_operation *msg_op = 
    malloc (sizeof (gzochid_client_session_pending_message_operation));
  gzochid_client_session_pending_operation *op = 
    (gzochid_client_session_pending_operation *) msg_op;

  op->type = GZOCHID_CLIENT_SESSION_OP_MESSAGE;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  msg_op->message = msg;
  msg_op->len = len;

  return op;
}

static int 
session_prepare (gpointer data)
{
  return TRUE;
}

static void 
free_operation (gpointer data)
{
  gzochid_client_session_pending_operation *op = data;

  mpz_clear (op->target_session);
  free (op);
}

static void 
cleanup_transaction (gzochid_client_session_transaction_context *tx_context)
{
  g_list_free_full (tx_context->operations, free_operation);

  if (tx_context->login_operation != NULL)
    free_operation (tx_context->login_operation);
  
  free (tx_context);
}

static void 
session_commit_operation
(gzochid_client_session_transaction_context *tx_context, 
 gzochid_client_session_pending_operation *op)
{
  gzochid_application_context *context = tx_context->context;
  gzochid_client_session_pending_message_operation *msg_op = NULL;

  char *oid_str = mpz_get_str (NULL, 16, op->target_session);
  gzochid_game_client *client = NULL;

  g_mutex_lock (&context->client_mapping_lock);
  client = g_hash_table_lookup (context->oids_to_clients, oid_str);

  if (client == NULL)
    {
      g_warning
	("Client not found for session '%s'; skipping operation.", oid_str);
      g_mutex_unlock (&context->client_mapping_lock);
      free (oid_str);
      return;
    }

  free (oid_str);

  switch (op->type)
    {
    case GZOCHID_CLIENT_SESSION_OP_DISCONNECT:
      gzochid_game_client_disconnect (client); break;
    case GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS:
      gzochid_game_client_login_success (client); break;
    case GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE:
      gzochid_game_client_login_failure (client); break;
    case GZOCHID_CLIENT_SESSION_OP_MESSAGE:

      if (!tx_context->login_failed 
	  || mpz_cmp (op->target_session, 
		      tx_context->login_operation->target_session) != 0)
	{
	  msg_op = (gzochid_client_session_pending_message_operation *) op;
	  gzochid_application_event_dispatch
	    (context->event_source,
	     gzochid_application_event_new (MESSAGE_SENT));
	  gzochid_game_client_send (client, msg_op->message, msg_op->len);
	}

      break;

    default:
      break;
    }

  g_mutex_unlock (&context->client_mapping_lock);
}

static void 
session_commit_operation_visitor (gpointer data, gpointer user_data)
{
  gzochid_client_session_transaction_context *tx_context = user_data;
  gzochid_client_session_pending_operation *op = data;

  session_commit_operation (tx_context, op);
}

static void 
session_commit (gpointer data)
{
  gzochid_client_session_transaction_context *tx_context = data;

  if (tx_context->login_operation != NULL)
    session_commit_operation (tx_context, tx_context->login_operation);

  g_list_foreach 
    (tx_context->operations, session_commit_operation_visitor, tx_context);

  cleanup_transaction (tx_context);
}

static void 
session_rollback (gpointer data)
{
  gzochid_client_session_transaction_context *tx_context = data;

  cleanup_transaction (tx_context);
}

static gzochid_client_session_handler *
deserialize_handler (gzochid_application_context *context, GString *in, 
		     GError **err)
{
  gzochid_client_session_handler *handler = malloc 
    (sizeof (gzochid_client_session_handler)); 

  handler->received_message = 
    gzochid_application_callback_serialization.deserializer (context, in, NULL);
  handler->disconnected = 
    gzochid_application_callback_serialization.deserializer (context, in, NULL);

  return handler;
}

static gpointer 
deserialize_client_session (gzochid_application_context *context, GString *in, 
			    GError **err)
{
  gzochid_auth_identity *identity = 
    gzochid_auth_identity_deserializer (context, in, NULL);
  GSequence *channels = gzochid_util_deserialize_sequence 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string, free);
  gzochid_client_session *session = gzochid_client_session_new (identity);

  g_sequence_free (session->channels);
  session->channels = channels;

  gzochid_util_deserialize_mpz (in, session->scm_oid);

  if (gzochid_util_deserialize_boolean (in))
    session->handler = deserialize_handler (context, in, NULL);

  gzochid_auth_identity_unref (identity);
  
  return session;
}

static void 
serialize_handler (gzochid_application_context *context, 
		   gzochid_client_session_handler *handler, GString *out, 
		   GError **err)
{
  gzochid_application_callback_serialization.serializer 
    (context, handler->received_message, out, NULL);
  gzochid_application_callback_serialization.serializer 
    (context, handler->disconnected, out, NULL);
}

static void 
serialize_client_session (gzochid_application_context *context, gpointer obj, 
			  GString *out, GError **err)
{
  gzochid_client_session *session = obj;

  gzochid_auth_identity_serializer (context, session->identity, out, NULL);
  gzochid_util_serialize_sequence 
    (session->channels, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);
  gzochid_util_serialize_mpz (session->scm_oid, out);
  if (session->handler != NULL)
    {
      gzochid_util_serialize_boolean (TRUE, out);
      serialize_handler (context, session->handler, out, NULL);
    }
  else gzochid_util_serialize_boolean (FALSE, out);
}

static void 
finalize_handler (gzochid_application_context *context, 
		  gzochid_client_session_handler *handler)
{
  gzochid_application_callback_serialization.finalizer 
    (context, handler->received_message);
  gzochid_application_callback_serialization.finalizer 
    (context, handler->disconnected);
  free (handler);
}

static void 
finalize_client_session (gzochid_application_context *context, gpointer obj)
{
  gzochid_client_session *session = obj;

  gzochid_auth_identity_finalizer (context, session->identity);
  g_sequence_free (session->channels);
  mpz_clear (session->scm_oid);

  if (session->handler != NULL)
    finalize_handler (context, session->handler);

  free (session);
}

static gzochid_transaction_participant session_participant =
  { "session", session_prepare, session_commit, session_rollback };

gzochid_io_serialization gzochid_client_session_serialization = 
  { 
    serialize_client_session, 
    deserialize_client_session, 
    finalize_client_session 
  };

static void 
remove_session (gzochid_application_context *context, const char *oid_str, 
		GError **err)
{
  GError *local_err = NULL;
  GString *binding = g_string_new (SESSION_PREFIX);

  g_string_append (binding, oid_str);
  gzochid_data_remove_binding (context, binding->str, &local_err);
  if (local_err == NULL)
    {
      mpz_t oid;
      gzochid_data_managed_reference *session_reference = NULL;

      mpz_init (oid);
      mpz_set_str (oid, oid_str, 16);

      session_reference = gzochid_data_create_reference_to_oid 
	(context, &gzochid_client_session_serialization, oid);
      gzochid_data_dereference (session_reference, &local_err);

      if (local_err == NULL)
	{
	  gzochid_client_session *session = session_reference->obj;
	  gzochid_data_managed_reference *scm_session_reference =
	    gzochid_data_create_reference_to_oid 
	    (context, &gzochid_scm_location_aware_serialization, 
	     session->scm_oid);
	  
	  gzochid_data_remove_object (scm_session_reference, &local_err);

	  if (local_err != NULL)
	    {
	      /* This might happen if the disconnect callback (or some other
		 piece of code) has already deleted the SCM representation of
		 the session, or if the login handler never completed
		 successfully. */

	      if (local_err->message != NULL)
		g_info 
		  ("Unable to remove Scheme object for session '%s': %s",
		   oid_str, local_err->message);
	    }

	  /* Don't attempt to remove the object if the transaction is in a
	     failed state - it's a waste of time and tends to irritate the 
	     storage engine. */
	  
	  if (!g_error_matches
	      (local_err, GZOCHID_DATA_ERROR, GZOCHID_DATA_ERROR_TRANSACTION))
	    {
	      g_clear_error (&local_err);
	      gzochid_data_remove_object (session_reference, &local_err);
	    }
	}
      if (local_err != NULL)
	g_propagate_error (err, local_err);	  
	
      mpz_clear (oid);
    }
  else g_propagate_error (err, local_err);
   
  g_string_free (binding, TRUE);
}

void 
gzochid_client_session_disconnected_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gpointer data)
{
  remove_session (context, data, NULL);
}

gzochid_client_session *
gzochid_client_session_new (gzochid_auth_identity *identity)
{
  gzochid_client_session *session = (gzochid_client_session *)
    calloc (1, sizeof (gzochid_client_session));

  session->identity = gzochid_auth_identity_ref (identity);
  session->channels = g_sequence_new (NULL);
  mpz_init (session->scm_oid);

  return session;
}

void 
gzochid_client_session_free (gzochid_client_session *session)
{
  gzochid_auth_identity_unref (session->identity);
  g_sequence_free (session->channels);
  mpz_clear (session->scm_oid);
  
  free (session);
}

static gzochid_client_session_transaction_context *
join_transaction (gzochid_application_context *context)
{
  gzochid_client_session_transaction_context *tx_context = NULL;

  if (! gzochid_transaction_active ()
      || (tx_context = gzochid_transaction_context (&session_participant)) 
      == NULL)
    {
      tx_context = create_transaction_context (context);
      gzochid_transaction_join (&session_participant, tx_context);
    }

  return tx_context;
}

void 
gzochid_client_session_disconnect (gzochid_application_context *context, 
				   gzochid_client_session *session)
{
  GError *err = NULL;
  char *oid_str = NULL;
  gzochid_client_session_transaction_context *tx_context = 
    join_transaction (context);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  oid_str = mpz_get_str (NULL, 16, reference->oid);

  remove_session (context, oid_str, &err);

  if (err == NULL)
    tx_context->operations = g_list_append 
      (tx_context->operations, create_disconnect_operation (reference->oid));
  else g_error_free (err);

  free (oid_str);
}

void 
gzochid_client_session_send_login_success (gzochid_application_context *context,
					   gzochid_client_session *session)
{
  gzochid_client_session_transaction_context *tx_context =
    join_transaction (context);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);
  assert (tx_context->login_operation == NULL);
  
  tx_context->login_operation = create_login_success_operation 
    (reference->oid);
}

void 
gzochid_client_session_send_login_failure (gzochid_application_context *context,
					   gzochid_client_session *session)
{
  gzochid_client_session_transaction_context *tx_context =
    join_transaction (context);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);
  assert (tx_context->login_operation == NULL);

  tx_context->login_operation = create_login_failure_operation (reference->oid);
  tx_context->login_failed = TRUE;
}

void 
gzochid_client_session_send_message (gzochid_application_context *context, 
				     gzochid_client_session *session, 
				     unsigned char *msg, short len)
{
  gzochid_client_session_transaction_context *tx_context =
    join_transaction (context);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_message_operation (reference->oid, msg, len));
}

struct _gzochid_persistence_task_data
{
  gpointer data;
  gzochid_io_serialization *serialization;
  gzochid_oid_holder *holder;
  char *prefix;
};

typedef struct _gzochid_persistence_task_data gzochid_persistence_task_data;

static gzochid_persistence_task_data *
gzochid_persistence_task_data_new (gzochid_io_serialization *serialization,
				   gpointer data, gzochid_oid_holder *holder,
				   char *prefix)
{
  gzochid_persistence_task_data *task =
    malloc (sizeof (gzochid_persistence_task_data));

  task->data = data;
  task->serialization = serialization;
  task->holder = holder;
  task->prefix = prefix;

  return task;
}

static void 
persistence_task_worker (gzochid_application_context *context,
			 gzochid_auth_identity *identity, gpointer data)
{
  gzochid_persistence_task_data *persistence_task = data;
  GString *binding = g_string_new (persistence_task->prefix);
  char *oid_str = NULL;

  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, persistence_task->serialization, persistence_task->data);
  
  mpz_set (persistence_task->holder->oid, reference->oid);
  
  oid_str = mpz_get_str (NULL, 16, persistence_task->holder->oid);
  g_string_append (binding, oid_str);
  free (oid_str);

  gzochid_data_set_binding_to_oid 
    (context, binding->str, persistence_task->holder->oid, NULL);

  g_string_free (binding, TRUE);
}

static void
persistence_task_cleanup_worker (gzochid_application_context *context,
				 gzochid_auth_identity *identity, gpointer data)
{
  free (data);
}

gzochid_task *
gzochid_data_persistence_task_new
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gzochid_io_serialization *serialization, gpointer data, 
 gzochid_oid_holder *holder, char *prefix)
{
  gzochid_persistence_task_data *task_data = gzochid_persistence_task_data_new 
    (serialization, data, holder, prefix);
  gzochid_application_task *task = gzochid_application_task_new
    (context, identity, persistence_task_worker, task_data);
  gzochid_application_task *cleanup_task = gzochid_application_task_new 
    (context, identity, persistence_task_cleanup_worker, task_data);
  gzochid_transactional_application_task_execution *execution = 
    gzochid_transactional_application_task_execution_new
    (task, NULL, cleanup_task);

  /* Not necessary to hold a ref to these, as we've transferred them to the
     execution. */
  
  gzochid_application_task_unref (task);
  gzochid_application_task_unref (cleanup_task);
  
  /* This task uses the re-executing task worker (in concert with 
     `gzochid_schedule_run_task' below) to ensure that the full transactional
     execution lifecycle - including retries - is completed before the oid 
     holder is freed. */

  gzochid_application_task *application_task = gzochid_application_task_new 
    (context, identity,
     gzochid_application_reexecuting_transactional_task_worker, execution);

  return gzochid_task_immediate_new 
    (gzochid_application_task_thread_worker, application_task);
}

void 
gzochid_client_session_persist (gzochid_application_context *context, 
				gzochid_client_session *session, mpz_t oid)
{
  gzochid_game_context *game_context = 
    (gzochid_game_context *) ((gzochid_context *) context)->parent;
  gzochid_oid_holder *holder = gzochid_oid_holder_new ();
  gzochid_task *task = gzochid_data_persistence_task_new
    (context, session->identity, &gzochid_client_session_serialization, session,
     holder, SESSION_PREFIX);

  gzochid_schedule_run_task (game_context->task_queue, task);  
  gzochid_task_free (task);

  mpz_set (oid, holder->oid);
  gzochid_oid_holder_free (holder);
}

void 
gzochid_sweep_client_sessions (gzochid_application_context *context,
			       GError **err)
{
  mpz_t oid;
  GError *local_err = NULL;
  char *next_binding = NULL;
  int prefix_len = strlen (SESSION_PREFIX), num_sessions = 0;

  mpz_init (oid);
  gzochid_tx_info (context, "Sweeping old client sessions.");
  next_binding = gzochid_data_next_binding_oid 
    (context, SESSION_PREFIX, oid, &local_err);

  assert (local_err == NULL);

  while (next_binding != NULL 
         && strncmp (SESSION_PREFIX, next_binding, prefix_len) == 0)
    {
      char *oid_str = mpz_get_str (NULL, 16, oid);
      char *next_next_binding = NULL;
      
      gzochid_scheme_application_disconnected_worker 
	(context, gzochid_auth_system_identity (), oid_str);      

      if (gzochid_transaction_rollback_only ())
	{
	  g_set_error
	    (err, GZOCHID_SESSION_ERROR, GZOCHID_SESSION_ERROR_DISCONNECT,
	     "Transaction rollback while sweeping disconnected session '%s'.",
	     oid_str);
	  
	  free (next_binding);
	  free (oid_str);
	  mpz_clear (oid);
	  
	  return;
	}

      free (oid_str);
      next_next_binding = gzochid_data_next_binding_oid 
	(context, next_binding, oid, &local_err);

      assert (local_err == NULL);

      free (next_binding);
      next_binding = next_next_binding;
      num_sessions++;
    }

  gzochid_tx_info (context, "Swept %d session(s).", num_sessions);
  mpz_clear (oid);  
}
