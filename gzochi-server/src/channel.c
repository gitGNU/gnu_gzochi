/* channel.c: Channel management routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "app.h"
#include "app-task.h"
#include "auth_int.h"
#include "channel.h"
#include "event.h"
#include "game.h"
#include "game-protocol.h"
#include "gzochid-auth.h"
#include "io.h"
#include "log.h"
#include "scheme.h"
#include "session.h"
#include "task.h"
#include "tx.h"
#include "util.h"

#define CHANNEL_PREFIX "s.channel."

enum gzochid_channel_operation
  {
    GZOCHID_CHANNEL_OP_JOIN,
    GZOCHID_CHANNEL_OP_LEAVE,
    GZOCHID_CHANNEL_OP_SEND,
    GZOCHID_CHANNEL_OP_CLOSE
  };

struct _gzochid_channel_pending_operation
{
  enum gzochid_channel_operation type;
  mpz_t target_channel;
  struct timeval timestamp;
};

typedef struct _gzochid_channel_pending_operation
gzochid_channel_pending_operation;

struct _gzochid_channel_pending_send_operation
{
  gzochid_channel_pending_operation base;
  unsigned char *message;
  short len;
};

typedef struct _gzochid_channel_pending_send_operation
gzochid_channel_pending_send_operation;

struct _gzochid_channel_pending_membership_operation
{
  gzochid_channel_pending_operation base;
  mpz_t target_session;
};

typedef struct _gzochid_channel_pending_membership_operation
gzochid_channel_pending_membership_operation;

enum gzochid_channel_message_type
  {
    GZOCHID_CHANNEL_MESSAGE_JOIN,
    GZOCHID_CHANNEL_MESSAGE_LEAVE,
    GZOCHID_CHANNEL_MESSAGE_SEND
  };

struct _gzochid_channel_message
{
  gzochid_application_context *app_context;
  enum gzochid_channel_message_type type;
  char *oid_str;
};

typedef struct _gzochid_channel_message gzochid_channel_message;

struct _gzochid_channel_payload_message
{
  gzochid_channel_message base;
  unsigned char *msg;
  short len;
};

typedef struct _gzochid_channel_payload_message
gzochid_channel_payload_message;

static gzochid_channel_message *
gzochid_channel_message_new (gzochid_application_context *app_context,
			     enum gzochid_channel_message_type type,
			     char *oid_str)
{
  gzochid_channel_message *message = malloc (sizeof (gzochid_channel_message));

  message->app_context = app_context;
  message->type = type;
  message->oid_str = oid_str;

  return message;
}

static gzochid_channel_payload_message *
gzochid_channel_payload_message_new (gzochid_application_context *app_context,
				     enum gzochid_channel_message_type type,
				     char *oid_str, unsigned char *msg,
				     short len)
{
  gzochid_channel_payload_message *payload_message =
    malloc (sizeof (gzochid_channel_payload_message));
  gzochid_channel_message *message =
    (gzochid_channel_message *) payload_message;

  message->app_context = app_context;
  message->type = type;
  message->oid_str = oid_str;

  payload_message->msg = malloc (sizeof (char) * len);
  payload_message->len = len;
  
  memcpy (payload_message->msg, msg, len);
  
  return payload_message;
}

struct _gzochid_channel_side_effects_transaction_context 
{
  gzochid_application_context *context;
  gzochid_channel *channel;
  GList *messages;
};

typedef struct _gzochid_channel_side_effects_transaction_context 
gzochid_channel_side_effects_transaction_context;

static gzochid_channel_side_effects_transaction_context *
create_side_effects_transaction_context (gzochid_application_context *context,
					 gzochid_channel *channel)
{
  gzochid_channel_side_effects_transaction_context *tx_context = 
    calloc (1, sizeof (gzochid_channel_side_effects_transaction_context));

  tx_context->context = context;
  tx_context->channel = channel;

  return tx_context;
}

static int
channel_side_effects_prepare (gpointer data)
{
  return TRUE;
}

static void
free_message (gpointer data)
{
  gzochid_channel_message *message = data;

  if (message->type == GZOCHID_CHANNEL_MESSAGE_SEND)
    {
      gzochid_channel_payload_message *payload_message =
	(gzochid_channel_payload_message *) message;

      free (payload_message->msg);
    }
  else if (message->type == GZOCHID_CHANNEL_MESSAGE_LEAVE)
    free (message->oid_str);

  free (message);
}

static void
cleanup_side_effects_transaction
(gzochid_channel_side_effects_transaction_context *tx_context)
{
  g_list_free_full (tx_context->messages, free_message);
  free (tx_context);
}

static void
commit_channel_side_effect_message (gpointer data, gpointer user_data)
{
  gzochid_channel_message *message = data;
  gzochid_channel_payload_message *payload_message = NULL;
  gzochid_game_client *client = NULL;

  g_mutex_lock (&message->app_context->client_mapping_lock);
  client = g_hash_table_lookup
    (message->app_context->oids_to_clients, message->oid_str);

  if (client != NULL)
    {
      switch (message->type)
	{
	case GZOCHID_CHANNEL_MESSAGE_SEND:
	  payload_message = (gzochid_channel_payload_message *) message;
	  gzochid_application_event_dispatch
	    (message->app_context->event_source,
	     gzochid_application_event_new (MESSAGE_SENT));
	  gzochid_game_client_send
	    (client, payload_message->msg, payload_message->len);
      
	default:  break;
	}
    }
  else g_warning ("Client not found for channel session '%s'; skipping.",
		  message->oid_str);
	  
  g_mutex_unlock (&message->app_context->client_mapping_lock);
}

static void
channel_side_effects_commit (gpointer data)
{
  gzochid_channel_side_effects_transaction_context *tx_context = data;

  g_list_foreach 
    (tx_context->messages, commit_channel_side_effect_message, 
     tx_context->channel);
  
  cleanup_side_effects_transaction (tx_context);
}

static void
channel_side_effects_rollback (gpointer data)
{
  gzochid_channel_side_effects_transaction_context *tx_context = data;
  
  cleanup_side_effects_transaction (tx_context);
}

static gzochid_transaction_participant channel_side_effects_participant =
  { 
    "channel-side-effects", 
    channel_side_effects_prepare, 
    channel_side_effects_commit, 
    channel_side_effects_rollback 
  };

static gzochid_channel_side_effects_transaction_context *
join_side_effects_transaction (gzochid_application_context *context,
			       gzochid_channel *channel)
{
  gzochid_channel_side_effects_transaction_context *tx_context = NULL;
  if (!gzochid_transaction_active()
      || (gzochid_transaction_context 
	  (&channel_side_effects_participant) == NULL))
    {
      tx_context = create_side_effects_transaction_context (context, channel);
      gzochid_transaction_join (&channel_side_effects_participant, tx_context);
    }
  else tx_context = gzochid_transaction_context
	 (&channel_side_effects_participant);

  return tx_context;
}

/* Frees the specified operation, potentially downcasting it to a more specific
   type to allow operation-specific fields to be freed first. */

static void
free_operation (gzochid_channel_pending_operation *op)
{
  if (op->type == GZOCHID_CHANNEL_OP_SEND)
    {
      gzochid_channel_pending_send_operation *send_op =
	(gzochid_channel_pending_send_operation *) op;

      free (send_op->message);
    }
  else if (op->type == GZOCHID_CHANNEL_OP_JOIN
	   || op->type == GZOCHID_CHANNEL_OP_LEAVE)
    {
      gzochid_channel_pending_membership_operation *membership_op =
	(gzochid_channel_pending_membership_operation *) op;

      mpz_clear (membership_op->target_session);
    }
  
  mpz_clear (op->target_channel);  
  free (op);
}

/* Exposes `free_operation' as a `gzochid_application_worker'. */

static void
free_operation_worker (gzochid_application_context *context,
		       gzochid_auth_identity *identity, gpointer data)
{
  free_operation (data);
}

static void
close_channel (gzochid_application_context *context,
	       gzochid_auth_identity *identity, gpointer data)
{
  GError *err = NULL;
  gzochid_channel_pending_operation *op = data;
  gzochid_data_managed_reference *channel_reference =
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, op->target_channel);
  gzochid_channel_side_effects_transaction_context *tx_context = NULL;

  char *channel_oid_str = NULL; 
  gzochid_channel *channel = NULL;
  GSequenceIter *iter = NULL;
  mpz_t session_oid;

  gzochid_data_dereference (channel_reference, &err);
  
  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  channel = channel_reference->obj;

  mpz_init (session_oid);
  channel_oid_str = mpz_get_str (NULL, 16, op->target_channel);
  iter = g_sequence_get_begin_iter (channel->sessions);
  tx_context = join_side_effects_transaction (context, channel);

  g_mutex_lock (&context->client_mapping_lock);

  while (!g_sequence_iter_is_end (iter))
    {
      char *session_oid_str = g_sequence_get (iter);
      gzochid_data_managed_reference *session_reference = NULL;
      gzochid_client_session *session = NULL;
      GSequenceIter *session_iter = NULL;

      mpz_set_str (session_oid, session_oid_str, 16);

      session_reference = gzochid_data_create_reference_to_oid 
	(context, &gzochid_client_session_serialization, session_oid);

      gzochid_data_dereference (session_reference, &err);

      if (err != NULL)
	{
	  if (err->code != GZOCHID_DATA_ERROR_NOT_FOUND)
	    {
	      g_error_free (err);
	      return;
	    }
	  else 
	    {
	      g_error_free (err);
	      err = NULL;

	      iter = g_sequence_iter_next (iter);
	      continue;
	    }
	}
	
      session = session_reference->obj;
      session_iter = g_sequence_lookup
	(session->channels, channel_oid_str, gzochid_util_string_data_compare,
	 NULL);

      if (session_iter != NULL)
	{
	  g_sequence_remove (session_iter);

	  gzochid_data_mark 
	    (context, &gzochid_client_session_serialization, session, &err);
	      
	  if (err == NULL)
	    tx_context->messages = g_list_append
	      (tx_context->messages,
	       gzochid_channel_message_new
	       (context, GZOCHID_CHANNEL_MESSAGE_LEAVE, session_oid_str));
	  else
	    {
	      g_error_free (err);
	      break;
	    }
	}
      
      iter = g_sequence_iter_next (iter);
    }
        
  g_mutex_unlock (&context->client_mapping_lock);
  mpz_clear (session_oid);
  free (channel_oid_str);
}

static void
send_channel_message (gzochid_application_context *context,
		      gzochid_auth_identity *identity, gpointer data)
{
  GError *err = NULL;
  gzochid_channel_pending_send_operation *send_op = data;
  gzochid_channel_pending_operation *op =
    (gzochid_channel_pending_operation *) send_op;
  gzochid_data_managed_reference *channel_reference =
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, op->target_channel);
  gzochid_channel_side_effects_transaction_context *tx_context = NULL;

  gzochid_channel *channel = NULL;
  GSequenceIter *iter = NULL;
  gboolean channel_modified = FALSE;

  gzochid_data_dereference (channel_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }
  
  channel = channel_reference->obj;
  tx_context = join_side_effects_transaction (context, channel);
  iter = g_sequence_get_begin_iter (channel->sessions);

  while (!g_sequence_iter_is_end (iter))
    {
      char *session_oid_str = g_sequence_get (iter);
      
      g_mutex_lock (&context->client_mapping_lock);

      if (g_hash_table_contains (context->oids_to_clients, session_oid_str))
	{
	  tx_context->messages = g_list_append 
	    (tx_context->messages, 
	     gzochid_channel_payload_message_new 
	     (context, GZOCHID_CHANNEL_MESSAGE_SEND, session_oid_str,
	      send_op->message, send_op->len));
	  
	  iter = g_sequence_iter_next (iter);
	}
      else 
	{
	  GSequenceIter *old_iter = iter;

	  iter = g_sequence_iter_next (iter);

	  gzochid_warning 
	    ("Client not found for messaged channel session '%s'; removing.", 
	     session_oid_str);
	  g_sequence_remove (old_iter);
	  channel_modified = TRUE;
	}

	g_mutex_unlock (&context->client_mapping_lock);
    }

  if (channel_modified)
    gzochid_data_mark 
      (context, &gzochid_channel_serialization, channel, NULL);
}

static void
join_channel (gzochid_application_context *context,
	      gzochid_auth_identity *identity, gpointer data)
{
  GError *err = NULL;
  gzochid_channel_pending_membership_operation *member_op = data;
  gzochid_channel_pending_operation *op = 
    (gzochid_channel_pending_operation *) member_op;

  gzochid_channel *channel = NULL;
  gzochid_client_session *session = NULL;

  gzochid_data_managed_reference *channel_reference =
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, op->target_channel);
  gzochid_data_managed_reference *session_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, member_op->target_session);
  gzochid_channel_side_effects_transaction_context *tx_context = NULL;

  char *session_oid_str = NULL;
  GSequenceIter *iter = NULL;

  gzochid_data_dereference (channel_reference, &err);
  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  gzochid_data_dereference (session_reference, &err);
  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  channel = channel_reference->obj;
  session = session_reference->obj;

  session_oid_str = mpz_get_str (NULL, 16, session_reference->oid);
  iter = g_sequence_lookup 
    (channel->sessions, session_oid_str, gzochid_util_string_data_compare, 
     NULL);

  if (iter == NULL)
    {
      tx_context = join_side_effects_transaction (context, channel);

      g_sequence_insert_sorted 
	(channel->sessions, session_oid_str, gzochid_util_string_data_compare,
	 NULL);
      g_sequence_insert_sorted
	(session->channels, mpz_get_str (NULL, 16, channel_reference->oid), 
	 gzochid_util_string_data_compare, NULL);
      tx_context->messages = g_list_append 
	(tx_context->messages, gzochid_channel_message_new 
	 (context, GZOCHID_CHANNEL_MESSAGE_JOIN, session_oid_str));
      
      gzochid_data_mark 
	(context, &gzochid_channel_serialization, channel, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return;
	}

      gzochid_data_mark 
	(context, &gzochid_client_session_serialization, session, NULL);

      return;
    }
  else free (session_oid_str);
}

static void
leave_channel (gzochid_application_context *context,
	       gzochid_auth_identity *identity, gpointer data)
{
  GError *err = NULL;
  gzochid_channel_pending_membership_operation *member_op = data;
  gzochid_channel_pending_operation *op = 
    (gzochid_channel_pending_operation *) member_op;

  gzochid_channel *channel = NULL;
  gzochid_client_session *session = NULL;

  gzochid_data_managed_reference *channel_reference =
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, op->target_channel);
  gzochid_data_managed_reference *session_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, member_op->target_session);
  gzochid_channel_side_effects_transaction_context *tx_context = NULL;

  char *session_oid_str = NULL;
  GSequenceIter *iter = NULL;

  gzochid_data_dereference (channel_reference, &err);
  if (err != NULL)
    {
      g_error_free (err);
      return;
    }
 
  gzochid_data_dereference (session_reference, &err);
  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  channel = channel_reference->obj;
  session = session_reference->obj;

  session_oid_str = mpz_get_str (NULL, 16, session_reference->oid);
  iter = g_sequence_lookup 
    (channel->sessions, session_oid_str, gzochid_util_string_data_compare, 
     NULL);

  if (iter != NULL)
    {
      char *channel_oid_str = mpz_get_str (NULL, 16, channel_reference->oid);

      tx_context = join_side_effects_transaction (context, channel);

      g_sequence_remove (iter);
      iter = g_sequence_lookup
	(session->channels, channel_oid_str, 
	 gzochid_util_string_data_compare, NULL);
      g_sequence_remove (iter);
      
      tx_context->messages = g_list_append 
	(tx_context->messages, gzochid_channel_message_new 
	 (context, GZOCHID_CHANNEL_MESSAGE_LEAVE, session_oid_str));
      
      free (channel_oid_str);
      
      gzochid_data_mark 
	(context, &gzochid_channel_serialization, channel, &err);
      if (err != NULL)
	{
	  g_error_free (err);
	  return;
	}
      gzochid_data_mark 
	(context, &gzochid_client_session_serialization, session, NULL);
    }
  else free (session_oid_str);
}

/* Creates a retryable `gzochid_application_task' to execute the specified
   worker, which represents some channel-related operation, and then finally 
   free the operation as a cleanup step. (The result is then wrapped again as a
   `gzochid_task' that may be submitted directly to the scheduler.) */

static gzochid_task *
create_transactional_channel_operation_task
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gzochid_application_worker worker, gpointer data, 
 struct timeval target_execution_time)
{
  gzochid_application_task *task = gzochid_application_task_new
    (context, identity, worker, data);
  gzochid_application_task *cleanup_task =
    gzochid_application_task_new
    (context, identity, free_operation_worker, data);
  
  gzochid_transactional_application_task_execution *execution = 
    gzochid_transactional_application_task_execution_new
    (task, NULL, cleanup_task);
  
  gzochid_application_task *application_task = 
    gzochid_application_task_new
    (context, identity, 
     gzochid_application_resubmitting_transactional_task_worker, execution);

  /* Not necessary to hold a ref to these, as we've transferred them to the
     execution. */
  
  gzochid_application_task_unref (task);
  gzochid_application_task_unref (cleanup_task);
  
  return gzochid_task_new
    (gzochid_application_task_thread_worker, application_task, 
     target_execution_time);
}

/* Returns a `gzochid_task' that executes the specified channel operation
   transactionally. */

static gzochid_task *
create_channel_operation_task
(gzochid_channel_pending_operation *op, gzochid_application_context *context)
{
  gzochid_task *task = NULL;

  struct timeval now;

  gzochid_auth_identity *identity = gzochid_auth_identity_from_name
    (context->identity_cache, "[SYSTEM]");

  gettimeofday (&now, NULL);

  switch (op->type)
    {
    case GZOCHID_CHANNEL_OP_CLOSE: 
      task = create_transactional_channel_operation_task	
	(context, identity, close_channel, op, now);

      break;

    case GZOCHID_CHANNEL_OP_SEND:
      task = create_transactional_channel_operation_task	
	(context, identity, send_channel_message, op, now);

      break;

    case GZOCHID_CHANNEL_OP_LEAVE: 
      task = create_transactional_channel_operation_task	
	(context, identity, leave_channel, op, now);

      break;

    case GZOCHID_CHANNEL_OP_JOIN: 
      task = create_transactional_channel_operation_task	
	(context, identity, join_channel, op, now);

      break;
    }

  return task;
}

struct _gzochid_channel_transaction_context 
{
  gzochid_application_context *context;
  GList *operations;
};

typedef struct _gzochid_channel_transaction_context
gzochid_channel_transaction_context;

static gzochid_channel_pending_operation *
create_close_operation (mpz_t channel_oid)
{
  gzochid_channel_pending_operation *operation = malloc 
    (sizeof (gzochid_channel_pending_operation));

  operation->type = GZOCHID_CHANNEL_OP_CLOSE;
  mpz_init (operation->target_channel);
  mpz_set (operation->target_channel, channel_oid);

  return operation;
}

static gzochid_channel_pending_send_operation *
create_send_operation (mpz_t channel_oid, unsigned char *message, short len)
{
  gzochid_channel_pending_send_operation *send_operation = malloc 
    (sizeof (gzochid_channel_pending_send_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) send_operation;

  operation->type = GZOCHID_CHANNEL_OP_SEND;
  send_operation->message = malloc (sizeof (unsigned char) * len);

  memcpy (send_operation->message, message, len);
  
  send_operation->len = len;
  mpz_init (operation->target_channel);
  mpz_set (operation->target_channel, channel_oid);

  return send_operation;
}

static gzochid_channel_pending_membership_operation *
create_join_operation (mpz_t channel_oid, mpz_t session_oid)
{
  gzochid_channel_pending_membership_operation *join_operation = malloc 
    (sizeof (gzochid_channel_pending_membership_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) join_operation;

  operation->type = GZOCHID_CHANNEL_OP_JOIN;
  mpz_init (operation->target_channel);
  mpz_init (join_operation->target_session);
  mpz_set (operation->target_channel, channel_oid);
  mpz_set (join_operation->target_session, session_oid);

  return join_operation;
}

static gzochid_channel_pending_membership_operation *
create_leave_operation (mpz_t channel_oid, mpz_t session_oid)
{
  gzochid_channel_pending_membership_operation *leave_operation = malloc 
    (sizeof (gzochid_channel_pending_membership_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) leave_operation;

  operation->type = GZOCHID_CHANNEL_OP_LEAVE;
  mpz_init (operation->target_channel);
  mpz_init (leave_operation->target_session);
  mpz_set (operation->target_channel, channel_oid);
  mpz_set (leave_operation->target_session, session_oid);

  return leave_operation;
}

static gzochid_channel_transaction_context *
create_transaction_context (gzochid_application_context *context)
{
  gzochid_channel_transaction_context *tx_context = 
    calloc (1, sizeof (gzochid_channel_transaction_context));

  tx_context->context = context;

  return tx_context;
}

static int
channel_prepare (gpointer data)
{
  return TRUE;
}

static void
cleanup_transaction (gzochid_channel_transaction_context *tx_context,
		     gboolean free_operations)
{
  if (free_operations)
    g_list_free_full (tx_context->operations, (GDestroyNotify) free_operation);
  else g_list_free (tx_context->operations);

  free (tx_context);
}

static void
channel_commit (gpointer data)
{
  gzochid_channel_transaction_context *tx_context = data;
  gzochid_application_context *app_context = tx_context->context;
  gzochid_game_context *game_context =
    (gzochid_game_context *) ((gzochid_context *) app_context)->parent;

  GList *task_chain = NULL;
  GList *operation_ptr = tx_context->operations;

  while (operation_ptr != NULL)
    {
      task_chain = g_list_append 
	(task_chain, 
	 create_channel_operation_task (operation_ptr->data, app_context));
      operation_ptr = operation_ptr->next;
    }

  gzochid_schedule_submit_task_chain (game_context->task_queue, task_chain);
  g_list_free_full (task_chain, (GDestroyNotify) gzochid_task_free);

  cleanup_transaction (tx_context, FALSE);
}

static void
channel_rollback (gpointer tx_context)
{
  cleanup_transaction (tx_context, TRUE);
}

static gzochid_transaction_participant channel_participant =
  { "channel", channel_prepare, channel_commit, channel_rollback };

static gpointer 
deserialize_channel (gzochid_application_context *context, GString *in,
		     GError **err)
{
  gzochid_channel *channel = calloc (1, sizeof (gzochid_channel));

  channel->name = gzochid_util_deserialize_string (in);
  channel->id = gzochid_util_deserialize_bytes (in, (int *) &channel->id_len);

  channel->sessions = gzochid_util_deserialize_sequence 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string, free);

  gzochid_util_deserialize_mpz (in, channel->oid);
  gzochid_util_deserialize_mpz (in, channel->scm_oid);

  return channel;
}

static void 
serialize_channel (gzochid_application_context *context, gpointer obj,
		   GString *out, GError **err)
{
  gzochid_channel *channel = obj;
  gzochid_util_serialize_string (channel->name, out);

  gzochid_util_serialize_bytes (channel->id, channel->id_len, out);
  
  gzochid_util_serialize_sequence
    (channel->sessions, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);

  gzochid_util_serialize_mpz (channel->oid, out);
  gzochid_util_serialize_mpz (channel->scm_oid, out);
}

static void finalize_channel
(gzochid_application_context *context, gpointer obj)
{
  gzochid_channel *channel = obj;
  
  free (channel->name);
  free (channel->id);
  g_sequence_free (channel->sessions);

  mpz_clear (channel->oid);
  mpz_clear (channel->scm_oid);

  free (channel);
}

gzochid_io_serialization gzochid_channel_serialization =
  { serialize_channel, deserialize_channel, finalize_channel };

gzochid_channel *
gzochid_channel_new (char *name)
{
  gzochid_channel *channel = calloc (1, sizeof (gzochid_channel));

  channel->name = name;
  channel->sessions = g_sequence_new (free);
  mpz_init (channel->oid);
  mpz_init (channel->scm_oid);

  return channel;
}

void
gzochid_channel_free (gzochid_channel *channel)
{
  mpz_clear (channel->oid);
  mpz_clear (channel->scm_oid);
  g_sequence_free (channel->sessions);
  free (channel);
}

static gzochid_channel_transaction_context *
join_transaction (gzochid_application_context *context)
{
  gzochid_channel_transaction_context *tx_context = NULL;
  if (!gzochid_transaction_active()
      || gzochid_transaction_context (&channel_participant) == NULL)
    {
      tx_context = create_transaction_context (context);
      gzochid_transaction_join (&channel_participant, tx_context);
    }
  else tx_context = gzochid_transaction_context (&channel_participant);

  return tx_context;
}

static char *
make_channel_binding (char *name)
{
  int prefix_len = strlen (CHANNEL_PREFIX);
  int name_len = strlen (name) + 1;
  char *binding = malloc (sizeof (char) * (prefix_len + name_len));

  strncpy (binding, CHANNEL_PREFIX, prefix_len);
  strncpy (binding + prefix_len, name, name_len);

  return binding;
}

gzochid_channel *
gzochid_channel_create (gzochid_application_context *context, char *name)
{
  GError *err = NULL;
  char *binding = make_channel_binding (name);
  gzochid_channel *channel = gzochid_channel_new (name);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel);
  gzochid_data_managed_reference *scm_reference = NULL;
  SCM scm_channel = SCM_BOOL_F;
  
  mpz_set (channel->oid, reference->oid);
  scm_channel = gzochid_scheme_create_channel (channel, reference->oid);
  scm_reference = gzochid_data_create_reference
    (context, &gzochid_scheme_data_serialization, scm_channel);
  mpz_set (channel->scm_oid, scm_reference->oid);
  
  channel->id = (unsigned char *) mpz_get_str (NULL, 16, channel->oid);
  channel->id_len = strlen ((char *) channel->id);

  gzochid_data_set_binding_to_oid (context, binding, reference->oid, &err);
  if (err != NULL)
    {
      free (binding);
      return NULL;
    }
  else 
    {
      free (binding);
      return channel;
    }
}

gzochid_channel *
gzochid_channel_get (gzochid_application_context *context, char *name)
{
  char *binding = make_channel_binding (name);
  gzochid_channel *channel = gzochid_data_get_binding
    (context, binding, &gzochid_channel_serialization, NULL);

  free (binding);

  return channel;
}

void
gzochid_channel_join (gzochid_application_context *context,
		      gzochid_channel *channel, 
		      gzochid_client_session *session)
{
  gzochid_data_managed_reference *channel_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel);
  gzochid_data_managed_reference *session_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_join_operation (channel_reference->oid, session_reference->oid));
}

void
gzochid_channel_leave (gzochid_application_context *context,
		       gzochid_channel *channel, 
		       gzochid_client_session *session)
{
  gzochid_data_managed_reference *channel_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel);
  gzochid_data_managed_reference *session_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_leave_operation (channel_reference->oid, session_reference->oid));
}

void
gzochid_channel_send (gzochid_application_context *context,
		      gzochid_channel *channel, unsigned char *message,
		      short len)
{
  gzochid_data_managed_reference *channel_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel);
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  tx_context->operations = g_list_append 
    (tx_context->operations,
     create_send_operation (channel_reference->oid, message, len));
}

void
gzochid_channel_close (gzochid_application_context *context,
		       gzochid_channel *channel)
{
  gzochid_data_managed_reference *channel_reference = 
    gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel);
  gzochid_channel_transaction_context *tx_context = join_transaction (context);
  
  tx_context->operations = g_list_append 
    (tx_context->operations, create_close_operation (channel_reference->oid));
}
