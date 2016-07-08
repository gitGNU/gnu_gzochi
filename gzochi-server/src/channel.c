/* channel.c: Channel management routines for gzochid
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

struct _gzochid_channel
{
  char *name;


  mpz_t oid;
  mpz_t scm_oid;
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

struct _gzochid_channel_side_effect
{
  enum gzochid_channel_operation op;
  char *channel_oid_str;
};

typedef struct _gzochid_channel_side_effect gzochid_channel_side_effect;

struct _gzochid_channel_membership_side_effect
{
  gzochid_channel_side_effect base;
  
  char *session_oid_str;
};

typedef struct _gzochid_channel_membership_side_effect
gzochid_channel_membership_side_effect;

struct _gzochid_channel_message_side_effect
{
  gzochid_channel_side_effect base;
  
  GPtrArray *session_oid_strs;
  unsigned char *msg;
  short len;
};

typedef struct _gzochid_channel_message_side_effect
gzochid_channel_message_side_effect;

static gzochid_channel_side_effect *
gzochid_channel_side_effect_new (enum gzochid_channel_operation op,
				 char *channel_oid_str)
{
  gzochid_channel_side_effect *side_effect =
    malloc (sizeof (gzochid_channel_side_effect));

  assert (op == GZOCHID_CHANNEL_OP_CLOSE);
  
  side_effect->op = op;
  side_effect->channel_oid_str = channel_oid_str;

  return side_effect;
}

static gzochid_channel_side_effect *
gzochid_channel_membership_side_effect_new (enum gzochid_channel_operation op,
					    char *channel_oid_str,
					    char *session_oid_str)
{
  gzochid_channel_membership_side_effect *membership_side_effect =
    malloc (sizeof (gzochid_channel_membership_side_effect));
  gzochid_channel_side_effect *side_effect = (gzochid_channel_side_effect *)
    membership_side_effect;

  assert (op == GZOCHID_CHANNEL_OP_JOIN || op == GZOCHID_CHANNEL_OP_LEAVE);
  
  side_effect->op = op;
  side_effect->channel_oid_str = channel_oid_str;

  membership_side_effect->session_oid_str = session_oid_str;
  
  return side_effect;
}

static gzochid_channel_side_effect *
gzochid_channel_message_side_effect_new (enum gzochid_channel_operation op,
					 char *channel_oid_str,
					 GPtrArray *session_oid_strs,
					 unsigned char *msg, short len)
{
  gzochid_channel_message_side_effect *message_side_effect =
    malloc (sizeof (gzochid_channel_message_side_effect));
  gzochid_channel_side_effect *side_effect = (gzochid_channel_side_effect *)
    message_side_effect;

  assert (op == GZOCHID_CHANNEL_OP_SEND);
  
  side_effect->op = op;
  side_effect->channel_oid_str = channel_oid_str;

  message_side_effect->session_oid_strs = g_ptr_array_ref (session_oid_strs);
  message_side_effect->msg = malloc (sizeof (char) * len);
  message_side_effect->len = len;
  
  memcpy (message_side_effect->msg, msg, len);
  
  return side_effect;
}

struct _gzochid_channel_side_effect_transaction_context 
{
  gzochid_application_context *app_context;
  gzochid_channel_side_effect *side_effect;
};

typedef struct _gzochid_channel_side_effect_transaction_context 
gzochid_channel_side_effect_transaction_context;

static gzochid_channel_side_effect_transaction_context *
create_side_effect_transaction_context
(gzochid_application_context *app_context)
{
  gzochid_channel_side_effect_transaction_context *tx_context = malloc
    (sizeof (gzochid_channel_side_effect_transaction_context));

  tx_context->app_context = app_context;
  tx_context->side_effect = NULL;
  
  return tx_context;
}

static int
channel_side_effect_prepare (gpointer data)
{
  return TRUE;
}

static void
free_side_effect (gpointer data)
{
  gzochid_channel_side_effect *side_effect = data;

  if (side_effect->op == GZOCHID_CHANNEL_OP_SEND)
    {
      gzochid_channel_message_side_effect *message_side_effect =
	(gzochid_channel_message_side_effect *) side_effect;

      g_ptr_array_unref (message_side_effect->session_oid_strs);
      free (message_side_effect->msg);
    }
  else if (side_effect->op == GZOCHID_CHANNEL_OP_JOIN
      || side_effect->op == GZOCHID_CHANNEL_OP_LEAVE)
    {
      gzochid_channel_membership_side_effect *membership_side_effect =
	(gzochid_channel_membership_side_effect *) side_effect;

      free (membership_side_effect->session_oid_str);
    }
  
  free (side_effect->channel_oid_str);
  free (side_effect);
}

static void
cleanup_side_effect_transaction
(gzochid_channel_side_effect_transaction_context *tx_context)
{
  if (tx_context->side_effect != NULL)
    free_side_effect (tx_context->side_effect);
  free (tx_context);
}

static void
channel_side_effect_commit (gpointer data)
{
  gzochid_channel_side_effect_transaction_context *tx_context = data;

  if (tx_context->side_effect != NULL)
    {
      GSequence *sessions = NULL;

      g_mutex_lock (&tx_context->app_context->channel_mapping_lock);

      sessions = g_hash_table_lookup
	(tx_context->app_context->channel_oids_to_local_session_oids,
	 tx_context->side_effect->channel_oid_str);

      if (sessions == NULL)
	{
	  sessions = g_sequence_new (free);

	  g_hash_table_insert
	    (tx_context->app_context->channel_oids_to_local_session_oids,
	     strdup (tx_context->side_effect->channel_oid_str), sessions);
	}
      
      if (tx_context->side_effect->op == GZOCHID_CHANNEL_OP_SEND)
	{
	  int i = 0;
	  gzochid_channel_message_side_effect *message_side_effect =
	    (gzochid_channel_message_side_effect *) tx_context->side_effect;

	  g_mutex_lock (&tx_context->app_context->client_mapping_lock);
	  
	  for (; i < message_side_effect->session_oid_strs->len; i++)
	    {
	      char *session_oid_str = g_ptr_array_index
		(message_side_effect->session_oid_strs, i);
	      gzochid_game_client *client = g_hash_table_lookup
		(tx_context->app_context->oids_to_clients, session_oid_str);

	      if (client != NULL)
		{
		  gzochid_application_event_dispatch
		    (tx_context->app_context->event_source,
		     gzochid_application_event_new (MESSAGE_SENT));
		  gzochid_game_client_send
		    (client, message_side_effect->msg,
		     message_side_effect->len);
		}
	      else
		{
		  GSequenceIter *iter = g_sequence_lookup
		    (sessions, session_oid_str,
		     gzochid_util_string_data_compare, NULL);
		  
		  if (iter != NULL)
		    {
		      g_debug 
			("Client not found for messaged channel session '%s'; "
			 "removing.", session_oid_str);

		      g_sequence_remove (iter);
		    }
		}
	    }
	  
	  g_mutex_unlock (&tx_context->app_context->client_mapping_lock);
	}
      else
	{
	  if (tx_context->side_effect->op == GZOCHID_CHANNEL_OP_JOIN
	      || tx_context->side_effect->op == GZOCHID_CHANNEL_OP_LEAVE)
	    {
	      gzochid_channel_membership_side_effect *membership_side_effect =
		(gzochid_channel_membership_side_effect *)
		tx_context->side_effect;

	      GSequenceIter *iter = g_sequence_lookup
 		(sessions, membership_side_effect->session_oid_str,
		 gzochid_util_string_data_compare, NULL);
	      
	      if (tx_context->side_effect->op == GZOCHID_CHANNEL_OP_JOIN)
		{
		  if (iter == NULL)
		    g_sequence_insert_sorted
		      (sessions,
		       strdup (membership_side_effect->session_oid_str),
		       gzochid_util_string_data_compare, NULL);
		}
	      else if (iter != NULL)
		g_sequence_remove (iter);
	    }
	  else g_hash_table_remove
		 (tx_context->app_context->channel_oids_to_local_session_oids,
		  tx_context->side_effect->channel_oid_str);	  
	}

      g_mutex_unlock (&tx_context->app_context->channel_mapping_lock);
    }

  cleanup_side_effect_transaction (tx_context);
}

static void
channel_side_effect_rollback (gpointer data)
{
  cleanup_side_effect_transaction (data);
}

static gzochid_transaction_participant channel_side_effect_participant =
  { 
    "channel-side-effect", 
    channel_side_effect_prepare, 
    channel_side_effect_commit, 
    channel_side_effect_rollback 
  };

static gzochid_channel_side_effect_transaction_context *
join_side_effect_transaction (gzochid_application_context *context)
{
  gzochid_channel_side_effect_transaction_context *tx_context = NULL;

  if (!gzochid_transaction_active()
      || (gzochid_transaction_context 
	  (&channel_side_effect_participant) == NULL))
    {
      tx_context = create_side_effect_transaction_context (context);
      gzochid_transaction_join (&channel_side_effect_participant, tx_context);
    }
  else tx_context = gzochid_transaction_context
	 (&channel_side_effect_participant);

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
  gzochid_channel_pending_operation *op = data;
  char *channel_oid_str = mpz_get_str (NULL, 16, op->target_channel);

  g_mutex_lock (&context->channel_mapping_lock);

  if (g_hash_table_contains
      (context->channel_oids_to_local_session_oids, channel_oid_str))
    {
      gzochid_channel_side_effect_transaction_context *tx_context =
	join_side_effect_transaction (context);

      tx_context->side_effect = gzochid_channel_side_effect_new
	(GZOCHID_CHANNEL_OP_CLOSE, channel_oid_str);
    }
  else free (channel_oid_str);
  
  g_mutex_unlock (&context->channel_mapping_lock);
}

static void
send_channel_message (gzochid_application_context *context,
		      gzochid_auth_identity *identity, gpointer data)
{
  gzochid_channel_pending_send_operation *send_op = data;
  gzochid_channel_pending_operation *op =
    (gzochid_channel_pending_operation *) send_op;

  char *channel_oid_str = mpz_get_str (NULL, 16, op->target_channel);

  g_mutex_lock (&context->channel_mapping_lock);

  if (g_hash_table_contains
      (context->channel_oids_to_local_session_oids, channel_oid_str))
    {
      GSequence *sessions = g_hash_table_lookup
	(context->channel_oids_to_local_session_oids, channel_oid_str);
      GSequenceIter *iter = g_sequence_get_begin_iter (sessions);

      if (g_sequence_iter_is_end (iter))
	free (channel_oid_str);
      else
	{
	  GPtrArray *session_oid_strs = g_ptr_array_new ();
	  gzochid_channel_side_effect_transaction_context *tx_context =
	    join_side_effect_transaction (context);

	  while (!g_sequence_iter_is_end (iter))
	    {
	      g_ptr_array_add (session_oid_strs, g_sequence_get (iter));
	      iter = g_sequence_iter_next (iter);
	    }

	  tx_context->side_effect = gzochid_channel_message_side_effect_new
	    (GZOCHID_CHANNEL_OP_SEND, channel_oid_str, session_oid_strs,
	     send_op->message, send_op->len);
	  
	  g_ptr_array_unref (session_oid_strs);
	}
    }
  else free (channel_oid_str);
  
  g_mutex_unlock (&context->channel_mapping_lock);
}

static void
join_channel (gzochid_application_context *context,
	      gzochid_auth_identity *identity, gpointer data)
{
  gzochid_channel_pending_membership_operation *member_op = data;
  gzochid_channel_pending_operation *op = 
    (gzochid_channel_pending_operation *) member_op;

  gboolean do_join = TRUE;

  char *channel_oid_str = mpz_get_str (NULL, 16, op->target_channel);
  char *session_oid_str = mpz_get_str (NULL, 16, member_op->target_session);
  
  g_mutex_lock (&context->channel_mapping_lock);

  if (g_hash_table_contains
      (context->channel_oids_to_local_session_oids, channel_oid_str))
    {
      GSequence *sessions = g_hash_table_lookup
	(context->channel_oids_to_local_session_oids, channel_oid_str);
      GSequenceIter *iter = g_sequence_lookup
	(sessions, session_oid_str, gzochid_util_string_data_compare, NULL);

      do_join = iter == NULL;
    }

  g_mutex_unlock (&context->channel_mapping_lock);

  if (do_join)
    {
      gzochid_channel_side_effect_transaction_context *tx_context =
	join_side_effect_transaction (context);

      tx_context->side_effect = gzochid_channel_membership_side_effect_new
	(GZOCHID_CHANNEL_OP_JOIN, channel_oid_str,
	 mpz_get_str (NULL, 16, member_op->target_session));
    }
  else
    {
      free (channel_oid_str);
      free (session_oid_str);
    }
}

static void
leave_channel (gzochid_application_context *context,
	       gzochid_auth_identity *identity, gpointer data)
{
  gzochid_channel_pending_membership_operation *member_op = data;
  gzochid_channel_pending_operation *op = 
    (gzochid_channel_pending_operation *) member_op;

  gboolean do_leave = FALSE;

  char *channel_oid_str = mpz_get_str (NULL, 16, op->target_channel);
  char *session_oid_str = mpz_get_str (NULL, 16, member_op->target_session);
  
  g_mutex_lock (&context->channel_mapping_lock);

  if (g_hash_table_contains
      (context->channel_oids_to_local_session_oids, channel_oid_str))
    {
      GSequence *sessions = g_hash_table_lookup
	(context->channel_oids_to_local_session_oids, channel_oid_str);
      GSequenceIter *iter = g_sequence_lookup
	(sessions, session_oid_str, gzochid_util_string_data_compare, NULL);

      do_leave = iter != NULL;
    }

  g_mutex_unlock (&context->channel_mapping_lock);

  if (do_leave)
    {
      gzochid_channel_side_effect_transaction_context *tx_context =
	join_side_effect_transaction (context);

      tx_context->side_effect = gzochid_channel_membership_side_effect_new
	(GZOCHID_CHANNEL_OP_LEAVE, channel_oid_str,
	 mpz_get_str (NULL, 16, member_op->target_session));
    }
  else
    {
      free (channel_oid_str);
      free (session_oid_str);
    }
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

  gzochid_util_serialize_mpz (channel->oid, out);
  gzochid_util_serialize_mpz (channel->scm_oid, out);
}

static void finalize_channel
(gzochid_application_context *context, gpointer obj)
{
  gzochid_channel *channel = obj;
  
  free (channel->name);

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

  mpz_init (channel->oid);
  mpz_init (channel->scm_oid);

  return channel;
}

void
gzochid_channel_free (gzochid_channel *channel)
{
  mpz_clear (channel->oid);
  mpz_clear (channel->scm_oid);

  free (channel);
}

void
gzochid_channel_scm_oid (gzochid_channel *channel, mpz_t scm_oid)
{
  mpz_set (scm_oid, channel->scm_oid);
}

const char *
gzochid_channel_name (gzochid_channel *channel)
{
  return channel->name;
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
  char *binding = NULL;
  gzochid_channel *channel = gzochid_channel_new (name);
  gzochid_data_managed_reference *reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);
  gzochid_data_managed_reference *scm_reference = NULL;
  SCM scm_channel = SCM_BOOL_F;

  if (reference == NULL)
    {
      gzochid_channel_free (channel);
      return NULL;
    }
  
  mpz_set (channel->oid, reference->oid);
  scm_channel = gzochid_scheme_create_channel (channel, reference->oid);
  scm_reference = gzochid_data_create_reference
    (context, &gzochid_scheme_data_serialization, scm_channel, NULL);

  if (scm_reference == NULL)
    {
      gzochid_channel_free (channel);
      return NULL;
    }
  
  mpz_set (channel->scm_oid, scm_reference->oid);
  
  binding = make_channel_binding (name);
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
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */
  
  assert (channel_reference != NULL);
  
  session_reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session, NULL);

  /* The reference for the session must already be cached in the current
     transaction context. */

  assert (session_reference != NULL);
  
  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_join_operation (channel_reference->oid, session_reference->oid));
}

void
gzochid_channel_leave (gzochid_application_context *context,
		       gzochid_channel *channel, 
		       gzochid_client_session *session)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */

  assert (channel_reference != NULL);

  session_reference =  gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session, NULL);

  /* The reference for the session must already be cached in the current
     transaction context. */

  assert (session_reference != NULL);
  
  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_leave_operation (channel_reference->oid, session_reference->oid));
}

void
gzochid_channel_send (gzochid_application_context *context,
		      gzochid_channel *channel, unsigned char *message,
		      short len)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */

  assert (channel_reference != NULL);
  
  tx_context->operations = g_list_append 
    (tx_context->operations,
     create_send_operation (channel_reference->oid, message, len));
}

void
gzochid_channel_close (gzochid_application_context *context,
		       gzochid_channel *channel)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */

  assert (channel_reference != NULL);
    
  tx_context->operations = g_list_append 
    (tx_context->operations, create_close_operation (channel_reference->oid));
}
