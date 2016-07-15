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

/*
  The following data structures and functions define the gzochid container's
  channel service. As described in the manual, channels are named groups of
  client sessions that can be addressed en masse for the purpose of broadcast.
  
  The channel service provides transactional semantics for message delivery and
  membership management. There are two different "stages" of transaction 
  executed by this service. The first stage is user-initiated and provides 
  "all-or-nothing" guarantees for the scheduling of channel operations; if this
  transaction commits, all requested operations will be durably scheduled for 
  execution in serial. The second tier of transactions includes transactions
  initiated to execute each of scheduled tasks; when a channel "side effect" 
  transaction commits, the operation is executed against the current state of 
  the channel - a message being sent to all member sessions, for example. The
  initial transaction operates on the channel in the abstract, whereas the side
  effect transaction operates on the concrete state of the channel.

  Despite the transactional nature of channel interactions, channels themselves
  are ephemeral with respect to a gzochid application server node. Channel
  membership, for example, does not survive a container restart. ...Which stands
  to reason, since all members would have been disconnected. 
*/

#define CHANNEL_PREFIX "s.channel."

/* An enumeration of operation types. */

enum gzochid_channel_operation
  {
    GZOCHID_CHANNEL_OP_JOIN, /* Add a session to the channel. */
    GZOCHID_CHANNEL_OP_LEAVE, /* Remove a session from the channel. */
    GZOCHID_CHANNEL_OP_SEND, /* Send a message to all member sessions. */
    GZOCHID_CHANNEL_OP_CLOSE /* Destroy the channel, removing all members. */
  };

/* The channel structure. */

struct _gzochid_channel
{
  char *name; /* The channel name. */
  mpz_t oid; /* The object id of the channel in the object store. */
  mpz_t scm_oid; /* The object id of the channel's Scheme representation. */
};

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

/* Serialization routines for channel objects. */

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
  gzochid_channel_free (obj);
}

gzochid_io_serialization gzochid_channel_serialization =
  { serialize_channel, deserialize_channel, finalize_channel };

/* The following data structures represent the logical set of channel operations
   via a kind of "lite" polymorphism. */

/* The base operation type. Also represents a channel `close' operation. */

struct _gzochid_channel_pending_operation
{
  enum gzochid_channel_operation type; /* The operation type. */
  mpz_t target_channel; /* The target channel oid. */

  /* The operation timestamp, for schedule ordering. */
  
  struct timeval timestamp; 
};

typedef struct _gzochid_channel_pending_operation
gzochid_channel_pending_operation;

/* Represents a channel `send' operation. */

struct _gzochid_channel_pending_send_operation
{
  gzochid_channel_pending_operation base; /* Base channel operation. */
  unsigned char *message; /* The message payload. */
  short len; /* The message payload size. */
};

typedef struct _gzochid_channel_pending_send_operation
gzochid_channel_pending_send_operation;

/* Represents an operation that changes channel membership, i.e., a `join' or
   `leave' operation. */

struct _gzochid_channel_pending_membership_operation
{
  gzochid_channel_pending_operation base; /* Base channel operation. */
  mpz_t target_session; /* The target session oid. */
};

typedef struct _gzochid_channel_pending_membership_operation
gzochid_channel_pending_membership_operation;

/* Serialization routines for logical channel operations. */

static gpointer 
deserialize_channel_operation (gzochid_application_context *context,
			       GString *in, GError **err)
{
  enum gzochid_channel_operation type = gzochid_util_deserialize_int (in);
  gzochid_channel_pending_operation *op = NULL;

  /* 
     Figure out what kind of operation this is so we know how to deserialize 
     the rest of the object. 
  */
  
  if (type == GZOCHID_CHANNEL_OP_JOIN || type == GZOCHID_CHANNEL_OP_LEAVE)
    op = malloc (sizeof (gzochid_channel_pending_membership_operation));
  else if (type == GZOCHID_CHANNEL_OP_SEND)
    op = malloc (sizeof (gzochid_channel_pending_send_operation));
  else op = malloc (sizeof (gzochid_channel_pending_operation));

  op->type = type;

  mpz_init (op->target_channel);
  gzochid_util_deserialize_mpz (in, op->target_channel);

  if (type == GZOCHID_CHANNEL_OP_JOIN || type == GZOCHID_CHANNEL_OP_LEAVE)
    {
      gzochid_channel_pending_membership_operation *member_op =
	(gzochid_channel_pending_membership_operation *) op;
      
      mpz_init (member_op->target_session);
      gzochid_util_deserialize_mpz (in, member_op->target_session);
    }
  else if (type == GZOCHID_CHANNEL_OP_SEND)
    {
      gzochid_channel_pending_send_operation *send_op =
	(gzochid_channel_pending_send_operation *) op;
      
      send_op->message = gzochid_util_deserialize_bytes
	(in, (int *) &send_op->len);
    }

  return op;
}

static void 
serialize_channel_operation (gzochid_application_context *context,
			     gpointer data, GString *out, GError **err)
{
  gzochid_channel_pending_operation *op = data;

  /* Important to serialize the operation type first, so that the deserializer
     can read it before reading an operation type-specific fields. */
  
  gzochid_util_serialize_int (op->type, out);
  gzochid_util_serialize_mpz (op->target_channel, out);
  
  if (op->type == GZOCHID_CHANNEL_OP_JOIN
      || op->type == GZOCHID_CHANNEL_OP_LEAVE) {

    gzochid_channel_pending_membership_operation *member_op =
      (gzochid_channel_pending_membership_operation *) op;

    gzochid_util_serialize_mpz (member_op->target_session, out);
    
  } else if (op->type == GZOCHID_CHANNEL_OP_SEND) {

    gzochid_channel_pending_send_operation *send_op =
      (gzochid_channel_pending_send_operation *) op;

    gzochid_util_serialize_bytes (send_op->message, send_op->len, out);
  } 
}

static void
finalize_channel_operation (gzochid_application_context *context, gpointer data)
{
  gzochid_channel_pending_operation *op = data;

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

static gzochid_io_serialization gzochid_channel_operation_serialization =
  {
    serialize_channel_operation,
    deserialize_channel_operation,
    finalize_channel_operation
  };

/* The following data structures and functions make up the channel side effects
   transaction system, using the same kind of polymorphism as logical channel
   operations. */

/* The base side effect type. Also represents a channel `close' side effect. */

struct _gzochid_channel_side_effect
{
  enum gzochid_channel_operation op; /* The side effect type. */

  /* The hexadecimal string form of the channel oid; used as a key into the 
     application context's active channel map. */

  char *channel_oid_str; 
};

typedef struct _gzochid_channel_side_effect gzochid_channel_side_effect;

/* Represents a side effect that changes channel membership, i.e., a `join' or
   `leave' side effect. */

struct _gzochid_channel_membership_side_effect
{
  gzochid_channel_side_effect base; /* Base channel side effect. */
  
  /* The hexadecimal string form of the session oid; used as a key into the 
     application context's active session map. */

  char *session_oid_str;
};

typedef struct _gzochid_channel_membership_side_effect
gzochid_channel_membership_side_effect;

/* Represents a channel `send' side effect. */

struct _gzochid_channel_message_side_effect
{
  gzochid_channel_side_effect base; /* Base channel side effect. */
  
  /* A snapshot of the members of the channel, in the form of an array of hex 
     string session oids, at the time the side effect transaction was 
     executed. */

  GPtrArray *session_oid_strs;
  
  unsigned char *msg; /* The message to be sent. */
  short len; /* The message payload length. */
};

typedef struct _gzochid_channel_message_side_effect
gzochid_channel_message_side_effect;

/* Frees the memory used by the specified side effect. */

static void
free_side_effect (gzochid_channel_side_effect *side_effect)
{
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

/*
  Create and return a new channel `close' side effect. Memory should be freed
  via `free_side_effect' when no longer in use.
*/

static gzochid_channel_side_effect *
gzochid_channel_side_effect_new (enum gzochid_channel_operation op,
				 char *channel_oid_str)
{
  gzochid_channel_side_effect *side_effect =
    malloc (sizeof (gzochid_channel_side_effect));

  assert (op == GZOCHID_CHANNEL_OP_CLOSE);
  
  side_effect->op = op;
  side_effect->channel_oid_str = strdup (channel_oid_str);

  return side_effect;
}

/*
  Create and return a new channel `leave' or `join' side effect with the 
  specified target session oid. Memory should be freed via `free_side_effect' 
  when no longer in use.
*/

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
  side_effect->channel_oid_str = strdup (channel_oid_str);

  membership_side_effect->session_oid_str = strdup (session_oid_str);
  
  return side_effect;
}

/*
  Create and return a new channel `send' side effect with the 
  specified session oid array and message. Memory should be freed via 
  `free_side_effect' when no longer in use.
*/

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
  side_effect->channel_oid_str = strdup (channel_oid_str);

  message_side_effect->session_oid_strs = g_ptr_array_ref (session_oid_strs);
  message_side_effect->msg = malloc (sizeof (char) * len);
  message_side_effect->len = len;
  
  memcpy (message_side_effect->msg, msg, len);
  
  return side_effect;
}

/* The transaction participant context for channel side effect transactions. */

struct _gzochid_channel_side_effect_transaction_context 
{
  /* The gzochi game application context to which the channel belongs. */
  
  gzochid_application_context *app_context;

  /* The channel side effect to be executed on commit. There can only be one
     side effect per side effect transaction. */
  
  gzochid_channel_side_effect *side_effect;
};

typedef struct _gzochid_channel_side_effect_transaction_context 
gzochid_channel_side_effect_transaction_context;

static void
cleanup_side_effect_transaction
(gzochid_channel_side_effect_transaction_context *tx_context)
{
  if (tx_context->side_effect != NULL)
    free_side_effect (tx_context->side_effect);
  free (tx_context);
}

/* The `prepare' function for a side effects transaction. Effectively a 
   no-op. */

static int
channel_side_effect_prepare (gpointer data)
{
  return TRUE;
}

/* Makes the channel side effect permanent with respect to the current state of
   the channel on this application server node. */

static void
channel_side_effect_commit (gpointer data)
{
  gzochid_channel_side_effect_transaction_context *tx_context = data;

  if (tx_context->side_effect != NULL)
    {
      GSequence *sessions = NULL;

      /* Locking the channel map; no matter what kind of side effect is being
	 processed, it'll require exclusive access to the channel. */
      
      g_mutex_lock (&tx_context->app_context->channel_mapping_lock);

      /* Does the channel have any member sessions on this node? */
      
      sessions = g_hash_table_lookup
	(tx_context->app_context->channel_oids_to_local_session_oids,
	 tx_context->side_effect->channel_oid_str);

      if (sessions == NULL)
	{
	  /* If not, that's okay; it may not have been used on this node. Create
	     an empty sequence to store new members. */
	  
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

	      /* Grab the client connection to which the session oid 
		 corresponds. */
	      
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
		  /* If no client was found for the specified session, remove it
		     from the set of local session oids so we don't waste time 
		     on it again; a conveniently lazy way of purging old 
		     members. */
		  
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
      else if (tx_context->side_effect->op == GZOCHID_CHANNEL_OP_JOIN
	       || tx_context->side_effect->op == GZOCHID_CHANNEL_OP_LEAVE)
	{
	  gzochid_channel_membership_side_effect *membership_side_effect =
	    (gzochid_channel_membership_side_effect *)
	    tx_context->side_effect;
	  
	  GSequenceIter *iter = g_sequence_lookup
	    (sessions, membership_side_effect->session_oid_str,
	     gzochid_util_string_data_compare, NULL);

	  /* Add or remove the session to or from the channel's session list. */
	  
	  if (tx_context->side_effect->op == GZOCHID_CHANNEL_OP_JOIN)
	    {
	      if (iter == NULL)
		g_sequence_insert_sorted
		  (sessions, strdup (membership_side_effect->session_oid_str),
		   gzochid_util_string_data_compare, NULL);
	    }
	  else if (iter != NULL)
	    g_sequence_remove (iter);
	}

      /* Shut the channel down and free everything. */
      
      else g_hash_table_remove
	     (tx_context->app_context->channel_oids_to_local_session_oids,
	      tx_context->side_effect->channel_oid_str);	  

      g_mutex_unlock (&tx_context->app_context->channel_mapping_lock);
    }

  cleanup_side_effect_transaction (tx_context);
}

/* Abort the current transaction, discarding any pending side effect. */

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

/* Join the current general transaction, initiating one if there isn't one 
   already active, and bind an empty side effect transactional state to it. (Or,
   if we've already joined the transaction, just return the existing 
   transactional state. */

static gzochid_channel_side_effect_transaction_context *
join_side_effect_transaction (gzochid_application_context *app_context)
{
  gzochid_channel_side_effect_transaction_context *tx_context = NULL;

  if (!gzochid_transaction_active()
      || (gzochid_transaction_context 
	  (&channel_side_effect_participant) == NULL))
    {
      tx_context =
	malloc (sizeof (gzochid_channel_side_effect_transaction_context));
      
      tx_context->app_context = app_context;
      tx_context->side_effect = NULL;
  
      gzochid_transaction_join (&channel_side_effect_participant, tx_context);
    }
  else tx_context = gzochid_transaction_context
	 (&channel_side_effect_participant);

  return tx_context;
}

/* Close the target channel as a side effect of the current transaction. */

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

/* Send a message to the target channel's members as a side effect of the 
   current transaction. */

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

      /* Take a snapshot of the current membership of the channel. */
      
      if (g_sequence_iter_is_end (iter))

	/* ...which might be empty. */
	
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

/* Add the target session to the target channel as a side effect of the current
   transaction. */

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

/* Remove the target session to the target channel as a side effect from the 
   current transaction. */

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

/* The following data structures and functions make up the logical channel
   operation transaction system. */

struct _gzochid_channel_transaction_context 
{
  /* The gzochi game application context to which the channel belongs. */
  
  gzochid_application_context *context;

  /* A managed reference to the queue of pending channel operations to be 
     scheduled. */
  
  gzochid_data_managed_reference *queue_ref; 
};

typedef struct _gzochid_channel_transaction_context
gzochid_channel_transaction_context;

static int
channel_prepare (gpointer data)
{
  return TRUE;
}

/* The logical channel operation transaction participant doesn't actually need
   specialized commit and rollback processes; the queue of operations is 
   scheduled and persisted at the beginning of the transaction and can be 
   appended to for the duration of the transaction. If the general transaction
   commits, the queue and its contents will be persisted and scheduled; if the
   general transaction rolls back, it'll be cleaned up and forgotten. To put it
   another way, this transactional service achieves its outcomes via other 
   transational services. */

static gzochid_transaction_participant channel_participant =
  { "channel", channel_prepare, free, free };

static gzochid_channel_transaction_context *
join_transaction (gzochid_application_context *context)
{
  gzochid_channel_transaction_context *tx_context = NULL;
  if (!gzochid_transaction_active()
      || gzochid_transaction_context (&channel_participant) == NULL)
    {
      gzochid_data_managed_reference *queue_ref = gzochid_data_create_reference
	(context, &gzochid_durable_queue_serialization,
	 gzochid_durable_queue_new (context), NULL);

      /* The reference to the queue may be `NULL' if the data transaction 
	 deadlocks or times out. If that happens, this function shouldn't join
	 the general transaction will return `NULL' itself. */
      
      if (queue_ref != NULL)
	{
	  tx_context = malloc (sizeof (gzochid_channel_transaction_context));

	  tx_context->context = context;
	  tx_context->queue_ref = queue_ref;

	  gzochid_transaction_join (&channel_participant, tx_context);

	  /* Schedule the task chain even though it's currently empty; it'll
	     probably have tasks added to it before the transaction commits. */
	  
	  gzochid_schedule_durable_task_chain
	    (context, gzochid_auth_system_identity (),
	     tx_context->queue_ref->obj, NULL);
	}
    }
  else tx_context = gzochid_transaction_context (&channel_participant);

  return tx_context;
}

/*
  Returns a newly-allocated string containing the binding name for the
  specified channel name: s.channel.[name]
  
  Free this string with `free' when no longer in use.
*/

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
      g_debug ("Failed to persist struct for new channel '%s'.", name);
      
      /* If the reference can't be created (because the transaction's in a bad
	 state) free the memory used by the not-yet-persisted channel struct. */
      
      gzochid_channel_free (channel);
      return NULL;
    }
  
  mpz_set (channel->oid, reference->oid);
  scm_channel = gzochid_scheme_create_channel (channel, reference->oid);
  scm_reference = gzochid_data_create_reference
    (context, &gzochid_scheme_data_serialization, scm_channel, NULL);

  if (scm_reference == NULL)
    {
      g_debug
	("Failed to create Scheme representation of new channel '%s'.", name);
      return NULL;
    }
  
  mpz_set (channel->scm_oid, scm_reference->oid);
  
  binding = make_channel_binding (name);
  gzochid_data_set_binding_to_oid (context, binding, reference->oid, &err);

  if (err != NULL)
    {
      g_debug ("Failed to create binding for channel: %s", err->message);
      g_error_free (err);

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

/* The logical channel operation task worker; a dispatcher for the individual
   per-operation worker functions. */

static void
channel_operation_worker (gzochid_application_context *context,
			  gzochid_auth_identity *identity, gpointer data)
{
  gzochid_channel_pending_operation *op = data;
  gzochid_data_managed_reference *op_reference =
    gzochid_data_create_reference
    (context, &gzochid_channel_operation_serialization, op, NULL);

  if (op_reference == NULL)
    return;
  
  switch (op->type)
    {
    case GZOCHID_CHANNEL_OP_JOIN: join_channel (context, identity, data); break;

    case GZOCHID_CHANNEL_OP_LEAVE:
      leave_channel (context, identity, data);
      break;

    case GZOCHID_CHANNEL_OP_SEND:
      send_channel_message (context, identity, data);
      break;
      
    case GZOCHID_CHANNEL_OP_CLOSE:
      close_channel (context, identity, data);
      break;
      
    default: assert (1 == 0);
    }

  gzochid_data_remove_object (op_reference, NULL);
}

/* Durable task serialization boilerplate for logical channel operation
   transactional tasks. */
			  
static void
serialize_channel_operation_worker (gzochid_application_context *context,
				    gzochid_application_worker worker,
				    GString *out)
{
}

static gzochid_application_worker
deserialize_channel_operation_worker (gzochid_application_context *context,
				      GString *in)
{
  return channel_operation_worker;
}

static gzochid_application_worker_serialization
gzochid_channel_operation_worker_serialization =
  {
    serialize_channel_operation_worker,
    deserialize_channel_operation_worker
  };

gzochid_application_task_serialization
gzochid_channel_operation_task_serialization =
  {
    "channel-operation",
    &gzochid_channel_operation_worker_serialization,
    &gzochid_channel_operation_serialization
  };

void
gzochid_channel_join (gzochid_application_context *context,
		      gzochid_channel *channel, 
		      gzochid_client_session *session)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);
  gzochid_durable_application_task_handle *task_handle = NULL;

  gzochid_channel_pending_membership_operation *join_operation = malloc 
    (sizeof (gzochid_channel_pending_membership_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) join_operation;

  if (tx_context == NULL)
    return;
  
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

  operation->type = GZOCHID_CHANNEL_OP_JOIN;
  mpz_init (operation->target_channel);
  mpz_init (join_operation->target_session);
  mpz_set (operation->target_channel, channel_reference->oid);
  mpz_set (join_operation->target_session, session_reference->oid);

  /* Create a task to execute the join... */
  
  task_handle = gzochid_create_durable_application_task_handle
    (gzochid_application_task_new
     (context, gzochid_auth_system_identity (), channel_operation_worker,
      operation),
     &gzochid_channel_operation_task_serialization, (struct timeval) { 0, 0 },
     NULL, NULL);

  /* ...and add it to the task queue. */

  if (task_handle != NULL)
    gzochid_durable_queue_offer
      (tx_context->queue_ref->obj,
       &gzochid_durable_application_task_handle_serialization,
       task_handle, NULL);
}

void
gzochid_channel_leave (gzochid_application_context *context,
		       gzochid_channel *channel, 
		       gzochid_client_session *session)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);
  gzochid_durable_application_task_handle *task_handle = NULL;

  gzochid_channel_pending_membership_operation *leave_operation = malloc 
    (sizeof (gzochid_channel_pending_membership_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) leave_operation;

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
  
  operation->type = GZOCHID_CHANNEL_OP_LEAVE;
  mpz_init (operation->target_channel);
  mpz_init (leave_operation->target_session);
  mpz_set (operation->target_channel, channel_reference->oid);
  mpz_set (leave_operation->target_session, session_reference->oid);

  /* Create a task to execute the exit from the channel... */
  
  task_handle = gzochid_create_durable_application_task_handle
    (gzochid_application_task_new
     (context, gzochid_auth_system_identity (), channel_operation_worker,
      operation),
     &gzochid_channel_operation_task_serialization, (struct timeval) { 0, 0 },
     NULL, NULL);

  /* ...and add it to the task queue. */

  if (task_handle != NULL)
    gzochid_durable_queue_offer
      (tx_context->queue_ref->obj,
       &gzochid_durable_application_task_handle_serialization,
       task_handle, NULL);
}

void
gzochid_channel_send (gzochid_application_context *context,
		      gzochid_channel *channel, unsigned char *message,
		      short len)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);
  gzochid_durable_application_task_handle *task_handle = NULL;

  gzochid_channel_pending_send_operation *send_operation = malloc 
    (sizeof (gzochid_channel_pending_send_operation));
  gzochid_channel_pending_operation *operation = 
    (gzochid_channel_pending_operation *) send_operation;

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */

  assert (channel_reference != NULL);
  
  operation->type = GZOCHID_CHANNEL_OP_SEND;
  send_operation->message = malloc (sizeof (unsigned char) * len);

  memcpy (send_operation->message, message, len);
  
  send_operation->len = len;
  mpz_init (operation->target_channel);
  mpz_set (operation->target_channel, channel_reference->oid);

  /* Create a task to execute the message broadcast... */

  task_handle = gzochid_create_durable_application_task_handle
    (gzochid_application_task_new
     (context, gzochid_auth_system_identity (), channel_operation_worker,
      operation),
     &gzochid_channel_operation_task_serialization, (struct timeval) { 0, 0 },
     NULL, NULL);

  /* ...and add it to the task queue. */

  if (task_handle != NULL)
    gzochid_durable_queue_offer
      (tx_context->queue_ref->obj,
       &gzochid_durable_application_task_handle_serialization,
       task_handle, NULL);
}

void
gzochid_channel_close (gzochid_application_context *context,
		       gzochid_channel *channel)
{
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_channel_transaction_context *tx_context = join_transaction (context);
  gzochid_durable_application_task_handle *task_handle = NULL;

  gzochid_channel_pending_operation *operation = malloc 
    (sizeof (gzochid_channel_pending_operation));

  channel_reference = gzochid_data_create_reference 
    (context, &gzochid_channel_serialization, channel, NULL);

  /* The reference for the channel must already be cached in the current
     transaction context. */

  assert (channel_reference != NULL);

  operation->type = GZOCHID_CHANNEL_OP_CLOSE;
  mpz_init (operation->target_channel);
  mpz_set (operation->target_channel, channel_reference->oid);
  
  /* Create a task to execute the channel shutdown... */

  task_handle = gzochid_create_durable_application_task_handle
    (gzochid_application_task_new
     (context, gzochid_auth_system_identity (), channel_operation_worker,
      operation),
     &gzochid_channel_operation_task_serialization, (struct timeval) { 0, 0 },
     NULL, NULL);

  /* ...and add it to the task queue. */

  if (task_handle != NULL)
    gzochid_durable_queue_offer
      (tx_context->queue_ref->obj,
       &gzochid_durable_application_task_handle_serialization,
       task_handle, NULL);
}
