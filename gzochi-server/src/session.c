/* session.c: Client session management routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <gmp.h>
#include <stdlib.h>

#include "app.h"
#include "auth.h"
#include "data.h"
#include "io.h"
#include "protocol.h"
#include "scheme.h"
#include "session.h"
#include "tx.h"
#include "util.h"

enum gzochid_client_session_operation
  {
    GZOCHID_CLIENT_SESSION_OP_DISCONNECT,
    GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS,
    GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE,
    GZOCHID_CLIENT_SESSION_OP_MESSAGE
  };

typedef struct _gzochid_client_session_pending_operation
{
  enum gzochid_client_session_operation type;
  mpz_t target_session;
} gzochid_client_session_pending_operation;

typedef struct _gzochid_client_session_pending_message_operation
{
  gzochid_client_session_pending_operation base;

  unsigned char *message;
  short len;
} gzochid_client_session_pending_message_operation;

typedef struct _gzochid_client_session_transaction_context
{
  gzochid_application_context *context;
  GList *operations;
} gzochid_client_session_transaction_context;

static gzochid_client_session_transaction_context *create_transaction_context
(gzochid_application_context *context)
{
  gzochid_client_session_transaction_context *tx_context = 
    calloc (1, sizeof (gzochid_client_session_transaction_context));

  tx_context->context = context;

  return tx_context;
}

static gzochid_client_session_pending_operation *create_disconnect_operation
(mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_DISCONNECT;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;
}

static gzochid_client_session_pending_operation *create_login_success_operation
(mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;  
}

static gzochid_client_session_pending_operation *create_login_failure_operation
(mpz_t target_session)
{
  gzochid_client_session_pending_operation *op = 
    malloc (sizeof (gzochid_client_session_pending_operation));

  op->type = GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE;
  mpz_init (op->target_session);
  mpz_set (op->target_session, target_session);

  return op;  
}

static gzochid_client_session_pending_operation *create_message_operation
(mpz_t target_session, unsigned char *msg, short len)
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

static int session_prepare (gpointer data)
{
  return TRUE;
}

static void cleanup_transaction 
(gzochid_client_session_transaction_context *tx_context)
{
  free (tx_context);
}

static void session_commit_operation (gpointer data, gpointer user_data)
{
  gzochid_application_context *context = 
    (gzochid_application_context *) user_data;
  gzochid_client_session_pending_operation *op = 
    (gzochid_client_session_pending_operation *) data;
  gzochid_client_session_pending_message_operation *msg_op = NULL;

  char *oid_str = mpz_get_str (NULL, 16, op->target_session);
  gzochid_protocol_client *client = NULL;

  g_mutex_lock (context->client_mapping_lock);
  client = (gzochid_protocol_client *) g_hash_table_lookup 
    (context->oids_to_clients, oid_str);
  free (oid_str);

  switch (op->type)
    {
    case GZOCHID_CLIENT_SESSION_OP_DISCONNECT:
      gzochid_protocol_client_disconnect (client); break;
    case GZOCHID_CLIENT_SESSION_OP_LOGIN_SUCCESS:
      gzochid_protocol_client_login_success (client); break;
    case GZOCHID_CLIENT_SESSION_OP_LOGIN_FAILURE:
      gzochid_protocol_client_login_failure (client); break;
    case GZOCHID_CLIENT_SESSION_OP_MESSAGE:

      msg_op = (gzochid_client_session_pending_message_operation *) op;
      gzochid_protocol_client_send (client, msg_op->message, msg_op->len);
      break;

    default:
      break;
    }

  g_mutex_unlock (context->client_mapping_lock);
}

static void session_commit (gpointer data)
{
  gzochid_client_session_transaction_context *tx_context = 
    (gzochid_client_session_transaction_context *) data;

  g_list_foreach 
    (tx_context->operations, session_commit_operation, tx_context->context);
  cleanup_transaction (tx_context);
}

static void session_rollback (gpointer data)
{
  gzochid_client_session_transaction_context *tx_context = 
    (gzochid_client_session_transaction_context *) data;

  cleanup_transaction (tx_context);
}

static gzochid_client_session_handler *deserialize_handler 
(gzochid_application_context *context, GString *in)
{
  gzochid_client_session_handler *handler = malloc 
    (sizeof (gzochid_client_session_handler)); 

  handler->received_message = 
    gzochid_application_callback_serialization.deserializer (context, in);
  handler->disconnected = 
    gzochid_application_callback_serialization.deserializer (context, in);

  return handler;
}

static gpointer deserialize_client_session
(gzochid_application_context *context, GString *in)
{
  gzochid_auth_identity *identity = 
    gzochid_auth_identity_deserializer (context, in);
  GSequence *channels = gzochid_util_deserialize_sequence 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string);
  gzochid_client_session *session = gzochid_client_session_new (identity);

  g_sequence_free (session->channels);
  session->channels = channels;

  gzochid_util_deserialize_mpz (in, session->scm_oid);

  if (gzochid_util_deserialize_boolean (in))
    session->handler = deserialize_handler (context, in);

  return session;
}

static void serialize_handler 
(gzochid_application_context *context, gzochid_client_session_handler *handler,
 GString *out)
{
  gzochid_application_callback_serialization.serializer 
    (context, handler->received_message, out);
  gzochid_application_callback_serialization.serializer 
    (context, handler->disconnected, out);
}

static void serialize_client_session 
(gzochid_application_context *context, gpointer obj, GString *out)
{
  gzochid_client_session *session = (gzochid_client_session *) obj;

  gzochid_auth_identity_serializer (context, session->identity, out);
  gzochid_util_serialize_sequence 
    (session->channels, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);
  gzochid_util_serialize_mpz (session->scm_oid, out);
  if (session->handler != NULL)
    {
      gzochid_util_serialize_boolean (TRUE, out);
      serialize_handler (context, session->handler, out);
    }
  else gzochid_util_serialize_boolean (FALSE, out);
}

static gzochid_transaction_participant session_participant =
  { "session", session_prepare, session_commit, session_rollback };

gzochid_io_serialization gzochid_client_session_serialization = 
  { serialize_client_session, deserialize_client_session };

gzochid_client_session *gzochid_client_session_new 
(gzochid_auth_identity *identity)
{
  gzochid_client_session *session = (gzochid_client_session *)
    calloc (1, sizeof (gzochid_client_session));

  session->identity = identity;
  session->channels = g_sequence_new (NULL);
  mpz_init (session->scm_oid);

  return session;
}

void gzochid_client_session_free (gzochid_client_session *session)
{
  mpz_clear (session->scm_oid);
  g_sequence_free (session->channels);
  free (session);
}

static void join_transaction (gzochid_application_context *context)
{
  if (!gzochid_transaction_active()
      || gzochid_transaction_context (&session_participant) == NULL)
    gzochid_transaction_join
      (&session_participant, create_transaction_context (context));
}

void gzochid_client_session_disconnect 
(gzochid_application_context *context, gzochid_client_session *session)
{
  gzochid_client_session_transaction_context *tx_context = NULL;
  gzochid_data_managed_reference *reference = NULL;
  
  join_transaction (context);
  tx_context = gzochid_transaction_context (&session_participant);
  reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  tx_context->operations = g_list_append 
    (tx_context->operations, create_disconnect_operation (reference->oid));
}

void gzochid_client_session_send_login_success
(gzochid_application_context *context, gzochid_client_session *session)
{
  gzochid_client_session_transaction_context *tx_context = NULL;
  gzochid_data_managed_reference *reference = NULL;

  join_transaction (context);
  tx_context = gzochid_transaction_context (&session_participant);
  reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  tx_context->operations = g_list_append 
    (tx_context->operations, create_login_success_operation (reference->oid));
}

void gzochid_client_session_send_login_failure
(gzochid_application_context *context, gzochid_client_session *session)
{
  gzochid_client_session_transaction_context *tx_context = NULL;
  gzochid_data_managed_reference *reference = NULL;

  join_transaction (context);
  tx_context = gzochid_transaction_context (&session_participant);
  reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  tx_context->operations = g_list_append
    (tx_context->operations, create_login_failure_operation (reference->oid));
}

void gzochid_client_session_send_message 
(gzochid_application_context *context, gzochid_client_session *session, 
 unsigned char *msg, short len)
{
  gzochid_client_session_transaction_context *tx_context = NULL;
  gzochid_data_managed_reference *reference = NULL;

  join_transaction (context);
  tx_context = gzochid_transaction_context (&session_participant);
  reference = gzochid_data_create_reference 
    (context, &gzochid_client_session_serialization, session);

  assert (reference->state != GZOCHID_MANAGED_REFERENCE_STATE_NEW);

  tx_context->operations = g_list_append 
    (tx_context->operations, 
     create_message_operation (reference->oid, msg, len));
}
