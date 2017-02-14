/* game-protocol.c: Implementation of game application protocol.
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
#include <gzochi-common.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "app.h"
#include "app-task.h"
#include "event.h"
#include "game-protocol.h"
#include "game.h"
#include "gzochid-auth.h"
#include "protocol.h"
#include "schedule.h"
#include "scheme-task.h"
#include "session.h"
#include "sessionclient.h"
#include "socket.h"
#include "task.h"

/* The `gzochid_game_protocol_closure' struct definition. */

struct _gzochid_game_protocol_closure
{
  GzochidGameServer *game_server; /* Reference to the game server. */
  gzochid_task_queue *task_queue; /* The game server's task queue. */
  struct timeval tx_timeout; /* The default task execution timeout. */
};

gzochid_game_protocol_closure *
gzochid_game_protocol_create_closure (GzochidGameServer *game_server,
				      gzochid_task_queue *task_queue,
				      struct timeval tx_timeout)
{
  gzochid_game_protocol_closure *closure =
    malloc (sizeof (gzochid_game_protocol_closure));

  closure->game_server = g_object_ref (game_server);
  closure->task_queue = task_queue;
  closure->tx_timeout = tx_timeout;
  
  return closure;
}

/* The client struct for the game client protocol. */

struct _gzochid_game_client
{
  gzochid_game_protocol_closure *closure; /* The game protocol closure. */

  /* The client application context; `NULL' until login has completed. */
  
  gzochid_application_context *app_context; 
  
  /* The client's identity. `NULL' until login has completed. */

  gzochid_auth_identity *identity; 
  
  /* Whether the client is in the process of disconnecting. */

  gboolean disconnected; 

  gzochid_client_socket *sock; /* The client socket, for writes. */
};

static gzochid_client_socket *
server_accept (GIOChannel *channel, const char *desc, gpointer data)
{
  gzochid_game_protocol_closure *closure = data;
  gzochid_game_client *client = calloc (1, sizeof (gzochid_game_client));
  gzochid_client_socket *sock = gzochid_client_socket_new
    (channel, desc, gzochid_game_client_protocol, client);

  /* This is the initial bootstrap context for the client. Once they 
     authenticate, we'll assign them a real application context. */
  
  client->closure = closure;
  client->sock = sock;
  
  return sock;
}

gzochid_server_protocol gzochid_game_server_protocol = { server_accept };

static gboolean
client_can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return buffer->len >= 3
    && buffer->len >= gzochi_common_io_read_short (buffer->data, 0) + 3;
}

/* 
   A `gzochid_application_worker' implementation intended for use as the "catch"
   task worker for the transactional stage of the login process (see below). 

   Removes any mapping that may exist for the target client in the application
   context's session mapping table, and disconnects the actual client 
   connection.
*/

static void
login_catch_worker (gzochid_application_context *context,
		    gzochid_auth_identity *identity, gpointer data)
{
  guint64 *session_oid = data;
  gzochid_game_client *client = g_hash_table_lookup
    (context->oids_to_clients, session_oid);

  g_message
    ("Disconnecting session '%" G_GUINT64_FORMAT "'; failed login transaction.",
     *session_oid);

  g_mutex_lock (&context->client_mapping_lock);
  g_hash_table_remove (context->oids_to_clients, session_oid);
  g_hash_table_remove (context->clients_to_oids, client);
  g_mutex_unlock (&context->client_mapping_lock);

  /* Disconnect the client. */
  
  gzochid_game_client_disconnect (client);

  /* Run the Scheme disconnect callbacks. */
  
  gzochid_client_session_disconnected_worker (context, identity, session_oid);
}

/* The application task worker for the login event.  */

static void
logged_in_task (gzochid_application_context *context,
		gzochid_auth_identity *identity, gpointer data)
{
  GError *err = NULL;
  gzochid_game_client *client = data;
  gzochid_client_session *session = gzochid_client_session_new (identity);
  
  guint64 *session_oid = malloc (sizeof (guint64));
  guint64 local_session_oid = 0;
  
  gzochid_application_task *login_task = NULL;
  gzochid_application_task *login_catch_task = NULL;
  gzochid_application_task *application_task = NULL;
  gzochid_transactional_application_task_execution *execution = NULL;    

  gzochid_task task;
  
  gzochid_client_session_persist (context, session, session_oid, &err);

  if (err != NULL)
    {
      g_warning
	("Failed to bind session oid for identity '%s'; disconnecting: %s",
	 gzochid_auth_identity_name (identity), err->message);

      g_error_free (err);
      free (session_oid);
      
      gzochid_game_client_disconnect (client);
      return;
    }

  /* Useful to have a stack-local version of this id, in case it gets freed by
     one of the cleanup handler for some task. */
  
  else local_session_oid = *session_oid;
  
  login_task = gzochid_application_task_new
    (context, identity, gzochid_scheme_application_logged_in_worker,
     session_oid);

  login_catch_task = gzochid_application_task_new
    (context, identity, login_catch_worker, session_oid);

  execution = gzochid_transactional_application_task_timed_execution_new 
    (login_task, login_catch_task, NULL, client->closure->tx_timeout);

  /* Not necessary to hold a ref to these, as we've transferred them to the
     execution. */
  
  gzochid_application_task_unref (login_task);
  gzochid_application_task_unref (login_catch_task);

  g_mutex_lock (&context->client_mapping_lock);
  g_hash_table_insert (context->oids_to_clients,
		       g_memdup (session_oid, sizeof (guint64)), client);
  g_hash_table_insert (context->clients_to_oids, client,
		       g_memdup (session_oid, sizeof (guint64)));
  g_mutex_unlock (&context->client_mapping_lock);

  application_task = gzochid_application_task_new
    (context, gzochid_game_client_get_identity (client), 
     gzochid_application_reexecuting_transactional_task_worker, execution);
     
  task.worker = gzochid_application_task_thread_worker;
  task.data = application_task;
  gettimeofday (&task.target_execution_time, NULL);

  gzochid_schedule_run_task (client->closure->task_queue, &task);

  /* If after executing the login task the client is *still* present in the
     client-to-session oid mapping table, safe to assume they've completed the
     login process; if we're connected to a metaserver, inform it that there's
     a new session on this application server node. */
  
  if (g_hash_table_contains (context->oids_to_clients, &local_session_oid)
      && context->metaclient != NULL)
    {
      GzochidSessionClient *sessionclient = NULL;

      g_object_get
	(context->metaclient, "session-client", &sessionclient, NULL);

      gzochid_sessionclient_session_connected
	(sessionclient, context->descriptor->name, local_session_oid);
      
      g_object_unref (sessionclient);
    }
}

/* Schedules the transactional stage of the login process. */

static void 
logged_in (gzochid_application_context *context, gzochid_game_client *client)
{
  gzochid_application_task *application_task = gzochid_application_task_new
    (context, gzochid_game_client_get_identity (client), logged_in_task,
     client);

  gzochid_task *task = gzochid_task_immediate_new
    (gzochid_application_task_thread_worker, application_task);

  gzochid_schedule_submit_task (client->closure->task_queue, task);

  gzochid_task_free (task);
}

static void 
dispatch_login_request (gzochid_game_client *client, char *endpoint,
			unsigned char *cred, short cred_len)
{
  GError *error = NULL;

  if (client->identity != NULL)
    {
      g_warning
	("Client with identity %s attempted to re-authenticate", 
	 gzochid_auth_identity_name (client->identity));
      return;
    }

  client->app_context = gzochid_game_server_lookup_application
    (client->closure->game_server, endpoint);
  if (client->app_context == NULL)
    {
      g_warning
	("Client at %s attempted to authenticate to unknown endpoint %s", 
	 gzochid_client_socket_get_connection_description (client->sock),
	 endpoint);

      /* TODO: Disconnect! */
      
      return;
    }

  assert (client->app_context->authenticator != NULL);
  client->identity = client->app_context->authenticator 
    (cred, cred_len, client->app_context->auth_data, &error);
 
  if (client->identity == NULL)
    {
      if (error != NULL)
	g_critical 
	  ("Error from authenticator for endpoint '%s': %s", endpoint,
	   error->message);
      else g_warning 
	     ("Client at %s failed to authenticate to endpoint %s", 
	      gzochid_client_socket_get_connection_description (client->sock),
	      endpoint);

      g_clear_error (&error);
    }
  else 
    {
      g_message
	("Client at %s authenticated to endpoint %s as %s",
	 gzochid_client_socket_get_connection_description (client->sock),
	 endpoint, gzochid_auth_identity_name (client->identity));
      logged_in (client->app_context, client);
    }
}

/* Schedules the transactional stage of the disconnect process. */

static void 
disconnected (gzochid_application_context *context, gzochid_game_client *client)
{
  guint64 *session_oid = NULL;

  g_mutex_lock (&context->client_mapping_lock);
  session_oid = g_hash_table_lookup (context->clients_to_oids, client);
  if (session_oid == NULL)
    {
      g_mutex_unlock (&context->client_mapping_lock);
      return;
    }
  else 
    {
      gzochid_application_task *callback_task = 
	gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client),
	 gzochid_scheme_application_disconnected_worker, session_oid);
      gzochid_application_task *catch_task =
	gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client),
	 gzochid_client_session_disconnected_worker, session_oid);
      gzochid_application_task *cleanup_task = 
	gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client), 
	 gzochid_scheme_application_disconnected_cleanup_worker,
	 session_oid);
      gzochid_transactional_application_task_execution *execution = 
	gzochid_transactional_application_task_timed_execution_new 
	(callback_task, catch_task, cleanup_task,
	 client->closure->tx_timeout);
      gzochid_application_task *application_task = gzochid_application_task_new 
	(context, gzochid_game_client_get_identity (client),
	 gzochid_application_resubmitting_transactional_task_worker, execution);

      gzochid_task task;
      
      /* Not necessary to hold a ref to these, as we've transferred them to the
	 execution. */
  
      gzochid_application_task_unref (callback_task);
      gzochid_application_task_unref (catch_task);
      gzochid_application_task_unref (cleanup_task);

      task.worker = gzochid_application_task_thread_worker;
      task.data = application_task;
      gettimeofday (&task.target_execution_time, NULL);

      if (g_hash_table_contains (context->oids_to_clients, session_oid))
	{
	  /* If this application server node is connected to a metaserver, let
	     the metaserver know that the session is disconnecting. This isn't
	     the only place that a client can be unmapped, but it should be the
	     only place that it can happen to a client that was previously
	     announced to the metaserver. */
	  
	  if (context->metaclient != NULL)
	    {
	      GzochidSessionClient *sessionclient = NULL;

	      g_object_get
		(context->metaclient, "session-client", &sessionclient, NULL);

	      gzochid_sessionclient_session_disconnected
		(sessionclient, context->descriptor->name, *session_oid);
	      
	      g_object_unref (sessionclient);
	    }

	  g_hash_table_remove (context->clients_to_oids, client);
	  g_hash_table_remove (context->oids_to_clients, session_oid);
	}
      
      gzochid_schedule_submit_task (client->closure->task_queue, &task);

      g_mutex_unlock (&context->client_mapping_lock);
    }
}

static void 
dispatch_logout_request (gzochid_game_client *client)
{
  if (client->identity == NULL)
    g_warning
      ("Received logout request from unauthenticated client at %s",
       gzochid_client_socket_get_connection_description (client->sock));
  else disconnected (client->app_context, client);

  client->disconnected = TRUE;

  /* This will trigger the client's read source to be destroyed and ultimately
     the protocol's `free' function to be called. */
  
  gzochid_client_socket_free (client->sock);

}

/* Cleanup handler for the received message event. */

static void
cleanup_received_message_arguments (gzochid_application_context *context,
				    gzochid_auth_identity *identity,
				    gpointer data)
{
  void **args = data;

  /* The session oid (arg 0) comes from the client-session mapping table and
     can't be freed here. */
  
  g_bytes_unref (args[1]);  
  free (args);
}

/* Schedules the transactional stage of the "received message" process. */

static void 
received_message (gzochid_application_context *context,
		  gzochid_game_client *client, unsigned char *msg, short len)
{
  guint64 *session_oid = NULL;

  g_mutex_lock (&context->client_mapping_lock);
  session_oid = g_hash_table_lookup (context->clients_to_oids, client);
  g_mutex_unlock (&context->client_mapping_lock);

  if (session_oid == NULL)
    return;
  else 
    {
      void **data = malloc (sizeof (void *) * 2);
      
      gzochid_application_task *transactional_task = NULL;
      gzochid_application_task *cleanup_task = NULL;
      gzochid_application_task *application_task = NULL;
      gzochid_transactional_application_task_execution *execution = NULL;
      gzochid_task task;

      data[0] = session_oid;
      data[1] = g_bytes_new (msg, len);
      
      transactional_task = gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client),
	 gzochid_scheme_application_received_message_worker, data);

      cleanup_task = gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client),
	 cleanup_received_message_arguments, data);
      
      execution = gzochid_transactional_application_task_timed_execution_new
	(transactional_task, NULL, cleanup_task,
	 client->closure->tx_timeout);

      /* Not necessary to hold a ref to these, as we've transferred them to the
	 execution. */
  
      gzochid_application_task_unref (transactional_task);
      gzochid_application_task_unref (cleanup_task);
      
      application_task = gzochid_application_task_new
	(context, gzochid_game_client_get_identity (client),
	 gzochid_application_resubmitting_transactional_task_worker, execution);
      
      task.worker = gzochid_application_task_thread_worker;
      task.data = application_task;
      gettimeofday (&task.target_execution_time, NULL);

      gzochid_schedule_submit_task (client->closure->task_queue, &task);
    }
}

static void 
dispatch_session_message (gzochid_game_client *client, unsigned char *msg,
			  short len)
{
  if (client->identity == NULL)
      g_warning 
	("Received session message from unauthenticated client at %s",
	 gzochid_client_socket_get_connection_description (client->sock));
  else
    {
      gzochid_event_dispatch
	(client->app_context->event_source,
	 g_object_new (GZOCHID_TYPE_EVENT, "type", MESSAGE_RECEIVED, NULL));
      
      received_message (client->app_context, client, msg, len);
    }
}

static void 
dispatch_message (gzochid_game_client *client, unsigned char *message,
		  unsigned short len)
{
  int opcode = message[0];

  unsigned char *payload = message + 1;
  char *pfx = NULL;
  unsigned char *sfx = NULL;
  short pfx_len = 0, sfx_len = 0;

  len--;
  
  switch (opcode)
    {
    case GZOCHI_COMMON_PROTOCOL_LOGIN_REQUEST:

      pfx = strndup ((char *) payload, len);
      pfx_len = strlen (pfx) + 1;
      sfx = payload + pfx_len;
      sfx_len = len - pfx_len;

      dispatch_login_request (client, pfx, sfx, sfx_len);

      break;
    case GZOCHI_COMMON_PROTOCOL_LOGOUT_REQUEST:
      dispatch_logout_request (client); break;
    case GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE:
      dispatch_session_message (client, (unsigned char *) payload, len); break;

    default:
      g_warning ("Unexpected opcode %d received from client", opcode);
    }

  if (pfx != NULL)
    free (pfx);

  return;
}

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  gzochid_game_client *client = user_data;
  
  int offset = 0, total = 0;
  int remaining = buffer->len;

  while (remaining >= 3)
    {
      unsigned short len = gzochi_common_io_read_short
	((unsigned char *) buffer->data, offset);
      
      if (++len > remaining - 2)
	break;
      
      offset += 2;

      dispatch_message (client, (unsigned char *) buffer->data + offset, len);
      
      offset += len;
      remaining -= len + 2;
      total += len + 2;
    }

  return total;
}

static void 
client_free (gpointer data)
{
  gzochid_game_client *client = data;
  
  free (client);
}

static void
client_error (gpointer data)
{
  gzochid_game_client *client = data;

  if (!client->disconnected)
    {
      if (client->identity != NULL)
	disconnected (client->app_context, client);
      client->disconnected = TRUE;

      /* This will trigger the client's read source to be destroyed and 
	 ultimately the protocol's `free' function to be called. */
  
      gzochid_client_socket_free (client->sock);
    }
}

gzochid_client_protocol gzochid_game_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };

gzochid_auth_identity *
gzochid_game_client_get_identity (gzochid_game_client *client)
{
  return client->identity;
}

void 
gzochid_game_client_disconnect (gzochid_game_client *client)
{
  unsigned char buf[3] = 
    { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_login_success (gzochid_game_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_SUCCESS };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_login_failure (gzochid_game_client *client)
{
  unsigned char buf[3] = { 0x0, 0x0, GZOCHI_COMMON_PROTOCOL_LOGIN_FAILURE };
  gzochid_client_socket_write (client->sock, buf, 3);
}

void 
gzochid_game_client_send
(gzochid_game_client *client, const unsigned char *msg, unsigned short len)
{
  unsigned char *buf = malloc (sizeof (unsigned char) * (len + 3));

  gzochi_common_io_write_short (len, buf, 0);
  buf[2] = GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE;
  memcpy (buf + 3, msg, len);

  gzochid_client_socket_write (client->sock, buf, len + 3);
  free (buf);
}

gboolean
_gzochid_game_client_disconnected (gzochid_game_client *client)
{
  return client->disconnected;
}
