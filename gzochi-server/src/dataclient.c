/* dataclient.c: Dataserver client for gzochid
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
#include <glib-object.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "data-protocol.h"
#include "dataclient.h"
#include "dataclient-protocol.h"
#include "meta-protocol.h"
#include "socket.h"

/* Captures callback configuration for a request issued through the data 
   client. */

struct _dataclient_callback_registration
{
  /* The expected opcode of the response. Used as a kind of check bit to ensure
     that the correct response is being processed on behalf of a request. */

  unsigned char expected_opcode; 
  
  /* The success callback. */

  gzochid_dataclient_success_callback success_callback; 

  gpointer success_data; /* Closure data for the success callback. */

  /* The failure callback. */
  
  gzochid_dataclient_failure_callback failure_callback;

  gpointer failure_data; /* Closure data for the failure callback. */

  /* The duration of the point or range lock. Set upon success to the current 
     monotonic time plus the microsecond-adjusted value of `lock.release.msec'
     or `rangelock.release.msec', depending. */

  gint64 timeout; 

  /* The release callback. */
  
  gzochid_dataclient_release_callback release_callback; 

  gpointer release_data; /* Closure data for the release callback. */
};

typedef struct _dataclient_callback_registration
dataclient_callback_registration;

/* Holds the state of active callbacks for value and oid requests. */

struct _dataclient_callback_queue
{  
  GMutex mutex; /* Mutex to protect the list of registrations */

  /* The oids callback function. */

  gzochid_dataclient_oids_callback oids_callback; 

  gpointer oids_callback_data; /* Closure data for the oids callback. */  

  /* List of `dataclient_callback_registration' objects. */

  GList *callback_registrations; 
};

typedef struct _dataclient_callback_queue dataclient_callback_queue;

/* Boilerplate setup for the data client object. */

/* The data client object. */

struct _GzochidDataClient
{
  GObject parent_instance;

  /* The number of milliseconds before the release of a successfully-acquired 
     point lock will be requested. Set via `lock.release.msec'. */

  unsigned int lock_release_ms;

  /* The number of milliseconds before the release of a successfully-acquired 
     range lock will be requested. Set via `rangelock.release.msec'. */  
  
  unsigned int range_lock_release_ms;
  
  GzochidConfiguration *configuration; /* The global configuration object. */

  /* A map of application names to `dataclient_callback_queue' objects. */
  
  GHashTable *application_callback_queues; 

  GMutex queue_mutex; /* Protects the callback queue table. */
  
  /* A `GMainContext' for scheduling tasks (such as timed lock releases) to be 
     run by the metaclient while connected to the server. This field is 
     "inherited" from the metaclient. */

  GMainContext *main_context; 

  /* The reconnectable socket wrapping the connection to the metaserver, to be
     used for writes. This field is "inherited" from the metaclient. */
  
  gzochid_reconnectable_socket *socket;
};

/* Boilerplate setup for the data client object. */

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

enum gzochid_data_client_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_MAIN_CONTEXT,
    PROP_SOCKET,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_data_client_set_property (GObject *object, guint property_id,
				  const GValue *value, GParamSpec *pspec)
{
  GzochidDataClient *self = GZOCHID_DATA_CLIENT (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;

    case PROP_MAIN_CONTEXT:
      self->main_context = g_main_context_ref (g_value_get_boxed (value));
      break;

    case PROP_SOCKET:
      self->socket = g_value_get_pointer (value);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_data_client_constructed (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);  

  /* Extract the "metaserver" configuration group to use to look up lock 
     release timeouts . */

  GHashTable *metaserver_config = gzochid_configuration_extract_group
    (client->configuration, "metaserver");
  
  client->lock_release_ms = gzochid_config_to_int
    (g_hash_table_lookup (metaserver_config, "lock.release.msec"), 1000);
  client->range_lock_release_ms = gzochid_config_to_int
    (g_hash_table_lookup (metaserver_config, "rangelock.release.msec"), 500);

  g_hash_table_destroy (metaserver_config);
}

static void
gzochid_data_client_finalize (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);
  
  g_hash_table_destroy (client->application_callback_queues);

  g_mutex_clear (&client->queue_mutex);

  G_OBJECT_CLASS (gzochid_data_client_parent_class)->finalize (gobject);  
}

static void
gzochid_data_client_dispose (GObject *gobject)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (gobject);

  g_object_unref (client->configuration);

  G_OBJECT_CLASS (gzochid_data_client_parent_class)->dispose (gobject);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = gzochid_data_client_constructed;
  object_class->dispose = gzochid_data_client_dispose;
  object_class->finalize = gzochid_data_client_finalize;
  object_class->set_property = gzochid_data_client_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_MAIN_CONTEXT] = g_param_spec_boxed
    ("main-context", "main-context", "The meta client's main context",
     G_TYPE_MAIN_CONTEXT, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  obj_properties[PROP_SOCKET] = g_param_spec_pointer
    ("reconnectable-socket", "socket",
     "The meta client's reconnectable socket",
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

/* Create and return a new callback registration object with the specified
   expected opcode and success, failure, and release callbacks (with associated
   user data pointers. This object should freed via `free' when no longer 
   needed. */

static dataclient_callback_registration *
create_callback
(unsigned char expected_opcode,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  dataclient_callback_registration *registration =
    malloc (sizeof (dataclient_callback_registration));

  registration->expected_opcode = expected_opcode;
  
  registration->success_callback = success_callback;
  registration->success_data = success_data;

  registration->failure_callback = failure_callback;
  registration->failure_data = failure_data;

  /* This initial release timeout is a sentinel value of sorts indicating that
     the lock has not yet be acquired. */
  
  registration->timeout = G_MAXINT64;
  
  registration->release_callback = release_callback;
  registration->release_data = release_data;
  
  return registration;
}

/* Frees the callback queue structure, including all pending callbacks. */

static void
free_callback_queue (dataclient_callback_queue *queue)
{
  g_mutex_clear (&queue->mutex);
  g_list_free_full (queue->callback_registrations, free);
  free (queue);
}

/*
  Returns the callback queue structure associated with the specified gzochi 
  game application name, creating one if necessary.

  Note that in order to support safe concurrent access to the queue's internal
  fields, the callback queue is returned with its mutex locked. Callers must
  unlock release the queue when they are done using it via a call to 
  `release_callback_queue'.
*/

static dataclient_callback_queue *
acquire_callback_queue (GzochidDataClient *client, char *app)
{
  dataclient_callback_queue *queue = NULL;
  
  g_mutex_lock (&client->queue_mutex);

  if (g_hash_table_contains (client->application_callback_queues, app))   
    queue = g_hash_table_lookup (client->application_callback_queues, app);
  else
    {
      queue = malloc (sizeof (dataclient_callback_queue));

      g_mutex_init (&queue->mutex);

      queue->oids_callback = NULL;
      queue->oids_callback_data = NULL;
      queue->callback_registrations = NULL;

      g_hash_table_insert
	(client->application_callback_queues, strdup (app), queue);
    }

  g_mutex_lock (&queue->mutex);  
  g_mutex_unlock (&client->queue_mutex);

  return queue;
}

/* Releases the specified callback queue structure. */

static void
release_callback_queue (dataclient_callback_queue *queue)
{
  g_mutex_unlock (&queue->mutex);
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  g_mutex_init (&self->queue_mutex);
  self->application_callback_queues = g_hash_table_new_full
    (g_str_hash, g_str_equal, free, (GDestroyNotify) free_callback_queue);
  
}

/* End boilerplate. */

/*
  A convenience function for prefixing a message payload with its size and
  opcode. Every message needs these things and the memory management is a
  bit tedious. 

  Returns a pointer to a newly-allocated buffer containing the complete message,
  which should be freed when no longer needed. The `formatted_len' argument, if
  provided, will be set to the length of this buffer. 
*/

static unsigned char *
format_message (guchar opcode, GBytes *payload, size_t *formatted_len)
{
  size_t len = 0;
  const unsigned char *data = g_bytes_get_data (payload, &len);
  unsigned char *buf = malloc (sizeof (unsigned char) * (len + 3));

  /* Prefix the message with its length. */
  
  gzochi_common_io_write_short (len, buf, 0);
  buf[2] = opcode;
  memcpy (buf + 3, data, len);

  if (formatted_len != NULL)
    *formatted_len = len + 3;
  
  return buf;
}

/*
  Convenience function to write the specified opcode and byte payload to the
  connected metaserver.

  This function does not attempt to recover from cases in which a message is 
  only partially sent before the socket is disconnected. Under these 
  circumstances, clients should make no assumptions about the state of submitted
  changesets and should purge all internal state. */

static void
write_message (GzochidDataClient *client, guchar opcode, GBytes *payload)
{
  size_t len = 0;
  unsigned char *buf = format_message (opcode, payload, &len);

  gzochid_reconnectable_socket_write (client->socket, buf, len);
  free (buf);
}

void
gzochid_dataclient_received_oids (GzochidDataClient *client,
				  gzochid_data_reserve_oids_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  /* If somebody's waiting for oids, let them know. */
  
  if (queue->oids_callback != NULL)
    {
      queue->oids_callback (response->block, queue->oids_callback_data);

      /* Null out the oids callback after receipt. */
      
      queue->oids_callback = NULL;
      queue->oids_callback_data = NULL;
    }

  release_callback_queue (queue);
}

void
gzochid_dataclient_reserve_oids (GzochidDataClient *client, char *app,
				 gzochid_dataclient_oids_callback callback,
				 gpointer user_data)
{
  GBytes *message_bytes = g_bytes_new (app, strlen (app) + 1);
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_OIDS, message_bytes);
  g_bytes_unref (message_bytes);

  /* The client is responsible for synchronizing / serializing calls to this
     function. */
  
  assert (queue->oids_callback == NULL);
  queue->oids_callback = callback;
  queue->oids_callback_data = user_data;

  release_callback_queue (queue);
}

static gboolean
invoke_release_callback (gpointer user_data)
{
  dataclient_callback_registration *registration = user_data;
  registration->release_callback (registration->release_data);  
  return FALSE;
}

/* Convenience function to handle the processing of a message received in
   response to a value or sequential key request, and representing a successful
   or unsuccessful fulfillment of the request. Some error checking is performed
   to ensure that responses message are received in the same order in which the
   requests were submitted. */

static void
process_queued_callback (GzochidDataClient *client,
			 dataclient_callback_queue *queue, unsigned char opcode,
			 gzochid_data_response *response)
{
  GList *callback_link = queue->callback_registrations;
  dataclient_callback_registration *callbacks = callback_link->data;

  queue->callback_registrations = g_list_delete_link
    (queue->callback_registrations, queue->callback_registrations);

  /* Check that the message opcode is the same as the opcode expected by the
     callback registration at the head of the queue. */
  
  if (opcode != callbacks->expected_opcode)
    {
      g_warning
	("Received response %d for %s/%s; expected response %d.", opcode,
	 response->app, response->store, callbacks->expected_opcode);
      free (callbacks);
    }
  else if (response->success)
    {
      GSource *release_callback = NULL;
      
      callbacks->success_callback (response->data, callbacks->success_data);
      assert (callbacks->timeout == G_MAXINT64);

      if (opcode == GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE)
        release_callback = g_timeout_source_new (client->lock_release_ms);
      else if (opcode == GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE)
	release_callback = g_timeout_source_new (client->range_lock_release_ms);

      if (release_callback != NULL)
	{
	  g_source_set_callback
	    (release_callback, invoke_release_callback, callbacks, free);
	  g_source_attach (release_callback, client->main_context);
	}
    }
  else
    {
      callbacks->failure_callback (response->timeout, callbacks->failure_data);
      free (callbacks);
    }
}

void
gzochid_dataclient_received_value (GzochidDataClient *client,
				   gzochid_data_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  if (queue->callback_registrations == NULL)
    g_warning
      ("Received value for %s/%s but no callbacks registered.", response->app,
       response->store);
  else process_queued_callback
	 (client, queue, GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE, response);

  release_callback_queue (queue);
}

void
gzochid_dataclient_request_value
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gboolean for_write,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  GByteArray *payload = g_byte_array_new ();
  const unsigned char *key_bytes = NULL;
  size_t payload_len = 0, key_len = 0;
  GBytes *payload_bytes = NULL;

  /* Serialize the value request message. */
  
  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);
  g_byte_array_append
    (payload, (unsigned char *) &(unsigned char[]) { for_write ? 1 : 0 }, 1);

  key_bytes = g_bytes_get_data (key, &key_len);

  /* Grow the byte array by 2 bytes. */
  
  payload_len = payload->len;
  g_byte_array_set_size (payload, payload_len + 2);

  /* Write the key length directly to the buffer. */
  
  gzochi_common_io_write_short (key_len, payload->data, payload_len);
  
  g_byte_array_append (payload, key_bytes, key_len);
  
  payload_bytes = g_byte_array_free_to_bytes (payload);
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_VALUE, payload_bytes);
  g_bytes_unref (payload_bytes);

  /* Add a callback registration to the queue. */
  
  queue->callback_registrations = g_list_append
    (queue->callback_registrations,
     create_callback (GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE,
		      success_callback, success_data,
		      failure_callback, failure_data,
		      release_callback, release_data));

  release_callback_queue (queue);
}

void
gzochid_dataclient_received_next_key (GzochidDataClient *client,
				      gzochid_data_response *response)
{
  dataclient_callback_queue *queue = acquire_callback_queue
    (client, response->app);

  if (queue->callback_registrations == NULL)
    g_warning
      ("Received key range response for %s/%s but no callbacks registered.",
       response->app, response->store);
  else process_queued_callback
	 (client, queue, GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE, response);

  release_callback_queue (queue);
}

/* Convenience function to append a length-prefixed array of bytes to the
   specified `GByteArray', or a pair of `NULL' bytes if the specified byte
   buffer is `NULL'. */

static void
write_nullable_bytes (GByteArray *payload, GBytes *key)
{
  if (key != NULL)
    {
      size_t payload_len = 0, key_len = 0;
      const unsigned char *key_bytes = g_bytes_get_data (key, &key_len);

      /* Grow the byte array by 2 bytes. */
      
      payload_len = payload->len;
      g_byte_array_set_size (payload, payload_len + 2);

      /* Write the key length directly to the buffer. */
  
      gzochi_common_io_write_short (key_len, payload->data, payload_len);

      g_byte_array_append (payload, key_bytes, key_len);
    }
  else g_byte_array_append
	 (payload, (unsigned char *) &(unsigned char[]) { 0, 0 }, 2);
}

void
gzochid_dataclient_request_next_key
(GzochidDataClient *client, char *app, char *store, GBytes *key,
 gzochid_dataclient_success_callback success_callback, gpointer success_data,
 gzochid_dataclient_failure_callback failure_callback, gpointer failure_data,
 gzochid_dataclient_release_callback release_callback, gpointer release_data)
{
  dataclient_callback_queue *queue = acquire_callback_queue (client, app);
  GByteArray *payload = g_byte_array_new ();
  GBytes *payload_bytes = NULL;
  
  /* Serialize the key request message. */

  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);

  write_nullable_bytes (payload, key);

  payload_bytes = g_byte_array_free_to_bytes (payload);
  write_message (client, GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY, payload_bytes);
  g_bytes_unref (payload_bytes);
  
  /* Add a callback registration to the queue. */

  queue->callback_registrations = g_list_append
    (queue->callback_registrations,
     create_callback (GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE,
		      success_callback, success_data,
		      failure_callback, failure_data,
		      release_callback, release_data));

  release_callback_queue (queue);
}

void
gzochid_dataclient_submit_changeset
(GzochidDataClient *client, char *app, GArray *changes)
{
  gzochid_data_changeset *changeset = gzochid_data_changeset_new (app, changes);
  GByteArray *payload = g_byte_array_new ();
  GBytes *payload_bytes = NULL;

  /* Serialize the changeset submission message. */
  
  gzochid_data_protocol_changeset_write (changeset, payload);

  payload_bytes = g_byte_array_free_to_bytes (payload); 
  write_message (client, GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET, payload_bytes);
  g_bytes_unref (payload_bytes);

  gzochid_data_changeset_free (changeset);
}

void gzochid_dataclient_release_key
(GzochidDataClient *client, char *app, char *store, GBytes *key)
{
  GByteArray *payload = g_byte_array_new ();
  const unsigned char *key_bytes = NULL;
  size_t payload_len = 0, key_len = 0, len;
  GBytes *payload_bytes = NULL;
  unsigned char *buf = NULL;
  
  /* Serialize the key request message. */

  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);

  key_bytes = g_bytes_get_data (key, &key_len);

  /* Grow the byte array by 2 bytes. */
  
  payload_len = payload->len;
  g_byte_array_set_size (payload, payload_len + 2);

  /* Write the key length directly to the buffer. */
  
  gzochi_common_io_write_short (key_len, payload->data, payload_len);
  
  g_byte_array_append (payload, key_bytes, key_len);
  
  payload_bytes = g_byte_array_free_to_bytes (payload);

  /* We can't call `write_message' here because it wants to lock the client
     mutex. */

  buf = format_message (GZOCHID_DATA_PROTOCOL_RELEASE_KEY, payload_bytes, &len);
  gzochid_reconnectable_socket_write (client->socket, buf, len);
  free (buf);

  g_bytes_unref (payload_bytes);
}

void gzochid_dataclient_release_key_range
(GzochidDataClient *client, char *app, char *store, GBytes *from, GBytes *to)
{
  GByteArray *payload = g_byte_array_new ();
  GBytes *payload_bytes = NULL;
  unsigned char *buf = NULL;
  size_t len = 0;
  
  /* Serialize the key request message. */

  g_byte_array_append (payload, (unsigned char *) app, strlen (app) + 1);
  g_byte_array_append (payload, (unsigned char *) store, strlen (store) + 1);

  write_nullable_bytes (payload, from);   
  write_nullable_bytes (payload, to);

  payload_bytes = g_byte_array_free_to_bytes (payload);

  /* We can't call `write_message' here because it wants to lock the client
     mutex. */
  
  buf = format_message
    (GZOCHID_DATA_PROTOCOL_RELEASE_KEY_RANGE, payload_bytes, &len);
  gzochid_reconnectable_socket_write (client->socket, buf, len);
  free (buf);

  g_bytes_unref (payload_bytes);
}

GQuark
gzochid_data_client_error_quark ()
{
  return g_quark_from_static_string ("gzochid-data-client-error-quark");
}
