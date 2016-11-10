/* dataclient.c: Prototypes and declarations for dataclient.c
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

#ifndef GZOCHID_DATACLIENT_H
#define GZOCHID_DATACLIENT_H

#include <glib.h>
#include <glib-object.h>
#include <sys/time.h>

#include "data-protocol.h"

/* The core data client type definitions. */

#define GZOCHID_TYPE_DATA_CLIENT gzochid_data_client_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_data_client_get_type (void);

typedef struct _GzochidDataClient GzochidDataClient;

struct _GzochidDataClientClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidDataClientClass GzochidDataClientClass;

static inline GzochidDataClient *
GZOCHID_DATA_CLIENT (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_data_client_get_type (), GzochidDataClient);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to parse the meta server address. */
    
    GZOCHID_DATA_CLIENT_ERROR_ADDRESS,

    /* Indicates a failure to create a socket. May indicate a resource
       shortfall. */
    
    GZOCHID_DATA_CLIENT_ERROR_SOCKET, 

    GZOCHID_DATA_CLIENT_ERROR_NETWORK, /* A network request failure. */    
    GZOCHID_DATA_CLIENT_ERROR_FAILED /* Generic data client failure. */
  };

/* The data client error domain. */

#define GZOCHID_DATA_CLIENT_ERROR gzochid_data_client_error_quark ()

/* Starts the specified data client's connection maintenance thread and 
   prepares it to begin servicing requests for data from the configured meta
   server. */

void gzochid_dataclient_start (GzochidDataClient *, GError **);

/* Stops the specified client's connection maintenance thread, breaking the 
   client's connection to the meta server and halting request processing. */

void gzochid_dataclient_stop (GzochidDataClient *);

/* Notifies the specified data client that its connection to the meta server is
   no longer valid (because it has encountered an error, e.g.) and that it needs
   to be re-established. Intended for use as a callback from the `error' handler
   of the client protocol. */

void gzochid_dataclient_nullify_connection (GzochidDataClient *);

/* Function pointer typedef for a "success" callback to a request for a value or
   binding. The `GBytes' argument may be null if the request was successful
   (i.e., a lock was established) but the value was missing. */

typedef void (*gzochid_dataclient_success_callback) (GBytes *, gpointer);

/* Function pointer typedef for a "failure" callback to a request for a value or
   binding; generally indicates a failure to obtain a required lock. The 
   timestamp gives a suggested time to wait before retrying the request. */

typedef void (*gzochid_dataclient_failure_callback) (struct timeval, gpointer);

/* Function pointer typedef for a "release" callback, which indicates that the
   holder of a point or range lock should call `gzochid_dataclient_release_key'
   or `gzochid_dataclient_release_key_range' (as appropriate) at the earliest
   opportunity to do so. */

typedef void (*gzochid_dataclient_release_callback) (gpointer);

/* Function pointer typedef for a response to a request for a block of oids.
   Object id reservations may not fail; an invocation of this callback indicates
   a successful reservation of the specified block. */

typedef void (*gzochid_dataclient_oids_callback)
(gzochid_data_oids_block, gpointer);

/* Request a block of oids for objects belonging to the specified gzochi game 
   application, with the response delivered via the specified callback, which 
   will be invoked with the specified user data pointer. */

void gzochid_dataclient_reserve_oids
(GzochidDataClient *, char *, gzochid_dataclient_oids_callback, gpointer);

/*
  Request a value from the specified store (which must be "oids" or "names") 
  associated with the specified gzochi game application. The response to this
  request will be delivered to the specified success or failure callback (with
  associated user data pointer) as appropriate. 
  
  The release callback will be called (with its associated user data pointer) 
  `lock.release.msecs' milliseconds after the successful acquisition of a lock 
  on this value. (Upgrading a read lock to a write lock does not extend the 
  lease time.)
*/

void gzochid_dataclient_request_value
(GzochidDataClient *, char *, char *, GBytes *, gboolean,
 gzochid_dataclient_success_callback, gpointer,
 gzochid_dataclient_failure_callback, gpointer,
 gzochid_dataclient_release_callback, gpointer);

/*
  Request the key from the specified store (which must be "oids" or "names") 
  associated with the specified gzochi game application that follows the 
  specified key in lexicographical order. The response to this request will be 
  delivered to the specified success or failure callback (with associated user 
  data pointer) as appropriate.
  
  The release callback will be called (with its associated user data pointer) 
  `rangelock.release.msecs' milliseconds after the successful acquisition of a 
  range lock on these bounds.

  The key argument may be `NULL' to indicate that the first key in the store
  should be returned.
*/

void gzochid_dataclient_request_next_key
(GzochidDataClient *, char *, char *, GBytes *,
 gzochid_dataclient_success_callback, gpointer,
 gzochid_dataclient_failure_callback, gpointer,
 gzochid_dataclient_release_callback, gpointer);

/* Submit the specified array of changes against the specified gzochi game 
   application to the data server. */

void gzochid_dataclient_submit_changeset
(GzochidDataClient *, char *, GArray *);

/* Notify the data server of the release of the lock held on the specified key 
   with respect to the specified gzochi game application and store. */

void gzochid_dataclient_release_key
(GzochidDataClient *, char *, char *, GBytes *);

/* Notify the data server of the release of the range lock held on the specified
   key range with respect to the specified gzochi game application and store. */

void gzochid_dataclient_release_key_range
(GzochidDataClient *, char *, char *, GBytes *, GBytes *);

/* The following functions are used by the dataclient protocol to notify the 
   client of a response to a previously-submitted request. With the exception of
   usage in test code, they should not be called outside of the protocol's flow
   of control. */

/* Notify the client that a response to an oid reservation request has 
   arrived. */

void gzochid_dataclient_received_oids
(GzochidDataClient *, gzochid_data_reserve_oids_response *);

/* Notify the client that a response to a value request has arrived. */

void gzochid_dataclient_received_value
(GzochidDataClient *, gzochid_data_response *);

/* Notify the client that a response to a key range request has arrived. */

void gzochid_dataclient_received_next_key
(GzochidDataClient *, gzochid_data_response *);

GQuark gzochid_data_client_error_quark ();

#endif /* GZOCHID_DATACLIENT_H */
