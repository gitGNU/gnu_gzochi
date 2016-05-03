/* dataserver.c: Prototypes and declarations for dataserver.c
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

#ifndef GZOCHI_METAD_DATASERVER_H
#define GZOCHI_METAD_DATASERVER_H

#include <glib.h>
#include <glib-object.h>
#include <gmp.h>

#include "data-protocol.h"
#include "oids.h"

/* The core data server type definitions. */

#define GZOCHI_METAD_TYPE_DATA_SERVER gzochi_metad_data_server_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochi_metad_data_server_get_type (void);

typedef struct _GzochiMetadDataServer GzochiMetadDataServer;

struct _GzochiMetadDataServerClass
{
  GObjectClass parent_class;
};

typedef struct _GzochiMetadDataServerClass GzochiMetadDataServerClass;

static inline GzochiMetadDataServer *
GZOCHI_METAD_DATA_SERVER (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_data_server_get_type (), GzochiMetadDataServer);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to obtain the locks necessary to process a changeset,
       perhaps because the locks were not set ahead of time via object or
       binding requests. */
    
    GZOCHI_METAD_DATASERVER_ERROR_LOCK_CONFLICT,

    /* The store name was not one of "oids" or "names." */
    
    GZOCHI_METAD_DATASERVER_ERROR_STORE_NAME,
    
    GZOCHI_METAD_DATASERVER_ERROR_FAILED /* Generic data server failure. */
  };

#define GZOCHI_METAD_DATASERVER_ERROR gzochi_metad_dataserver_error_quark ()

GQuark gzochi_metad_dataserver_error_quark (void);

/* Prepares the specified data server to begin processing requests and starts it
   listening on the specified port. */

void gzochi_metad_dataserver_start (GzochiMetadDataServer *);

/* Stops the specified data server, halting request processing. */

void gzochi_metad_dataserver_stop (GzochiMetadDataServer *);

/* Requests from the specified data server on behalf of the specified node id a
   block of unallocated object ids for the specified application, and returns a
   `gzochid_data_reserve_oids_response' (which should be freed via 
   `gzochid_data_reserve_oids_response_free' when no longer necessary) 
   describing the block. */

gzochid_data_reserve_oids_response *gzochi_metad_dataserver_reserve_oids
(GzochiMetadDataServer *, guint, char *);

/* Requests from the specified data server on behalf of the specified node id
   the value with the specified key in one of the specified application's 
   persistent stores (optionally locking it for write) and returns a 
   `gzochid_data_response' (which should be freed via 
   `gzochid_data_response_free' when no longer necessary) describing the
   outcome of the request. */

gzochid_data_response *gzochi_metad_dataserver_request_value
(GzochiMetadDataServer *, guint, char *, char *, GBytes *, gboolean, GError **);

/* Requests from the specified data server on behalf of the specified node id
   the key that immediately follows the specified key in one of the specified 
   application's persistent stores and returns a `gzochid_data_response' 
   (which should be freed via `gzochid_data_response_free' when no longer 
   necessary) describing the outcome of the request. */

gzochid_data_response *gzochi_metad_dataserver_request_next_key
(GzochiMetadDataServer *, guint, char *, char *, GBytes *, GError **);

/* Releases all locks held by the specified node id on the specified key
   within the specified data server, application, and store. */ 

void gzochi_metad_dataserver_release_key
(GzochiMetadDataServer *, guint, char *, char *, GBytes *);

/* Releases the range lock held by the specified node id on the specified 
   key interval (as established by a previous call to 
   `gzochi_metad_dataserver_request_next_key') within the specified data server,
   application, and store. */ 

void gzochi_metad_dataserver_release_range
(GzochiMetadDataServer *, guint, char *, char *, GBytes *, GBytes *);

/* Releases all locks (read / write / range) held by the specified node id 
   within the specified data server. */ 

void gzochi_metad_dataserver_release_all
(GzochiMetadDataServer *, guint);

/* Process the specified changeset on behalf of the specified node id with 
   respect to the specified data server as a single, atomic transaction. Sets
   the optional `GError' if the transaction cannot be committed (e.g., because 
   of missing locks). */

void gzochi_metad_dataserver_process_changeset
(GzochiMetadDataServer *, guint, gzochid_data_changeset *, GError **);

#endif /* GZOCHI_METAD_DATASERVER_H */
