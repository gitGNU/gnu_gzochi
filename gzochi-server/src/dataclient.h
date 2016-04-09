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

GQuark gzochid_data_client_error_quark ();

#endif /* GZOCHID_DATACLIENT_H */
