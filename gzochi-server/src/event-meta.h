/* event-meta.h: Prototypes and declarations for event-meta.c
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

#ifndef GZOCHI_METAD_EVENT_H
#define GZOCHI_METAD_EVENT_H

#include <glib-object.h>

#include "event.h"

/* Enumeration of client event types. */

enum _gzochi_metad_client_event_type
  {
    /* A gzochid application node has connected to the meta server. The 
       `connection-description' and (conditionally) the `admin-server-base-url'
       properties will be set. */
    
    CLIENT_CONNECTED,

    /* A gzochid application node has disconnected from the meta server. The
       `node-id' property can be used to identify the client. */
    
    CLIENT_DISCONNECTED
  };

typedef enum _gzochi_metad_client_event_type gzochi_metad_client_event_type;

/* The core client event type definitions. */

#define GZOCHI_METAD_TYPE_CLIENT_EVENT gzochi_metad_client_event_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochi_metad_client_event_get_type (void);

/*
  The client event sub-type. The following properties are available:
  
  node-id: meta server-assigned id for the connected client
  connection-description: string giving the address of the client
  admin-server-base-url: string giving the URL of the client's admin web 
    console, if available; `NULL' otherwise  
*/

typedef struct _GzochiMetadClientEvent GzochiMetadClientEvent;

struct _GzochiMetadClientEventClass
{
  GzochidEventClass parent_class;
};

typedef struct _GzochiMetadClientEventClass GzochiMetadClientEventClass;

static inline GzochiMetadClientEvent *
GZOCHI_METAD_CLIENT_EVENT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_client_event_get_type (), GzochiMetadClientEvent);
}

/* End boilerplate. */

#endif /* GZOCHI_METAD_EVENT_H */
