/* event-app.h: Prototypes and declarations for event-app.c
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

#ifndef GZOCHID_EVENT_APP_H
#define GZOCHID_EVENT_APP_H

#include <glib-object.h>

#include "event.h"

/* Enumeration of meta server event types. */

enum _gzochid_meta_server_event_type
  {
    /* A connection to a meta server has been established. The 
       `connection-description' and (conditionally) the `admin-server-base-url'
       properties will be set. */
    
    META_SERVER_CONNECTED,

    /* The connection to the meta server has been broken. */
    
    META_SERVER_DISCONNECTED 
  };

typedef enum _gzochid_meta_server_event_type gzochid_meta_server_event_type;

/* The core meta server event type definitions. */

#define GZOCHID_TYPE_META_SERVER_EVENT gzochid_meta_server_event_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_meta_server_event_get_type (void);

/*
  The meta server event sub-type. The following properties are available:

  connection-description: string giving the address of the meta server
  admin-server-base-url: string giving the URL of the meta server's admin web 
    console, if available; `NULL' otherwise
*/

typedef struct _GzochidMetaServerEvent GzochidMetaServerEvent;

struct _GzochidMetaServerEventClass
{
  GzochidEventClass parent_class;
};

typedef struct _GzochidMetaServerEventClass GzochidMetaServerEventClass;

static inline GzochidMetaServerEvent *
GZOCHID_META_SERVER_EVENT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_meta_server_event_get_type (), GzochidMetaServerEvent);
}

/* End boilerplate. */

#endif /* GZOCHID_EVENT_APP_H */
