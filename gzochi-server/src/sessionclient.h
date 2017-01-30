/* sessionclient.h: Prototypes and declarations for sessionclient.c
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

#ifndef GZOCHID_SESSIONCLIENT_H
#define GZOCHID_SESSIONCLIENT_H

#include <glib.h>
#include <glib-object.h>

#include "game.h"

/* The core data client type definitions. */

#define GZOCHID_TYPE_SESSION_CLIENT gzochid_session_client_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_session_client_get_type (void);

typedef struct _GzochidSessionClient GzochidSessionClient;

struct _GzochidSessionClientClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidSessionClientClass GzochidSessionClientClass;

static inline GzochidSessionClient *
GZOCHID_SESSION_CLIENT (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_session_client_get_type (), GzochidSessionClient);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to deliver a message from the session server because
       the target identity was not mapped to a connected session on the 
       receiving node. */
    
    GZOCHID_SESSIONCLIENT_ERROR_NOT_MAPPED,
    
    GZOCHID_SESSIONCLIENT_ERROR_FAILED /* Generic session client failure. */
  };

#define GZOCHID_SESSIONCLIENT_ERROR gzochid_sessionclient_error_quark ()

GQuark gzochid_sessionclient_error_quark (void);

/* The following functions direct information outwards to the meta server. */

/* Notifies the meta server that the specified session (qualified by its
   application name) has connected to the local application server node. */

void gzochid_sessionclient_session_connected (GzochidSessionClient *,
					      const char *, guint64);

/* Notifies the meta server that the specified session (qualified by its
   application name) has disconnected from the local application server node. */

void gzochid_sessionclient_session_disconnected (GzochidSessionClient *,
						 const char *, guint64);

/* Relays via the meta server a signal to disconnect the specified session 
   (qualified by its application name) from the application server node to 
   which it is currently connected. */
  
void gzochid_sessionclient_relay_disconnect_from (GzochidSessionClient *,
						  const char *, guint64);

/* Relays via the meta server a message for the specified session (qualified by
   its application name) to be delivered to the application server node to which
   it is currently connected. */

void gzochid_sessionclient_relay_message_from (GzochidSessionClient *,
					       const char *, guint64, GBytes *);

/* The following functions are callbacks for messages delivered from the meta
   server. */

/*
  Notifies the application server node that the specified session (which the 
  meta server believes is local to this node) should be disconnected.
   
  An error is signaled if the target session is not locally connected.
*/

void gzochid_sessionclient_relay_disconnect_to (GzochidSessionClient *,
						const char *, guint64,
						GError **);

/*
  Notifies the application server node to deliver the specified message to the
  specified session, which the meta server believes is local to this node.

  An error is signaled if the target session is not locally connected.
*/

void gzochid_sessionclient_relay_message_to (GzochidSessionClient *,
					     const char *, guint64, GBytes *,
					     GError **);

#endif /* GZOCHID_SESSIONCLIENT_H */
