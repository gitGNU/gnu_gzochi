/* sessionserver.h: Prototypes and declarations for sessionserver.c
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

#ifndef GZOCHI_METAD_SESSION_SERVER_H
#define GZOCHI_METAD_SESSION_SERVER_H

#include <glib.h>
#include <glib-object.h>

#include "socket.h"

/* The core session server type definitions. */

#define GZOCHI_METAD_TYPE_SESSION_SERVER gzochi_metad_session_server_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochi_metad_session_server_get_type (void);

typedef struct _GzochiMetadSessionServer GzochiMetadSessionServer;

struct _GzochiMetadSessionServerClass
{
  GObjectClass parent_class;
};

typedef struct _GzochiMetadSessionServerClass GzochiMetadSessionServerClass;

static inline GzochiMetadSessionServer *
GZOCHI_METAD_SESSION_SERVER (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_session_server_get_type (), GzochiMetadSessionServer);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to service a request because the target resource 
       (application server node or client identity) is already connected per the
       session server's internal state. */
    
    GZOCHI_METAD_SESSIONSERVER_ERROR_ALREADY_CONNECTED,

    /* Indicates a failure to service a request because the target resource 
       (application server node or client identity) is not connected per the
       session server's internal state. */

    GZOCHI_METAD_SESSIONSERVER_ERROR_NOT_CONNECTED,
    
    /* Generic session server failure. */

    GZOCHI_METAD_SESSIONSERVER_ERROR_FAILED 
  };

#define GZOCHI_METAD_SESSIONSERVER_ERROR \
  gzochi_metad_sessionserver_error_quark ()

GQuark gzochi_metad_sessionserver_error_quark (void);

/*
  Notifies the session server that the specified gzochi application server node
  (identified by its node id and `gzochid_client_socket') has connected to the
  meta server. 
  
  An error is signaled if a server with the specified node id is already 
  connected. 
*/

void gzochi_metad_sessionserver_server_connected (GzochiMetadSessionServer *,
						  int, gzochid_client_socket *,
						  GError **);

/*
  Notifies the session server that the specified gzochi application server node
  (identified by its node id) has disconnected from the meta server. 
  
  An error is signaled if a server the specified node id is not currently 
  connected. 
*/

void gzochi_metad_sessionserver_server_disconnected (GzochiMetadSessionServer *,
						     int, GError **);

/*
  Notifies the session server that the specified client session (identified by
  the name of the gzochi application to which it has connected, and the session
  id) has connected from the specified gzochi application server node 
  (identified by its node id).

  An error is signaled if the specified application server node id is not 
  currently connected, or if the specified session id is already connected.
*/

void gzochi_metad_sessionserver_session_connected (GzochiMetadSessionServer *,
						   int, char *, guint64,
						   GError **);

/*
  Notifies the session server that the specified client session (identified by
  the name of the gzochi application to which it was connected, and the session
  id) has disconnected from the specified gzochi application server node 
  (identified by its node id).

  An error is signaled if the specified application server node id or the 
  specified session id are not currently known to be connected.
*/

void gzochi_metad_sessionserver_session_disconnected
(GzochiMetadSessionServer *, char *, guint64, GError **);

/*
  Relays a "disconnect" signal to the specified client session (identified by 
  the name of the gzochi application to which it is connected, and the session
  id) via the gzochi application server node through which it is connected.
  
  An error is signaled if the specified session id is not currently known to be
  connected.
*/

void gzochi_metad_sessionserver_relay_disconnect (GzochiMetadSessionServer *,
						  char *, guint64, GError **);

/*
  Relays the specified message to the specified client session (identified by 
  the name of the gzochi application to which it is connected, and the session
  id) via the gzochi application server node through which it is connected.
  
  An error is signaled if the specified session id is not currently known to be
  connected.
*/

void gzochi_metad_sessionserver_relay_message (GzochiMetadSessionServer *,
					       char *, guint64, GBytes *,
					       GError **);

#endif /* GZOCHI_METAD_SESSION_SERVER_H */
