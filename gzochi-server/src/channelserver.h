/* channelserver.h: Prototypes and declarations for channelserver.c
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

#ifndef GZOCHI_METAD_CHANNEL_SERVER_H
#define GZOCHI_METAD_CHANNEL_SERVER_H

#include <glib.h>
#include <glib-object.h>

#include "socket.h"

/* The core channel server type definitions. */

#define GZOCHI_METAD_TYPE_CHANNEL_SERVER gzochi_metad_channel_server_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochi_metad_channel_server_get_type (void);

typedef struct _GzochiMetadChannelServer GzochiMetadChannelServer;

struct _GzochiMetadChannelServerClass
{
  GObjectClass parent_class;
};

typedef struct _GzochiMetadChannelServerClass GzochiMetadChannelServerClass;

static inline GzochiMetadChannelServer *
GZOCHI_METAD_CHANNEL_SERVER (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochi_metad_channel_server_get_type (), GzochiMetadChannelServer);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to service a request because the target application
       server node is already connected per the channel server's internal 
       state. */
    
    GZOCHI_METAD_CHANNELSERVER_ERROR_ALREADY_CONNECTED,

    /* Indicates a failure to service a request because the target application 
       server node is not connected per the channel server's internal state. */

    GZOCHI_METAD_CHANNELSERVER_ERROR_NOT_CONNECTED,
    
    /* Generic channel server failure. */

    GZOCHI_METAD_CHANNELSERVER_ERROR_FAILED 
  };

#define GZOCHI_METAD_CHANNELSERVER_ERROR \
  gzochi_metad_channelserver_error_quark ()

GQuark gzochi_metad_channelserver_error_quark (void);

/*
  Notifies the channel server that the specified gzochi application server node
  (identified by its node id and `gzochid_client_socket') has connected to the
  meta server. 
  
  An error is signaled if a server with the specified node id is already 
  connected. 
*/

void gzochi_metad_channelserver_server_connected (GzochiMetadChannelServer *,
                                                  int, gzochid_client_socket *,
                                                  GError **);

/*
  Notifies the channel server that the specified gzochi application server node
  (identified by its node id) has disconnected from the meta server. 
  
  An error is signaled if a server the specified node id is not currently 
  connected. 
*/

void gzochi_metad_channelserver_server_disconnected (GzochiMetadChannelServer *,
                                                     int, GError **);

/* Relays a notification from the specified node id to join the specified 
   session to the specified channel (qualified by application name). */

void gzochi_metad_channelserver_relay_join (GzochiMetadChannelServer *, int,
					    const char *, guint64, guint64);

/* Relays a notification from the specified node id to remove the specified 
   session from the specified channel (qualified by application name). */

void gzochi_metad_channelserver_relay_leave (GzochiMetadChannelServer *, int,
					     const char *, guint64, guint64);

/* Relays a message from the specified node id to the specified channel, 
   qualified by application name. */

void gzochi_metad_channelserver_relay_message (GzochiMetadChannelServer *, int,
					       const char *, guint64, GBytes *);

/* Notifies all connected application nodes on behalf of the specified node id
   to close the specified channel, qualified by application name. */

void gzochi_metad_channelserver_relay_close (GzochiMetadChannelServer *, int,
					     const char *, guint64);


#endif /* GZOCHI_METAD_CHANNEL_SERVER_H */
