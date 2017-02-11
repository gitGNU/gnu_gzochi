/* channelclient.h: Prototypes and declarations for channelclient.c
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

#ifndef GZOCHID_CHANNELCLIENT_H
#define GZOCHID_CHANNELCLIENT_H

#include <glib.h>
#include <glib-object.h>

#include "game.h"

/* The core channel client type definitions. */

#define GZOCHID_TYPE_CHANNEL_CLIENT gzochid_channel_client_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_channel_client_get_type (void);

typedef struct _GzochidChannelClient GzochidChannelClient;

struct _GzochidChannelClientClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidChannelClientClass GzochidChannelClientClass;

static inline GzochidChannelClient *
GZOCHID_CHANNEL_CLIENT (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_channel_client_get_type (), GzochidChannelClient);
}

/* End boilerplate. */

enum
  {
    /* Indicates a failure to deliver a message from the channel server because
       the target identity was not mapped to a connected session on the 
       receiving node. */
    
    GZOCHID_CHANNELCLIENT_ERROR_NOT_MAPPED,
    
    GZOCHID_CHANNELCLIENT_ERROR_FAILED /* Generic channel client failure. */
  };

#define GZOCHID_CHANNELCLIENT_ERROR gzochid_channelclient_error_quark ()

GQuark gzochid_channelclient_error_quark (void);

/* The following functions direct information outwards to the meta server. */

/* Relays a notification through the meta server to join the specified session 
   to the specified channel (qualified by application name). */

void gzochid_channelclient_relay_join_from (GzochidChannelClient *,
					    const char *, guint64, guint64);

/* Relays a notification through the meta server to remove the specified session
   from the specified channel (qualified by application name). */

void gzochid_channelclient_relay_leave_from (GzochidChannelClient *,
					     const char *, guint64, guint64);

/* Relays a notification through the meta server to close the specified channel
   (qualified by application name) on all connected application nodes. */

void gzochid_channelclient_relay_close_from (GzochidChannelClient *,
					     const char *, guint64);

/* Relays a message to the specified channel (qualified by application name) 
   through the meta server. */

void gzochid_channelclient_relay_message_from (GzochidChannelClient *,
					       const char *, guint64, GBytes *);

/* The following functions are callbacks for message delivered from the meta
   server. */

/*
  Notifies the local channel management system to join the specified local
  session to the specified channel (qualified by application name). 

  Signals an error if the specified application is not running locally.
*/

void gzochid_channelclient_relay_join_to (GzochidChannelClient *, const char *,
					  guint64, guint64, GError **);

/*
  Notifies the local channel management system to remove the specified local
  session from the specified channel (qualified by application name). 

  Signals an error if the specified application is not running locally.
*/

void gzochid_channelclient_relay_leave_to (GzochidChannelClient *, const char *,
					   guint64, guint64, GError **);

/*
  Notifies the local channel management system to close the specified channel 
  (qualified by application name). 

  Signals an error if the specified application is not running locally.
*/

void gzochid_channelclient_relay_close_to (GzochidChannelClient *, const char *,
					   guint64, GError **);

/*
  Notifies the local channel management system to deliver the specified message
  to the specified channel (qualified by application name). 

  Signals an error if the specified application is not running locally.
*/

void gzochid_channelclient_relay_message_to (GzochidChannelClient *,
					     const char *, guint64, GBytes *,
					     GError **);

#endif /* GZOCHID_CHANNELCLIENT_H */
