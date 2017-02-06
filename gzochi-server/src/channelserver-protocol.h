/* channelserver-protocol.h: Prototypes and decls for channelserver-protocol.c
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

#ifndef GZOCHI_METAD_CHANNEL_SERVER_PROTOCOL_H
#define GZOCHI_METAD_CHANNEL_SERVER_PROTOCOL_H

#include "protocol.h"
#include "socket.h"
#include "channelserver.h"

typedef struct _gzochi_metad_channelserver_client
gzochi_metad_channelserver_client;

/*
  Construct and return a new `gzochi_metad_channelserver_client' object with the
  specified channel server reference, connected client socket, and corresponding
  application server node id. 

  The memory associated with the returned pointer should be freed via 
  `gzochi_metad_channelserver_client_free' when no longer in use. 
*/

gzochi_metad_channelserver_client *gzochi_metad_channelserver_client_new
(GzochiMetadChannelServer *, gzochid_client_socket *, unsigned int);

/*
  Releases the memory associated with the specified 
  `gzochi_metad_channelserver_client' object. 

  Note that in the course of normal operation, channelserver clients will be 
  freed automatically by the `free' callback of 
  `gzochi_metad_channelserver_client_protocol'.
*/

void gzochi_metad_channelserver_client_free
(gzochi_metad_channelserver_client *);

/* A `gzochid_client_protocol' implementation for the channelserver protocol. */

gzochid_client_protocol gzochi_metad_channelserver_client_protocol;

#endif /* GZOCHI_METAD_CHANNEL_SERVER_PROTOCOL_H */
