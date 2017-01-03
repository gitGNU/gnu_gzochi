/* sessionserver-protocol.h: Prototypes and decls for sessionserver-protocol.c
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

#ifndef GZOCHI_METAD_SESSION_SERVER_PROTOCOL_H
#define GZOCHI_METAD_SESSION_SERVER_PROTOCOL_H

#include "protocol.h"
#include "socket.h"
#include "sessionserver.h"

typedef struct _gzochi_metad_sessionserver_client
gzochi_metad_sessionserver_client;

/*
  Construct and return a new `gzochi_metad_sessionserver_client' object with the
  specified session server reference, connected client socket, and corresponding
  application server node id. 

  The memory associated with the returned pointer should be freed via 
  `gzochi_metad_sessionserver_client_free' when no longer in use. 
*/

gzochi_metad_sessionserver_client *gzochi_metad_sessionserver_client_new
(GzochiMetadSessionServer *, gzochid_client_socket *, unsigned int);

/*
  Releases the memory associated with the specified 
  `gzochi_metad_sessionserver_client' object. 

  Note that in the course of normal operation, sessionserver clients will be 
  freed automatically by the `free' callback of 
  `gzochi_metad_sessionserver_client_protocol'.
*/

void gzochi_metad_sessionserver_client_free
(gzochi_metad_sessionserver_client *);

/* A `gzochid_client_protocol' implementation for the sessionserver protocol. */

gzochid_client_protocol gzochi_metad_sessionserver_client_protocol;

#endif /* GZOCHI_METAD_SESSION_SERVER_PROTOCOL_H */
