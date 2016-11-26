/* dataserver-protocol.h: Prototypes and declarations for dataserver-protocol.c
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

#ifndef GZOCHI_METAD_DATASERVER_PROTOCOL_H
#define GZOCHI_METAD_DATASERVER_PROTOCOL_H

#include "dataserver.h"
#include "protocol.h"
#include "socket.h"

typedef struct _gzochi_metad_dataserver_client gzochi_metad_dataserver_client;

/*
  Construct and return a new `gzochi_metad_dataserver_client' object with the
  specified data server reference, connected client socket, and corresponding 
  application server node id. 

  The memory associated with the returned pointer should be freed via 
  `gzochi_metad_dataserver_client_free' when no longer in use. 
*/

gzochi_metad_dataserver_client *gzochi_metad_dataserver_client_new
(GzochiMetadDataServer *, gzochid_client_socket *, unsigned int);

/*
  Releases the memory associated with the specified 
  `gzochi_metad_dataserver_client' object. 

  Note that in the course of normal operation, dataserver clients will be freed
  automatically by the `free' callback of 
  `gzochi_metad_dataserver_client_protocol.
*/

void gzochi_metad_dataserver_client_free (gzochi_metad_dataserver_client *);

/* A `gzochid_client_protocol' implementation for the dataserver protocol. */

gzochid_client_protocol gzochi_metad_dataserver_client_protocol;

#endif /* GZOCHID_DATASERVER_PROTOCOL_H */
