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

#include "protocol.h"

typedef struct _gzochi_metad_dataserver_client gzochi_metad_dataserver_client;

/* A `gzochid_server_protocol' implementation for the dataserver protocol. */

gzochid_server_protocol gzochi_metad_dataserver_server_protocol;

/* A `gzochid_client_protocol' implementation for the dataserver protocol. */

gzochid_client_protocol gzochi_metad_dataserver_client_protocol;

#endif /* GZOCHID_DATASERVER_PROTOCOL_H */
