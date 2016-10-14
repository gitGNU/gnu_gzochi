/* dataclient-protocol.h: Prototypes and declarations for dataclient-protocol.c
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

#ifndef GZOCHID_DATACLIENT_PROTOCOL_H
#define GZOCHID_DATACLIENT_PROTOCOL_H

#include "protocol.h"

/* The version of the data protocol understood by the client. */

#define GZOCHID_DATACLIENT_PROTOCOL_VERSION 0x02

/* A `gzochid_client_protocol' implementation for the dataclient protocol. */

gzochid_client_protocol gzochid_dataclient_client_protocol;

#endif /* GZOCHID_DATACLIENT_PROTOCOL_H */
