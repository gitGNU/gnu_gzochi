/* game-protocol.h: Prototypes and declarations for game-protocol.c
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

#ifndef GZOCHID_GAME_PROTOCOL_H
#define GZOCHID_GAME_PROTOCOL_H

#include "gzochid-auth.h"
#include "protocol.h"
#include "socket.h"

/* A `gzochid_server_protocol' implementation for the gzochi game application
   protocol. */

extern gzochid_server_protocol gzochid_game_server_protocol;

/* A `gzochid_client_protocol' implementation for the gzochi game application
   protocol. */

extern gzochid_client_protocol gzochid_game_client_protocol;

/* A struct representing a connected gzochi game application client. 
   `gzochid_game_client' instances are created and managed by the protocol. */

typedef struct _gzochid_game_client gzochid_game_client;

/* Returns the `gzochid_auth_identity' associated with the specified client. */

gzochid_auth_identity *gzochid_game_client_get_identity (gzochid_game_client *);

/* Disconnects the specified client. The client struct should not be used after
   this function returns. */

void gzochid_game_client_disconnect (gzochid_game_client *);

/* Notifies the specified client that it has successfully authenticated with the
   target game application. */

void gzochid_game_client_login_success (gzochid_game_client *);

/* Notifies the specified client that it has failed to authenticate with the
   target game application. */

void gzochid_game_client_login_failure (gzochid_game_client *);

/* Sends the specified message (of specified length, which must be less than 
   65532 bytes) to the specified client. Note that this function returns once 
   the bytes have been copied to the client's send buffer; the entire payload
   may not be flushed until the associated socket is ready to write all of
   it. */

void gzochid_game_client_send
(gzochid_game_client *, unsigned char *, unsigned short);

/* Private client socket API, visible for testing only. */

gboolean _gzochid_game_client_disconnected (gzochid_game_client *);

gzochid_client_socket *_gzochid_game_client_get_socket (gzochid_game_client *);

#endif /* GZOCHID_GAME_PROTOCOL_H */
