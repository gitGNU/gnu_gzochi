/* protocol.h: Preprocessor definitions for shared client-server protocol
 * Copyright (C) 2011 Julian Graham
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

#ifndef GZOCHI_COMMON_PROTOCOL_H
#define GZOCHI_COMMON_PROTOCOL_H

#define GZOCHI_COMMON_PROTOCOL_LOGIN_REQUEST 0x10
#define GZOCHI_COMMON_PROTOCOL_LOGIN_SUCCESS 0x11
#define GZOCHI_COMMON_PROTOCOL_LOGIN_FAILURE 0x12

#define GZOCHI_COMMON_PROTOCOL_LOGOUT_REQUEST 0x20
#define GZOCHI_COMMON_PROTOCOL_LOGOUT_SUCCESS 0x21

#define GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECT_REQUEST 0x30
#define GZOCHI_COMMON_PROTOCOL_SESSION_DISCONNECTED 0x31
#define GZOCHI_COMMON_PROTOCOL_SESSION_MESSAGE 0x32

#define GZOCHI_COMMON_PROTOCOL_CHANNEL_JOIN 0x40
#define GZOCHI_COMMON_PROTOCOL_CHANNEL_DISCONNECTED 0x41
#define GZOCHI_COMMON_PROTOCOL_CHANNEL_MESSAGE 0x42

#endif /* GZOCHI_COMMON_PROTOCOL_H */
