/* protocol.h: Prototypes and declarations for protocol.c
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

#ifndef LIBGZOCHI_PROTOCOL_H
#define LIBGZOCHI_PROTOCOL_H

#include "session.h"

int gzochi_protocol_send_login_request 
(gzochi_client_session *, char *, unsigned char *, int);
int gzochi_protocol_send_disconnect (gzochi_client_session *);
int gzochi_protocol_send_session_message 
(gzochi_client_session *, unsigned char *, short);

void gzochi_protocol_run (gzochi_client_session *);
void gzochi_protocol_run_once (gzochi_client_session *);

#endif /* LIBGZOCHI_PROTOCOL_H */
