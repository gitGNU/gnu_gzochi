/* libgzochi.h: Public prototypes and declarations for libgzochi
 * Copyright (C) 2012 Julian Graham
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

#ifndef LIBGZOCHI_H
#define LIBGZOCHI_H

typedef struct _gzochi_client_session gzochi_client_session;

typedef void (*gzochi_client_session_disconnected_callback) 
(gzochi_client_session *);
typedef void (*gzochi_client_session_received_message_callback)
(gzochi_client_session *, unsigned char *, short);

gzochi_client_session *gzochi_client_connect 
(char *, int, char *, unsigned char *, int);
void gzochi_client_disconnect (gzochi_client_session *);
void gzochi_client_run (gzochi_client_session *);
void gzochi_client_send (gzochi_client_session *, unsigned char *, short);

char *gzochi_client_session_endpoint (gzochi_client_session *);
char *gzochi_client_session_hostname (gzochi_client_session *);
int gzochi_client_session_port (gzochi_client_session *);

void gzochi_client_session_set_disconnected_callback
(gzochi_client_session *, gzochi_client_session_disconnected_callback);
void gzochi_client_session_set_received_message_callback
(gzochi_client_session *, gzochi_client_session_received_message_callback);

#endif /* LIBGZOCHI_H */
