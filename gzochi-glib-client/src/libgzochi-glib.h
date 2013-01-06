/* libgzochi-glib.h: Public prototypes and declarations for libgzochi-glib
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

#ifndef LIBGZOCHI_GLIB_H
#define LIBGZOCHI_GLIB_H

#include <glib.h>

typedef struct _gzochi_glib_client_session gzochi_glib_client_session;
typedef struct _GzochiSource GzochiSource;

typedef void (*gzochi_glib_client_disconnected_callback) 
(gzochi_glib_client_session *, gpointer);
typedef void (*gzochi_glib_client_received_message_callback)
(gzochi_glib_client_session *, unsigned char *, short, gpointer);

gzochi_glib_client_session *gzochi_glib_client_connect
(char *, int, char *, unsigned char *, int);
GzochiSource *gzochi_source_new (gzochi_glib_client_session *);

char *gzochi_glib_client_session_endpoint (gzochi_glib_client_session *);
char *gzochi_glib_client_session_hostname (gzochi_glib_client_session *);
int gzochi_glib_client_session_port (gzochi_glib_client_session *);

void gzochi_glib_client_session_set_disconnected_callback 
(gzochi_glib_client_session *, 
 gzochi_glib_client_disconnected_callback, 
 gpointer);

void gzochi_glib_client_session_set_received_message_callback
(gzochi_glib_client_session *, 
 gzochi_glib_client_received_message_callback, 
 gpointer);

void gzochi_glib_client_send 
(gzochi_glib_client_session *, unsigned char *, short);

#endif /* LIBGZOCHI_GLIB_H */
