/* socket.h: Prototypes and declarations for socket.c
 * Copyright (C) 2014 Julian Graham
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

#ifndef GZOCHID_SOCKET_H
#define GZOCHID_SOCKET_H

#include <glib.h>

#include "context.h"

enum gzochid_socket_server_state 
  {
    GZOCHID_SOCKET_SERVER_STATE_INITIALIZING,
    GZOCHID_SOCKET_SERVER_STATE_RUNNING,
    GZOCHID_SOCKET_SERVER_STATE_STOPPED
  };

typedef struct _gzochid_socket_server_context
{
  gzochid_context base;

  GMainContext *main_context;
  GMainLoop *main_loop;
} gzochid_socket_server_context;

typedef struct _gzochid_client_socket gzochid_client_socket;

gzochid_socket_server_context *
gzochid_socket_server_context_new (void);

void 
gzochid_socket_server_context_free (gzochid_socket_server_context *);

void 
gzochid_socket_server_context_init 
(gzochid_socket_server_context *, gzochid_context *, int);

gzochid_socket_server_context *
gzochid_socket_get_server_context (gzochid_client_socket *);

char *
gzochid_socket_get_connection_description (gzochid_client_socket *);

void
gzochid_client_socket_write (gzochid_client_socket *, unsigned char *, size_t);

void
gzochid_client_socket_free (gzochid_client_socket *);

#endif /* GZOCHID_SOCKET_H */
