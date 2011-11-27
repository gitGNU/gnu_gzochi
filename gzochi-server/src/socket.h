/* socket.h: Prototypes and declarations for socket.c
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

#ifndef GZOCHID_SOCKET_H
#define GZOCHID_SOCKET_H

#include <glib.h>
#include <libserveez.h>

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

  svz_socket_t *server;
  GHashTable *clients;
} gzochid_socket_server_context;

gzochid_socket_server_context *gzochid_socket_server_context_new (void);
void gzochid_socket_server_context_free (gzochid_socket_server_context *);
void gzochid_socket_server_context_init 
(gzochid_socket_server_context *, gzochid_context *, int);

#endif /* GZOCHID_SOCKET_H */
