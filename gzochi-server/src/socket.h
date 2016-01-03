/* socket.h: Prototypes and declarations for socket.c
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

#ifndef GZOCHID_SOCKET_H
#define GZOCHID_SOCKET_H

#include <glib.h>
#include <sys/socket.h>

#include "context.h"
#include "protocol.h"

enum gzochid_socket_context_state 
  {
    GZOCHID_SOCKET_CONTEXT_STATE_RUNNING,
    GZOCHID_SOCKET_CONTEXT_STATE_STOPPED
  };

struct _gzochid_socket_context
{
  gzochid_context base;

  GMainContext *main_context; /* The main context for socket events. */
  GMainLoop *main_loop; /* The main loop for socket events. */
};

typedef struct _gzochid_socket_context gzochid_socket_context;

typedef struct _gzochid_server_socket gzochid_server_socket;
typedef struct _gzochid_client_socket gzochid_client_socket;

/* The following functions can be used to manipulate `gzochid_socket_context'
   structures. */

/* Create and return a new socket context. */

gzochid_socket_context *gzochid_socket_context_new (void);

/* Frees the resources associated with the specified socket context. The 
   reference count for the context's main context and main loop will be
   decremented as part of this process. */

void gzochid_socket_context_free (gzochid_socket_context *);

/* Associate the specified socket context with the specified parent context and
   transition it to the `GZOCHID_SOCKET_CONTEXT_STATE_RUNNING' state. The socket
   context will launch an event-processing thread as part of this process. */

void gzochid_socket_context_init (gzochid_socket_context *, gzochid_context *);

/* The following functions can be used to manipulate `gzochid_client_socket'
   structures. */

/* Create and return a new client socket with the specified underlying IO 
   channel (to be owned by the returned socket), connection description, and 
   client protocol and callback data.

   In most cases, this function should be called from the `accept' callback of a
   server protocol. */

gzochid_client_socket *gzochid_client_socket_new
(GIOChannel *, const char *, gzochid_client_protocol, gpointer);

/* Frees the resources associated with the specified client socket. */

void gzochid_client_socket_free (gzochid_client_socket *);

/* Returns the connection description of the specified client socket. */

const char *gzochid_client_socket_get_connection_description
(gzochid_client_socket *);

/* Copies the specified buffer to the specified client socket's send buffer; it
   will be written to the underlying socket as soon as that socket indicates its
   readiness for data. */

void gzochid_client_socket_write
(gzochid_client_socket *, unsigned char *, size_t);

/* Private client socket API, visible for testing only. */

/* Returns the client protocol associated with the specified client socket. */

gzochid_client_protocol _gzochid_client_socket_get_protocol
(gzochid_client_socket *);

/* Returns the client protocol callback data associated with the specified 
   client socket. */

gpointer _gzochid_client_socket_get_protocol_data (gzochid_client_socket *);

/* The following functions can be used to manipulate `gzochid_server_socket'
   structures. */

/* Create and return a new server socket with the specified protocol and 
   callback data. */

gzochid_server_socket *gzochid_server_socket_new
(gzochid_server_protocol, gpointer);

/* Frees the resources associated with the specified server socket. */

void gzochid_server_socket_free (gzochid_server_socket *);

/* Attaches the specified server socket to the specified socket context to allow
   it to listen for incoming connections on the specified port number.

   The port number may be `0' to indicate that the socket should listen on an 
   arbitrary, available port. The address (including port) on which the socket
   is actually listening will be set (in the `addr' and `addrlen' fields of the
   server socket) once this function returns. */

void gzochid_server_socket_listen
(gzochid_socket_context *, gzochid_server_socket *, int);

/* Private server socket API, visible for testing only. */

/* Copies the actual socket address of the specified server socket to the 
   specified `sockaddr' struct. Semantics following the `getsockname' function
   in `<sys/socket.h>' - the length pointer indicates the available buffer in
   the address struct. It is updated to reflect the actual size of the address,
   which may be truncated as necessary. */

void _gzochid_server_socket_getsockname
(gzochid_server_socket *, struct sockaddr *, size_t *);

#endif /* GZOCHID_SOCKET_H */
