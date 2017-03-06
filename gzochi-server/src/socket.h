/* socket.h: Prototypes and declarations for socket.c
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

#ifndef GZOCHID_SOCKET_H
#define GZOCHID_SOCKET_H

#include <glib.h>
#include <glib-object.h>
#include <sys/socket.h>

#include "protocol.h"

/* GObject type definition for the socket server. */

#define GZOCHID_TYPE_SOCKET_SERVER gzochid_socket_server_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_socket_server_get_type (void);

/* The socket server struct itself; this part of the server is public. */

struct _GzochidSocketServer
{
  GObject parent_instance;

  GMainContext *main_context; /* The main context for socket events. */
  GMainLoop *main_loop; /* The main loop for socket events. */
};

typedef struct _GzochidSocketServer GzochidSocketServer;

struct _GzochidSocketServerClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidSocketServerClass GzochidSocketServerClass;

static inline GzochidSocketServer *
GZOCHID_SOCKET_SERVER (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_socket_server_get_type (), GzochidSocketServer);
}

/* End boilerplate. */

/* The following functions can be used to control the lifecycle of a 
   `GzochidSocketServer' object. */

/* Starts the socket server's main loop. A thread (managed by the server) will
   be launched to drive the loop's iteration. */

void gzochid_socket_server_start (GzochidSocketServer *);

/* Stops the specified socket server. */

void gzochid_socket_server_stop (GzochidSocketServer *);

/* Typedefs for client and server sockets. */

typedef struct _gzochid_server_socket gzochid_server_socket;
typedef struct _gzochid_client_socket gzochid_client_socket;

/* The following functions can be used to manipulate `gzochid_client_socket'
   structures. */

/* Create and return a new client socket with the specified underlying IO 
   channel (to be owned by the returned socket), connection description, and 
   client protocol and callback data.

   In most cases, this function should be called from the `accept' callback of a
   server protocol. */

gzochid_client_socket *gzochid_client_socket_new
(GIOChannel *, const char *, gzochid_client_protocol, gpointer);

/* Attach the specified client socket to the specified socket server, increasing
   its reference count, and begin handling events according to its associated 
   protocol.

   This function can be used to listen for events on a client socket that was 
   created explicitly - that is to say, not as the result of an `accept' 
   invocation on a server socket. */

void gzochid_client_socket_listen
(GzochidSocketServer *, gzochid_client_socket *);

/* Increases the reference count of the specified client socket and returns the
   socket. */

gzochid_client_socket *gzochid_client_socket_ref (gzochid_client_socket *);

/* Decreases the reference count of the specified client socket. When the 
   reference count reaches zero, the memory associated with the socket will 
   be freed. */

void gzochid_client_socket_unref (gzochid_client_socket *);

/* Returns the connection description of the specified client socket. */

const char *gzochid_client_socket_get_connection_description
(gzochid_client_socket *);

/* Copies the specified buffer to the specified client socket's send buffer; it
   will be written to the underlying socket as soon as that socket indicates its
   readiness for data. */

void gzochid_client_socket_write
(gzochid_client_socket *, const unsigned char *, size_t);

/* Private client socket API, visible for testing only. */

/* Returns the client protocol associated with the specified client socket. */

gzochid_client_protocol _gzochid_client_socket_get_protocol
(gzochid_client_socket *);

/* Returns the client protocol callback data associated with the specified 
   client socket. */

gpointer _gzochid_client_socket_get_protocol_data (gzochid_client_socket *);

/* The following functions can be used to manipulate `gzochid_server_socket'
   structures. */

/* Create and return a new server socket with the specified name, protocol and 
   callback data. */

gzochid_server_socket *gzochid_server_socket_new
(char *, gzochid_server_protocol, gpointer);

/* Frees the resources associated with the specified server socket. */

void gzochid_server_socket_free (gzochid_server_socket *);

/* Attaches the specified server socket to the specified socket context to allow
   it to listen for incoming connections on the specified port number.

   The port number may be `0' to indicate that the socket should listen on an 
   arbitrary, available port. The address (including port) on which the socket
   is actually listening will be set (in the `addr' and `addrlen' fields of the
   server socket) once this function returns. */

void gzochid_server_socket_listen
(GzochidSocketServer *, gzochid_server_socket *, int);

/*
  The "reconnectable socket" API below provides an abstraction for buffering
  outgoing messages on a logical connection (such as the one between the 
  metaclient and metaserver) that may be periodically interrupted and 
  re-established, and which is shared between several components.
   
  N.B. This initial implementation has some rough edges, esp. with respect to 
  lifecycle management. Because they are not reference-counted, ownership of
  reconnectable sockets must be explicit. Additionally, no delivery guarantees 
  are provided with respect to buffered messages; they may be lost if they are
  in transit during disconnection. (Note that this is true of outbound
  `gzochid_client_socket' messages as well.)
*/

/* Typedef for reconnectable socket. */

typedef struct _gzochid_reconnectable_socket gzochid_reconnectable_socket;

/* Typedef for "connect" and "disconnect" event callback. */

typedef void (*gzochid_reconnectable_socket_connected) (gpointer);
typedef void (*gzochid_reconnectable_socket_disconnected) (gpointer);

/* Construct and return a new reconnectable socket. The returned socket will
   initially be in a disconnected state. The memory used by the returned socket
   should be freed via `gzochid_reconnectable_socket_free' when no longer in 
   use. */

gzochid_reconnectable_socket *gzochid_reconnectable_socket_new ();

/*
  Frees the memory used by the specified reconnectable socket.
  
  This function has no effect on the `gzochid_client_socket' that the 
  reconnectable socket may be wrapping.
*/

void gzochid_reconnectable_socket_free (gzochid_reconnectable_socket *);

/* Adds the specified pair of callbacks and associated user data pointers to
   the list of callbacks triggered on connect/disconnect. */

void gzochid_reconnectable_socket_listen
(gzochid_reconnectable_socket *,
 gzochid_reconnectable_socket_connected, gpointer,
 gzochid_reconnectable_socket_disconnected, gpointer);

/* 
   Puts the specified reconnectable socket into a connected state wrapping the
   specified `gzochid_client_socket'.

   Upon connect, all registered `gzochid_reconnectable_socket_connected' 
   callbacks are first invoked (during which calls to 
   `gzochid_reconnectable_socket_write' will not be buffered) following which 
   the outbound message buffer is flushed.

   It is a programming error to connect a reconnectable socket that is already 
   in a connected state.
*/

void gzochid_reconnectable_socket_connect (gzochid_reconnectable_socket *,
					   gzochid_client_socket *);

/*
  Puts the specified reconnectable socket into a disconnected state, 
  invoking all registered `gzochid_reconnectable_socket_disconnected' callbacks
  before returning. 

  It is a programming error to disconnect a reconnectable socket that is already
  in a disconnected state.
*/

void gzochid_reconnectable_socket_disconnect (gzochid_reconnectable_socket *);

/*
  Writes a message to the specified reconnectable socket. If the socket is
  connected, this function delegates to a `gzochid_client_socket_write' call for
  the wrapped socket; otherwise, the message is buffered to be sent pending a
  call to `gzochid_reconnectable_socket_connect'.

  N.B. There is no bound on the size of the outbound message buffer.
*/

void gzochid_reconnectable_socket_write (gzochid_reconnectable_socket *,
					 unsigned char *, size_t);

/* Private server socket API, visible for testing only. */

/* Copies the actual socket address of the specified server socket to the 
   specified `sockaddr' struct. Semantics following the `getsockname' function
   in `<sys/socket.h>' - the length pointer indicates the available buffer in
   the address struct. It is updated to reflect the actual size of the address,
   which may be truncated as necessary. */

void _gzochid_server_socket_getsockname
(gzochid_server_socket *, struct sockaddr *, size_t *);

#endif /* GZOCHID_SOCKET_H */
