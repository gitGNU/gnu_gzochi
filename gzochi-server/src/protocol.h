/* protocol.h: Struct definitions for protocol descriptors.
 * Copyright (C) 2015 Julian Graham
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

#ifndef GZOCHID_PROTOCOL_H
#define GZOCHID_PROTOCOL_H

#include <glib.h>
#include <sys/socket.h>

/* Describes a server protocol, used to create `gzochid_client_socket' instances
   out of incoming connections on a listening `gzochid_server_socket'. */

struct _gzochid_server_protocol
{
  /* Return a new client socket structure from the specified IO Channel and a
     `NULL'-terminated connection description string, which can be used in log
     messages about the connection. 

     Implementations of this function should almost always call 
     `gzochid_client_socket_new', which has a matching prototype.
  */
  
  struct _gzochid_client_socket *(*accept)
    (GIOChannel *, const char *, gpointer);
};

typedef struct _gzochid_server_protocol gzochid_server_protocol;

/* Describes a client protocol, used to handle incoming messages and state 
   changes from connected client sockets. */

struct _gzochid_client_protocol
{
  /* Return `TRUE' if the specified byte array contains at least one message 
     that can be consumed and dispatched via `dispatch'. This function will be
     called by the socket server when data has arrived from a client socket to 
     check whether any messages are ready to be dispatched. */
  
  gboolean (*can_dispatch) (const GByteArray *, gpointer);

  /* Read one or more messages from the specified byte array and dispatch it
     accordingly, returning the number of bytes that have been consumed. This
     function will be called if and only if the `can_dispatch' function returns
     `TRUE'. */
  
  unsigned int (*dispatch) (const GByteArray *, gpointer);

  /* Called when the socket server detects an error or unexpected disconnect
     from a connected client. */
  
  void (*error) (gpointer);

  /* Called to release any resources (e.g., memory) referenced by the protocol 
     data object. */
  
  void (*free) (gpointer);
};

typedef struct _gzochid_client_protocol gzochid_client_protocol;

#endif /* GZOCHID_PROTOCOL_H */
