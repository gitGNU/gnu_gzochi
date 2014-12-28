/* protocol.h: Prototypes and declarations for protocol.c
 * Copyright (C) 2013 Julian Graham
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
#include <gmp.h>
#include <libserveez.h>

#include "app.h"
#include "auth.h"
#include "socket.h"

typedef struct _gzochid_protocol_client
{
  gzochid_application_context *context;
  gzochid_auth_identity *identity;
  mpz_t oid;
  
  gboolean disconnected;
  GMutex sock_mutex;
  int (*write_socket) (svz_socket_t *);
  svz_socket_t *sock;
  char *connection_description;
} gzochid_protocol_client;

gzochid_protocol_client *gzochid_protocol_client_accept (svz_socket_t *);
void gzochid_protocol_client_disconnected (gzochid_protocol_client *);
void gzochid_protocol_client_free (gzochid_protocol_client *);
void gzochid_protocol_client_dispatch 
gzochid_auth_identity *
gzochid_protocol_client_get_identity (gzochid_protocol_client *);

(gzochid_protocol_client *, unsigned char *, short);

void gzochid_protocol_client_disconnect (gzochid_protocol_client *);
void gzochid_protocol_client_login_success (gzochid_protocol_client *);
void gzochid_protocol_client_login_failure (gzochid_protocol_client *);
void gzochid_protocol_client_send 
(gzochid_protocol_client *, unsigned char *, short);

#endif /* GZOCHID_PROTOCOL_H */
