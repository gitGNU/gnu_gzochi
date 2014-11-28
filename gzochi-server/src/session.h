/* session.h: Prototypes and declarations for session.c
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

#ifndef GZOCHID_SESSION_H
#define GZOCHID_SESSION_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "auth.h"
#include "data.h"
#include "io.h"

typedef struct _gzochid_client_session_handler
{
  gzochid_application_callback *received_message;
  gzochid_application_callback *disconnected;
} gzochid_client_session_handler;

typedef struct _gzochid_client_session
{
  gzochid_auth_identity *identity;
  gzochid_client_session_handler *handler;
  gboolean connected;

  GSequence *channels;

  mpz_t scm_oid;
} gzochid_client_session;

extern gzochid_io_serialization gzochid_client_session_serialization;

void gzochid_client_session_disconnected_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

gzochid_client_session *gzochid_client_session_new (gzochid_auth_identity *);
void gzochid_client_session_free (gzochid_client_session *);
void gzochid_client_session_disconnect 
(gzochid_application_context *, gzochid_client_session *);
void gzochid_client_session_send_login_success 
(gzochid_application_context *, gzochid_client_session *);
void gzochid_client_session_send_login_failure 
(gzochid_application_context *, gzochid_client_session *);
void gzochid_client_session_send_message 
(gzochid_application_context *, gzochid_client_session *, unsigned char *, 
 short);

void gzochid_client_session_persist 
(gzochid_application_context *, gzochid_client_session *, mpz_t);
void gzochid_sweep_client_sessions 
(gzochid_application_context *, gzochid_auth_identity *);

#endif /* GZOCHID_SESSION_H */
