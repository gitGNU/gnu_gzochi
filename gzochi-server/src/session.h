/* session.h: Prototypes and declarations for session.c
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

#ifndef GZOCHID_SESSION_H
#define GZOCHID_SESSION_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "data.h"
#include "gzochid-auth.h"
#include "io.h"

#define GZOCHID_SESSION_ERROR gzochid_session_error_quark ()

GQuark gzochid_session_error_quark (void);

enum _GzochidSessionError
  {
    GZOCHID_SESSION_ERROR_DISCONNECT,
    GZOCHID_SESSION_ERROR_FAILED
  };

typedef enum _GzochidSessionError GzochidSessionError;
  
struct _gzochid_client_session_handler
{
  gzochid_application_callback *received_message;
  gzochid_application_callback *disconnected;
};

typedef struct _gzochid_client_session_handler gzochid_client_session_handler;

typedef struct _gzochid_client_session gzochid_client_session;

extern gzochid_io_serialization gzochid_client_session_serialization;

/* Transactional application worker that removes the persisted session 
   corresponding to the specified oid string, as well as its binding in the
   "names" database. The Scheme representation of the session is also removed if
   it exists. */

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

gzochid_auth_identity *gzochid_client_session_identity
(gzochid_client_session *);

/* Sets the specified `mpz_t' to the oid of the specified session's Scheme
   representation. */

void gzochid_client_session_scm_oid (gzochid_client_session *, mpz_t);

/* Sets the oid of the specified session's Scheme representation to the 
   specified `mpz_t'. */

void gzochid_client_session_set_scm_oid (gzochid_client_session *, mpz_t);

/* Sets the specified `mpz_t' to the oid of the Scheme representation of the 
   specified session's session listener. */

void gzochid_client_session_handler_scm_oid (gzochid_client_session *, mpz_t);

/* Sets the specified `mpz_t' to the oid of the Scheme representation of the 
   specified session's session listener. */

void gzochid_client_session_set_handler_scm_oid
(gzochid_client_session *, mpz_t);

/* Returns the lifecycle handler of the specified channel. */

gzochid_client_session_handler *gzochid_client_session_get_handler
(gzochid_client_session *);

/* Sets the lifecycle handler for the specified channel. */

void gzochid_client_session_set_handler
(gzochid_client_session *, gzochid_client_session_handler *);

void gzochid_client_session_persist 
(gzochid_application_context *, gzochid_client_session *, mpz_t, GError **);
void gzochid_sweep_client_sessions (gzochid_application_context *, GError **);

#endif /* GZOCHID_SESSION_H */
