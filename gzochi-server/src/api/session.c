/* session.c: Primitive functions for user-facing gzochid client session API
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

#include <libguile.h>

#include "../app.h"
#include "../data.h"
#include "../session.h"
#include "../scheme.h"

#include "session.h"
#include "util.h"

SCM_DEFINE (primitive_send_message, "primitive-send-message", 2, 0, 0,
	    (SCM session, SCM msg), "Send a message to a client session.")
{
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *reference = NULL;
  
  short len = (short) SCM_BYTEVECTOR_LENGTH (msg);
  unsigned char *payload = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (msg);
  mpz_t c_oid;

  mpz_init (c_oid);
  gzochid_scheme_client_session_oid (session, c_oid);
  reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, c_oid);
  gzochid_data_dereference (reference);
 
  gzochid_client_session_send_message 
    (context, (gzochid_client_session *) reference->obj, payload, len);

  mpz_clear (c_oid);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_disconnect, "primitive-disconnect", 1, 0, 0,
	    (SCM session), "Disconnect a client session.")
{
  gzochid_application_context *context =
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *reference = NULL;  
  mpz_t c_oid;

  mpz_init (c_oid);
  gzochid_scheme_client_session_oid (session, c_oid);
  reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, c_oid);
  gzochid_data_dereference (reference);
 
  gzochid_client_session_disconnect 
    (context, (gzochid_client_session *) reference->obj);

  mpz_clear (c_oid);
  return SCM_UNSPECIFIED;
}

void gzochid_api_session_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_client = scm_c_resolve_module ("gzochi private client");
  scm_set_current_module (gzochi_private_client);

  #include "session.x"

  scm_set_current_module (current_module);
}
