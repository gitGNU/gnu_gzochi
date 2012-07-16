/* channel.c: Primitive functions for user-facing gzochid channel API
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
#include "../channel.h"
#include "../data.h"
#include "../scheme.h"
#include "../session.h"

#include "channel.h"
#include "util.h"

SCM_DEFINE (primitive_create_channel, "primitive-create-channel", 1, 0, 0,
	    (SCM name), "Create a new channel with the specified name.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  char *cname = scm_to_locale_string (name);
  gzochid_channel *channel = gzochid_channel_create (context, cname);
  gzochid_data_managed_reference *scm_reference = 
    gzochid_data_create_reference_to_oid 
    (context, &gzochid_scheme_data_serialization, channel->scm_oid);

  gzochid_data_dereference (scm_reference);

  return (SCM) scm_reference->obj;
}

SCM_DEFINE (primitive_get_channel, "primitive-get-channel", 1, 0, 0,
	    (SCM name), "Retrieves the channel with the specified name.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  char *cname = scm_to_locale_string (name);
  gzochid_channel *channel = gzochid_channel_get (context, cname);

  free (cname);
  if (channel == NULL)
    return SCM_BOOL_F;
  else
    {
      gzochid_data_managed_reference *scm_reference = 
	gzochid_data_create_reference_to_oid 
	(context, &gzochid_scheme_data_serialization, channel->scm_oid);
      gzochid_data_dereference (scm_reference);
      return (SCM) scm_reference->obj;
    }
}

SCM_DEFINE (primitive_join_channel, "primitive-join-channel", 2, 0, 0,
	    (SCM channel, SCM session), "Add client session to a channel.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  mpz_t channel_oid, session_oid;

  mpz_init (channel_oid);
  mpz_init (session_oid);
  
  gzochid_scheme_channel_oid (channel, channel_oid);
  gzochid_scheme_client_session_oid (session, session_oid);
  
  channel_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, channel_oid);
  session_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, session_oid);
  gzochid_data_dereference (channel_reference);
  gzochid_data_dereference (session_reference);

  gzochid_channel_join 
    (context, 
     (gzochid_channel *) channel_reference->obj, 
     (gzochid_client_session *) session_reference->obj);

  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_leave_channel, "primitive-leave-channel", 2, 0, 0,
	    (SCM channel, SCM session), 
	    "Remove a client session from a channel.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *channel_reference = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  mpz_t channel_oid, session_oid;

  mpz_init (channel_oid);
  mpz_init (session_oid);
  
  gzochid_scheme_channel_oid (channel, channel_oid);
  gzochid_scheme_client_session_oid (session, session_oid);
  
  channel_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, channel_oid);
  session_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, session_oid);
  gzochid_data_dereference (channel_reference);
  gzochid_data_dereference (session_reference);

  gzochid_channel_leave 
    (context, 
     (gzochid_channel *) channel_reference->obj, 
     (gzochid_client_session *) session_reference->obj);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_send_channel_message, "primitive-send-channel-message", 
	    2, 0, 0, (SCM channel, SCM bv), 
	    "Send a message to the sessions that belong to a channel.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();

  short len = (short) SCM_BYTEVECTOR_LENGTH (bv);
  unsigned char *msg = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);

  gzochid_data_managed_reference *channel_reference = NULL;
  mpz_t channel_oid;

  mpz_init (channel_oid);
  
  gzochid_scheme_channel_oid (channel, channel_oid);
  
  channel_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, channel_oid);
  gzochid_data_dereference (channel_reference);

  gzochid_channel_send 
    (context, (gzochid_channel *) channel_reference->obj, msg, len);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (primitive_close_channel, "primitive-close-channel", 1, 0, 0,
	    (SCM channel, SCM msg), "Shut down a channel.")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  gzochid_data_managed_reference *channel_reference = NULL;
  mpz_t channel_oid;

  mpz_init (channel_oid);
  gzochid_scheme_channel_oid (channel, channel_oid);  
  channel_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_channel_serialization, channel_oid);
  gzochid_data_dereference (channel_reference);

  gzochid_channel_close (context, (gzochid_channel *) channel_reference->obj);
  return SCM_UNSPECIFIED;
}

void gzochid_api_channel_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_channel = scm_c_resolve_module ("gzochi private channel");
  scm_set_current_module (gzochi_private_channel);

  #include "channel.x"

  scm_set_current_module (current_module);
}
