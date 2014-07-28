/* mock-session.c: Test-time replacements for session.c routines.
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

#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>

#include "../app.h"
#include "../auth.h"
#include "../auth_int.h"
#include "../session.h"
#include "../util.h"

gzochid_client_session *gzochid_client_session_new 
(gzochid_auth_identity *identity)
{
  gzochid_client_session *session = (gzochid_client_session *)
    calloc (1, sizeof (gzochid_client_session));

  session->identity = gzochid_auth_identity_clone (identity);
  session->channels = g_sequence_new (NULL);
  mpz_init (session->scm_oid);

  return session;
}

static void serialize_handler 
(gzochid_application_context *context, gzochid_client_session_handler *handler,
 GString *out, GError **err)
{
  gzochid_application_callback_serialization.serializer 
    (context, handler->received_message, out, NULL);
  gzochid_application_callback_serialization.serializer 
    (context, handler->disconnected, out, NULL);
}

static void serialize_client_session 
(gzochid_application_context *context, gpointer obj, GString *out, GError **err)
{
  gzochid_client_session *session = (gzochid_client_session *) obj;

  gzochid_auth_identity_serializer (context, session->identity, out, NULL);
  gzochid_util_serialize_sequence 
    (session->channels, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);
  gzochid_util_serialize_mpz (session->scm_oid, out);
  if (session->handler != NULL)
    {
      gzochid_util_serialize_boolean (TRUE, out);
      serialize_handler (context, session->handler, out, NULL);
    }
  else gzochid_util_serialize_boolean (FALSE, out);
}


static gzochid_client_session_handler *deserialize_handler 
(gzochid_application_context *context, GString *in, GError **err)
{
  gzochid_client_session_handler *handler = malloc 
    (sizeof (gzochid_client_session_handler)); 

  handler->received_message = 
    gzochid_application_callback_serialization.deserializer (context, in, NULL);
  handler->disconnected = 
    gzochid_application_callback_serialization.deserializer (context, in, NULL);

  return handler;
}

static gpointer deserialize_client_session
(gzochid_application_context *context, GString *in, GError **err)
{
  gzochid_auth_identity *identity = 
    gzochid_auth_identity_deserializer (context, in, NULL);
  GSequence *channels = gzochid_util_deserialize_sequence 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string, free);
  gzochid_client_session *session = gzochid_client_session_new (identity);

  g_sequence_free (session->channels);
  session->channels = channels;

  gzochid_util_deserialize_mpz (in, session->scm_oid);

  if (gzochid_util_deserialize_boolean (in))
    session->handler = deserialize_handler (context, in, NULL);

  return session;
}

gzochid_io_serialization gzochid_client_session_serialization = 
  { 
    serialize_client_session, 
    deserialize_client_session, 
    NULL
  };

void
gzochid_client_session_send_login_success
(gzochid_application_context *context, gzochid_client_session *session)
{
}

void
gzochid_client_session_send_login_failure
(gzochid_application_context *context, gzochid_client_session *session)
{
}

void
gzochid_client_session_disconnect
(gzochid_application_context *context, gzochid_client_session *session)
{
}

void
gzochid_client_session_disconnected_worker
(gzochid_application_context *context, gzochid_auth_identity *identity,
 gpointer data)
{
}
