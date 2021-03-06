/* test-scheme.c: Test routines for scheme.c in gzochid.
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

#include <glib.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "channel.h"
#include "descriptor.h"
#include "guile.h"
#include "gzochid-auth.h"
#include "scheme.h"
#include "session.h"
#include "util.h"

/* Fake implementations to avoid having to pull in `app.o'. */

static gzochid_application_context *current_app_context = NULL;

gzochid_application_context *
gzochid_application_context_new ()
{
  return calloc (1, sizeof (gzochid_application_context));
}

void
gzochid_application_context_free (gzochid_application_context *app_context)
{
  free (app_context);
}

void *
gzochid_with_application_context (gzochid_application_context *app_context,
				  gzochid_auth_identity *identity,
				  void *(*thunk) (gpointer), gpointer data)
{
  gpointer ret = NULL;
  
  current_app_context = app_context;
  ret = thunk (data);
  current_app_context = NULL;
  
  return ret;
}

/* Fake implementation to avoid having to pull in `channel.o'. */

const char *
gzochid_channel_name (gzochid_channel *channel)
{
  return NULL;
}

/* Fake implementation to avoid having to pull in `session.o'. */

gzochid_auth_identity *
gzochid_client_session_identity (gzochid_client_session *session)
{
  return NULL;
}

static void
test_serialization_exception ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GByteArray *str = g_byte_array_new ();
  GError *err = NULL;

  context->descriptor = g_object_new
    (GZOCHID_TYPE_APPLICATION_DESCRIPTOR, NULL);
  context->deployment_root = "/";
  
  gzochid_util_serialize_string ("test-unknown-type", str);

  g_assert
    (scm_is_false
     (gzochid_scheme_data_serialization.deserializer (context, str, &err)));

  g_assert_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED);
  g_clear_error (&err);

  g_object_unref (context->descriptor);
  gzochid_application_context_free (context);
  g_byte_array_unref (str);
}

static void
test_serialization_corrupt ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GByteArray *str = g_byte_array_new ();
  GError *err = NULL;

  gzochid_util_serialize_int (16, str);  

  g_assert
    (scm_is_false
     (gzochid_scheme_data_serialization.deserializer (context, str, &err)));

  g_assert_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_SERIAL);
  g_clear_error (&err);

  gzochid_application_context_free (context);
  g_byte_array_unref (str);
}

static void
inner_main (void *data, int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func
    ("/scheme/serialization/exception", test_serialization_exception);
  g_test_add_func ("/scheme/serialization/corrupt", test_serialization_corrupt);

  gzochid_guile_init ();
  gzochid_scheme_initialize_bindings ();
  exit (g_test_run ());
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);

  return 0;
}
