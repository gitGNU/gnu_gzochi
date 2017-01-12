/* test-reloc.c: Test routines for reloc.c in gzochid.
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
#include "gzochid-auth.h"
#include "io.h"
#include "reloc.h"
#include "scheme.h"
#include "session.h"
#include "tx.h"

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

static int prepare (gpointer data) { return TRUE; }
static void commit (gpointer data) { }
static void rollback (gpointer data) { }

gzochid_transaction_participant test_participant = 
  { "test", prepare, commit, rollback };

static void
serializer 
(gzochid_application_context *context, void *obj, GByteArray *str, GError **err)
{
}

static void *
deserializer (gzochid_application_context *context, GByteArray *str,
	      GError **err)
{
  gzochid_transaction_join (&test_participant, NULL);
  gzochid_transaction_mark_for_rollback (&test_participant, TRUE);
  g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_RETRY,
	       "Failed to deserialize managed record.");
  return SCM_BOOL_F;
}

static void
finalizer (gzochid_application_context *context, void *obj)
{
}

static void
test_deserializer_error_inner (gpointer data)
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GByteArray *str = g_byte_array_new ();

  gzochid_scm_location_aware_serialization.deserializer (context, str, NULL);
  g_assert (gzochid_transaction_rollback_only ());

  gzochid_application_context_free (context);
  g_byte_array_unref (str);
}

static void
test_deserializer_error ()
{
  gzochid_scheme_data_serialization.serializer = serializer;
  gzochid_scheme_data_serialization.deserializer = deserializer;
  gzochid_scheme_data_serialization.finalizer = finalizer;
  
  gzochid_transaction_execute (test_deserializer_error_inner, NULL);
}

static void
inner_main (void *data, int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func 
    ("/reloc/serialization/deserializer/error", test_deserializer_error);

  exit (g_test_run ());
}

int main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, NULL);

  return 0;
}
