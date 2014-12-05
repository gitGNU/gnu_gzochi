/* test-reloc.c: Test routines for reloc.c in gzochid.
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
#include <libguile.h>
#include <stddef.h>

#include "app.h"
#include "io.h"
#include "reloc.h"
#include "tx.h"

static int prepare (gpointer data) { return TRUE; }
static void commit (gpointer data) { }
static void rollback (gpointer data) { }

gzochid_transaction_participant test_participant = 
  { "test", prepare, commit, rollback };

static void
serializer 
(gzochid_application_context *context, void *obj, GString *str, GError **err)
{
}

static void *
deserializer (gzochid_application_context *context, GString *str, GError **err)
{
  gzochid_transaction_join (&test_participant, NULL);
  gzochid_transaction_mark_for_rollback (&test_participant, TRUE);
  g_set_error (err, GZOCHID_IO_ERROR, GZOCHID_IO_ERROR_SERIALIZATION,
	       "Failed to deserialize managed record.");
  return SCM_BOOL_F;
}

static void
finalizer (gzochid_application_context *context, void *obj)
{
}

gzochid_io_serialization gzochid_scheme_data_serialization = 
  {
    serializer,
    deserializer,
    finalizer
  };

static void
test_deserializer_error_inner (gpointer data)
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GString *str = g_string_new ("");

  gzochid_scm_location_aware_serialization.deserializer (context, str, NULL);
}

static void
test_deserializer_error ()
{
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
