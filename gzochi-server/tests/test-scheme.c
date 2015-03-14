/* test-scheme.c: Test routines for scheme.c in gzochid.
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

#include <glib.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "mock-data.h"
#include "scheme.h"

static void
test_serialization ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  GString *str = g_string_new ("");
  GError *err = NULL;
  gpointer data = NULL;

  context->descriptor = calloc (1, sizeof (gzochid_application_descriptor));
  context->descriptor->deployment_root = "/";

  gzochid_util_serialize_string ("test-unknown-type", str);
  data = gzochid_scheme_data_serialization.deserializer (context, str, &err);

  g_assert (data == SCM_BOOL_F);
  g_assert_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED);
}

static void
inner_main (void *data, int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/scheme/serialization/exception", test_serialization);

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
