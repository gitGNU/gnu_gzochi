/* test-data.c: Test routines for data.c in gzochid.
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

#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "../data.h"
#include "../tx.h"

static gboolean serialized = FALSE;
static gboolean deserialized = FALSE;
static gboolean finalized = FALSE;

static void test_serializer 
(gzochid_application_context *context, void *ptr, GString *out)
{
  serialized = TRUE;
}

static void *test_deserializer 
(gzochid_application_context *context, GString *in)
{
  deserialized = TRUE;
  return g_string_new_len (in->str, in->len);
}

static void test_finalizer 
(gzochid_application_context *context, void *ptr)
{
  g_string_free ((GString *) ptr, TRUE);
  finalized = TRUE;
}

gzochid_io_serialization test_serialization = 
  { test_serializer, test_deserializer, test_finalizer };

static void reset_serialization_state ()
{
  serialized = FALSE;
  deserialized = FALSE;
  finalized = FALSE;
}

static void fetch_reference (gpointer data)
{
  mpz_t z;
  gzochid_application_context *context = (gzochid_application_context *) data;
  gzochid_data_managed_reference *ref = NULL;

  mpz_init (z);

  ref = gzochid_data_create_reference_to_oid (context, &test_serialization, z);
  gzochid_data_dereference (ref);

  mpz_clear (z);
}

static void test_data_reference_finalize ()
{
  mpz_t z;
  char *key = NULL;
  gzochid_application_context *context = gzochid_application_context_new ();

  reset_serialization_state ();

  mpz_init (z);
  key = mpz_get_str (NULL, 16, z);

  gzochid_storage_put 
    (context->oids, key, strlen (key) + 1, "foo", 4);

  free (key);
  mpz_clear (z);

  gzochid_transaction_execute (fetch_reference, context);

  g_assert (!serialized);
  g_assert (deserialized);
  g_assert (finalized);
}

int main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/data/reference/finalize", test_data_reference_finalize);

  return g_test_run ();
}
