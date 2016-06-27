/* mock-data.c: Test-time replacements for data.c routines.
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

#include <glib.h>
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "data.h"
#include "io.h"

static mpz_t next_oid;
static GHashTable *oids = NULL;
static GHashTable *oids_to_references = NULL;
static GHashTable *bindings = NULL;

static gzochid_data_managed_reference *
create_empty_reference (gzochid_application_context *context, mpz_t oid, 
 gzochid_io_serialization *serialization)
{
  gzochid_data_managed_reference *reference = 
    calloc (1, sizeof (gzochid_data_managed_reference));

  reference->context = context;

  mpz_init (reference->oid);
  mpz_set (reference->oid, oid);
 
  reference->state = GZOCHID_MANAGED_REFERENCE_STATE_EMPTY;
  reference->serialization = serialization;

  g_hash_table_insert
    (oids_to_references, mpz_get_str (NULL, 16, oid), reference);
  
  return reference;
}

gzochid_data_managed_reference *
gzochid_data_create_reference_to_oid 
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 mpz_t oid)
{
  char *oid_str = mpz_get_str (NULL, 16, oid);
  gzochid_data_managed_reference *ref = NULL;
  
  if (g_hash_table_contains (oids_to_references, oid_str))
    ref = g_hash_table_lookup (oids_to_references, oid_str);
  else ref = create_empty_reference (context, oid, serialization);

  free (oid_str);
  return ref;
}

gzochid_data_managed_reference *
gzochid_data_create_reference
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 gpointer data, GError **err)
{
  mpz_t oid;
  gzochid_data_managed_reference *ref = NULL;
  
  mpz_init_set (oid, next_oid);
  mpz_add_ui (next_oid, next_oid, 1);
      
  ref = create_empty_reference (context, oid, serialization);
  ref->obj = data;

  mpz_clear (oid);
  return ref;
}

void
gzochid_data_mark 
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 gpointer data, GError **error)
{
}

void
gzochid_data_remove_object (gzochid_data_managed_reference *ref, GError **err)
{
}

void 
gzochid_data_set_binding_to_oid 
(gzochid_application_context *context, char *name, mpz_t oid, GError **error)
{
  char *oid_str = mpz_get_str (NULL, 16, oid);
  gboolean can_free = g_hash_table_contains (bindings, oid_str);

  g_hash_table_insert (bindings, name, oid_str);

  if (can_free)
    free (oid_str);
}

void *
gzochid_data_dereference 
(gzochid_data_managed_reference *reference, GError **error)
{
  GString *in = NULL;
  GString *data = NULL;
  char *oid_str = NULL;

  if (reference->obj != NULL
      || reference->state == GZOCHID_MANAGED_REFERENCE_STATE_REMOVED_EMPTY)
    return reference->obj;

  oid_str = mpz_get_str (NULL, 16, reference->oid);
  data = g_hash_table_lookup (oids, oid_str); 
  free (oid_str);

  in = g_string_new_len (data->str, data->len);
  reference->obj = reference->serialization->deserializer 
    (reference->context, in, NULL);
  g_string_free (in, TRUE);

  return reference->obj;
}

void
gzochid_test_mock_data_store
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 gpointer data, mpz_t oid)
{
  GString *out = g_string_new (NULL);
  
  mpz_set (oid, next_oid);
  mpz_add_ui (next_oid, next_oid, 1);
 
  serialization->serializer (context, data, out, NULL);
  g_hash_table_insert (oids, mpz_get_str (NULL, 16, oid), out);
}

void 
gzochid_test_mock_data_initialize ()
{
  mpz_init (next_oid);
  oids = g_hash_table_new_full (g_str_hash, g_str_equal, free, NULL);
  oids_to_references = g_hash_table_new_full
    (g_str_hash, g_str_equal, free, NULL);
  bindings = g_hash_table_new_full (g_str_hash, g_str_equal, free, NULL);
}
