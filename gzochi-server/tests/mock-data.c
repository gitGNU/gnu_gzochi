/* mock-data.c: Test-time replacements for data.c routines.
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
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "data.h"
#include "io.h"

static guint64 next_oid;
static GHashTable *oids = NULL;
static GHashTable *oids_to_references = NULL;
static GHashTable *bindings = NULL;

static gzochid_data_managed_reference *
create_empty_reference (gzochid_application_context *context, guint64 oid, 
 gzochid_io_serialization *serialization)
{
  guint64 *oid_ptr = g_memdup (&oid, sizeof (guint64));
  gzochid_data_managed_reference *reference = 
    calloc (1, sizeof (gzochid_data_managed_reference));

  reference->context = context;
  reference->oid = oid;
  reference->state = GZOCHID_MANAGED_REFERENCE_STATE_EMPTY;
  reference->serialization = serialization;

  g_hash_table_insert (oids_to_references, oid_ptr, reference);
  
  return reference;
}

gzochid_data_managed_reference *
gzochid_data_create_reference_to_oid 
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 guint64 oid)
{
  gzochid_data_managed_reference *ref = NULL;
  
  if (g_hash_table_contains (oids_to_references, &oid))
    ref = g_hash_table_lookup (oids_to_references, &oid);
  else ref = create_empty_reference (context, oid, serialization);

  return ref;
}

gzochid_data_managed_reference *
gzochid_data_create_reference
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 gpointer data, GError **err)
{
  guint64 oid;
  gzochid_data_managed_reference *ref = NULL;

  oid = next_oid++;
      
  ref = create_empty_reference (context, oid, serialization);
  ref->obj = data;

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
(gzochid_application_context *context, char *name, guint64 oid, GError **error)
{
  guint64 *oid_ptr = g_memdup (&oid, sizeof (guint64));
  
  if (g_hash_table_contains (bindings, name))
    free (g_hash_table_lookup (bindings, name));

  g_hash_table_insert (bindings, name, oid_ptr);
}

void *
gzochid_data_dereference 
(gzochid_data_managed_reference *reference, GError **error)
{
  GByteArray *in = NULL;
  GString *data = NULL;

  if (reference->obj != NULL
      || reference->state == GZOCHID_MANAGED_REFERENCE_STATE_REMOVED_EMPTY)
    return reference->obj;

  data = g_hash_table_lookup (oids, &reference->oid); 

  in = g_byte_array_sized_new (data->len);
  g_byte_array_append (in, data->str, data->len);
  
  reference->obj = reference->serialization->deserializer 
    (reference->context, in, NULL);
  g_byte_array_unref (in);

  return reference->obj;
}

void *
gzochid_data_dereference_for_update
(gzochid_data_managed_reference *reference, GError **error)
{
  return gzochid_data_dereference (reference, error);
}

void
gzochid_test_mock_data_store
(gzochid_application_context *context, gzochid_io_serialization *serialization,
 gpointer data, guint64 oid)
{
  GByteArray *out = g_byte_array_new ();
  guint64 *oid_ptr = NULL;

  oid = next_oid++;
  oid_ptr = g_memdup (&oid, sizeof (guint64));
 
  serialization->serializer (context, data, out, NULL);
  g_hash_table_insert (oids, oid_ptr, out);
}

void 
gzochid_test_mock_data_initialize ()
{
  oids = g_hash_table_new_full (g_int64_hash, g_int64_equal, g_free, NULL);
  oids_to_references = g_hash_table_new_full
    (g_int64_hash, g_int64_equal, g_free, NULL);
  bindings = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
}
