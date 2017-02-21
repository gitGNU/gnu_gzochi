/* reloc.c: Support for assigning fixed native pointer locations to SCM ojects
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
#include <libguile.h>
#include <stdlib.h>

#include "app.h"
#include "reloc.h"
#include "scheme.h"
#include "tx.h"

static void 
transaction_context_free (gzochid_scm_location_transaction_context *context)
{
  g_hash_table_destroy (context->bits_cache);
  free (context);
}

static int 
scm_location_prepare (gpointer data)
{
  return TRUE;
}

static void 
scm_location_commit (gpointer data) 
{
  transaction_context_free (data);
}

static void 
scm_location_rollback (gpointer data)
{
  transaction_context_free (data);
}

static gzochid_transaction_participant scm_location_participant =
  { 
    "scm_location", 
    scm_location_prepare, 
    scm_location_commit, 
    scm_location_rollback
  };

static guint 
scm_bits_hash (gconstpointer key)
{
  const scm_t_bits *bits = key;

  return (guint) *bits;
}

static gboolean 
scm_bits_equal (gconstpointer a, gconstpointer b)
{
  const scm_t_bits *a_bits = a;
  const scm_t_bits *b_bits = b;

  return *a_bits == *b_bits;
}

static void 
free_bits (gpointer ptr)
{
  scm_t_bits *bits = ptr;
  scm_gc_unprotect_object (SCM_PACK (*bits));
  free (bits);
}

static gzochid_scm_location_transaction_context *
create_transaction_context (gzochid_application_context *app_context)
{
  gzochid_scm_location_transaction_context *tx_context =
    malloc (sizeof (gzochid_scm_location_transaction_context));

  tx_context->context = app_context;
  tx_context->bits_cache = g_hash_table_new_full 
    (scm_bits_hash, scm_bits_equal, free_bits, free);  
  
  return tx_context;
}

static void 
join_transaction (gzochid_application_context *context)
{
  if (!gzochid_transaction_active ()
      || gzochid_transaction_context (&scm_location_participant) == NULL)
    gzochid_transaction_join
      (&scm_location_participant, create_transaction_context (context));
}

static void 
location_aware_scheme_serializer 
(gzochid_application_context *context, void *ptr, GByteArray *out, GError **err)
{
  GError *local_err = NULL;

  gzochid_scm_location_info *location = ptr;
  SCM obj = gzochid_scm_location_resolve (context, location);

  gzochid_scheme_data_serialization.serializer (context, obj, out, &local_err);

  if (local_err != NULL)
    {
      /* Don't join the transaction here as the serializer should only be
	 called during the PREPARING phase, which is too late to join. */

      gzochid_transaction_mark_for_rollback 
	(&scm_location_participant, 
	 local_err->code == GZOCHID_SCHEME_ERROR_RETRY);

      g_propagate_error (err, local_err);
    }
}

static void *
location_aware_scheme_deserializer
(gzochid_application_context *context, GByteArray *in, GError **err)
{
  GError *local_err = NULL;
  SCM obj = gzochid_scheme_data_serialization.deserializer 
    (context, in, &local_err);

  if (local_err != NULL)
    {
      if (gzochid_transaction_rollback_only ())
	{
	  join_transaction (context);
	  gzochid_transaction_mark_for_rollback 
	    (&scm_location_participant, 
	     local_err->code == GZOCHID_SCHEME_ERROR_RETRY);
	}

      g_propagate_error (err, local_err);
      return obj;
    }
  else return gzochid_scm_location_get (context, obj);
}

static void 
location_aware_scheme_finalizer (gzochid_application_context *context, 
				 void *ptr)
{
}

gzochid_io_serialization gzochid_scm_location_aware_serialization = 
  { 
    location_aware_scheme_serializer, 
    location_aware_scheme_deserializer, 
    location_aware_scheme_finalizer
  };

gzochid_scm_location_info *
gzochid_scm_location_get (gzochid_application_context *context, SCM obj)
{
  gzochid_scm_location_transaction_context *tx_context = NULL;
  scm_t_bits bits = SCM_UNPACK (obj);

  join_transaction (context);
  tx_context = gzochid_transaction_context (&scm_location_participant);

  if (g_hash_table_contains (tx_context->bits_cache, &bits))
    return g_hash_table_lookup (tx_context->bits_cache, &bits);
  else
    {
      scm_t_bits *key = malloc (sizeof (scm_t_bits));
      gzochid_scm_location_info *value = 
	malloc (sizeof (gzochid_scm_location_info));

      /* The object may be getting protected in other places as well, but if
	 it's going until the table, it's gotta get protected because the
	 table's going to unprotect it at the end of the transaction. */
      
      scm_gc_protect_object (obj);
      
      *key = bits;
      value->bits = bits;

      g_hash_table_insert (tx_context->bits_cache, key, value);

      return value;
    }
}
 
SCM 
gzochid_scm_location_resolve 
(gzochid_application_context *context, gzochid_scm_location_info *location)
{
  return SCM_PACK (location->bits);
}
