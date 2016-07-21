/* oids-storage.c: A storage-based oid block allocation strategy for gzochid
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "gzochid-storage.h"
#include "oids.h"
#include "oids-storage.h"
#include "util.h"

#define ALLOCATION_BLOCK_SIZE 100
#define NEXT_OID_KEY 0x00

struct _gzochid_oid_storage_context
{
  gzochid_storage_engine_interface *engine;
  gzochid_storage_context *storage_context;
  gzochid_storage_store *store;
};

typedef struct _gzochid_oid_storage_context gzochid_oid_storage_context;

static gboolean
allocate (gpointer user_data, gzochid_data_oids_block *ret, GError **err)
{
  gzochid_oid_storage_context *context = user_data;
  
  char next_oid_key[] = { NEXT_OID_KEY };
  char *next_oid_value = NULL;
  size_t next_oid_value_len = 0;
  guint64 encoded_oid = 0, block_start = 0, next_block = 0;
  
  gzochid_storage_transaction *transaction = context->engine->transaction_begin
    (context->storage_context);

  if (transaction == NULL)
    {
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Failed to create object id allocation transaction.");
      return FALSE;
    }

  /* This is the step of the reservation transaction most likely 
     (though still not very likely) to fail. */
  
  next_oid_value = context->engine->transaction_get_for_update
    (transaction, context->store, next_oid_key, 1, &next_oid_value_len);

  if (transaction->rollback)
    {
      context->engine->transaction_rollback (transaction);
      
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Transaction rollback while allocating object ids.");

      return FALSE;
    }
  
  if (next_oid_value != NULL)
    {
      assert (next_oid_value_len == sizeof (guint64));

      memcpy (&encoded_oid, next_oid_value, sizeof (guint64));
      block_start = gzochid_util_decode_oid (encoded_oid);
      
      free (next_oid_value);
    }
  else block_start = 0;

  next_block = block_start + ALLOCATION_BLOCK_SIZE;
  encoded_oid = gzochid_util_encode_oid (next_block);
  
  context->engine->transaction_put
    (transaction, context->store, next_oid_key, 1, (char *) &encoded_oid,
     sizeof (guint64));
  
  if (!transaction->rollback)
    context->engine->transaction_prepare (transaction);
  if (transaction->rollback)
    {
      context->engine->transaction_rollback (transaction);
  
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Transaction rollback while allocating object ids.");

      return FALSE;
    }

  context->engine->transaction_commit (transaction);  

  /* Transfer the block information to the target variable. */

  ret->block_start = block_start;
  ret->block_size = ALLOCATION_BLOCK_SIZE;

  return TRUE;
}

gzochid_oid_allocation_strategy *
gzochid_storage_oid_strategy_new (gzochid_storage_engine_interface *engine,
				  gzochid_storage_context *storage_context,
				  gzochid_storage_store *store)
{
  gzochid_oid_storage_context *context =
    malloc (sizeof (gzochid_oid_storage_context));

  context->engine = engine;
  context->storage_context = storage_context;
  context->store = store;
  
  return gzochid_oid_allocation_strategy_new (allocate, context, free);
}
