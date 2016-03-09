/* oids.c: An object id block allocation strategy for gzochid
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
#include <string.h>

#include "gzochid-storage.h"
#include "oids.h"

#define ALLOCATION_BLOCK_SIZE 100
#define NEXT_OID_KEY 0x00

GQuark
gzochid_oids_error_quark ()
{
  return g_quark_from_static_string ("gzochid-oids-error-quark");
}

gboolean
gzochid_oids_reserve_block (gzochid_storage_engine_interface *engine,
			    gzochid_storage_context *context,
			    gzochid_storage_store *store,
			    gzochid_data_oids_block *ret, GError **err)
{
  char next_oid_key[] = { NEXT_OID_KEY };
  char *next_oid_value = NULL;

  /* Because initializing an `mpz_t' requires modifying an opaque structure 
     "in place," all of the intermediate computation is done on local variables
     that are only transferred to the target `gzochid_data_oids_block' if the
     reservation transaction is successful. */
  
  mpz_t block_start, next_block;

  gzochid_storage_transaction *transaction = engine->transaction_begin
    (store->context);

  if (transaction == NULL)
    {
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Failed to create object id allocation transaction.");
      return FALSE;
    }

  /* This is the step of the reservation transaction most likely 
     (though still not very likely) to fail. */
  
  next_oid_value = engine->transaction_get_for_update
    (transaction, store, next_oid_key, 1, NULL);

  if (transaction->rollback)
    {
      engine->transaction_rollback (transaction);
      
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Transaction rollback while allocating object ids.");

      return FALSE;
    }
  
  mpz_init (block_start);
  
  if (next_oid_value != NULL)
    {
      mpz_set_str (block_start, next_oid_value, 16);
      free (next_oid_value);
    }

  mpz_init (next_block);
  mpz_add_ui (next_block, block_start, ALLOCATION_BLOCK_SIZE);
  next_oid_value = mpz_get_str (NULL, 16, next_block);
  
  engine->transaction_put
    (transaction, store, next_oid_key, 1, next_oid_value,
     strlen (next_oid_value) + 1);

  free (next_oid_value);  
  mpz_clear (next_block);
  
  if (!transaction->rollback)
    engine->transaction_prepare (transaction);
  if (transaction->rollback)
    {
      engine->transaction_rollback (transaction);
      mpz_clear (block_start);

      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Transaction rollback while allocating object ids.");

      return FALSE;
    }

  engine->transaction_commit (transaction);  

  /* Transfer the block information to the target variable. */
  
  mpz_init (ret->block_start);
  mpz_set (ret->block_start, block_start);
  ret->block_size = ALLOCATION_BLOCK_SIZE;

  mpz_clear (block_start);
  return TRUE;
}
