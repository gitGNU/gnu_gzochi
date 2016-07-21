/* oids.h: Prototypes and declarations for oids.c
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

#ifndef GZOCHID_OIDS_H
#define GZOCHID_OIDS_H

#include <glib.h>
#include <stddef.h>

/* The following data structures and prototypes provide a framework for 
   implementations of object id allocation strategies. */

enum GzochidOidError
  {
    /* An object id allocation failure related to a failure in the underlying
       storage transcation. */
    
    GZOCHID_OIDS_ERROR_TRANSACTION,

    GZOCHID_OIDS_ERROR_FAILED  /* Generic object id allocation failure. */
  };

/* The error domain for object id allocation errors. Error codes for errors in
   this domain will be values from the `GzochidOidError' enum above. */

#define GZOCHID_OIDS_ERROR gzochid_oids_error_quark ()

/* 
   A block of reserved object ids. 

   The ids in this block begin at the value given by `block_start' and proceed,
   incrementing by 1, for `block_size' values. For example, for a block of size
   100 starting at 0, the available values are 0 through 99, inclusive.
*/

struct _gzochid_data_oids_block
{
  guint64 block_start; /* The first object id in the block. */
  size_t block_size; /* The size of the block. */
};

typedef struct _gzochid_data_oids_block gzochid_data_oids_block;

/* The opaque oid allocation strategy type. */

typedef struct _gzochid_oid_allocation_strategy
gzochid_oid_allocation_strategy;

/*
  Function pointer prototype for oid allocation strategies.

  Implementations of this function should identify the next free block of oids
  and update the specified block pointer with the first oid in the new block as
  well as the block size, and update any internal state as necessary. Returning
  `TRUE' signals that the allocation was successful; `FALSE' that it failed, and
  that further details may be found in the specified `GError'.
*/

typedef gboolean (*gzochid_oid_allocation_func)
(gpointer, gzochid_data_oids_block *, GError **);

/*
  Intended for use by specific oid allocation strategy implementations.

  Create and return a new `gzochid_oid_allocation_strategy' that uses the
  specified allocation function (which will be passed the specified user data 
  pointer as its first argument) to reserve blocks of oids. The 
  `GDestroyNotify' callback, if specified, will be called on the user data 
  pointer when the allocation strategy is freed via 
  `gzochid_oid_allocation_strategy_free'.
*/

gzochid_oid_allocation_strategy *
gzochid_oid_allocation_strategy_new
(gzochid_oid_allocation_func, gpointer, GDestroyNotify);

/* Free the specified oid allocation strategy, invoking its `GDestroyNotify' if
   one was given at construction time. */

void gzochid_oid_allocation_strategy_free (gzochid_oid_allocation_strategy *);

/*
  Reserve a new block of object ids using the specified allocation strategy to
  obtain and persist id allocation state. 
  
  If the allocation operation is successful, this function updates the 
  specified `gzochid_data_oids_block' with information about the allocated
  block and returns `TRUE'. Otherwise, it updates the specified `GError' (if
  provided) and returns `FALSE'.
*/

gboolean gzochid_oids_reserve_block
(gzochid_oid_allocation_strategy *, gzochid_data_oids_block *, GError **);

GQuark gzochid_oids_error_quark (void);

#endif /* GZOCHID_OIDS_H */
