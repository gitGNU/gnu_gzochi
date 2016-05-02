/* oids-storage.h: Protoypes and declarations for oids-storage.h
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

#ifndef GZOCHID_OIDS_STORAGE_H
#define GZOCHID_OIDS_STORAGE_H

#include "oids.h"
#include "gzochid-storage.h"

/* 
   Create and return an oid allocation strategy that uses the specified storage
   engine, environment, and store to manage the state of allocated ids.

   The returned strategy should be freed with 
   `gzochid_oid_allocation_strategy_free' when no longer needed. The storage
   resources - in particular the storage context and store - should remain open
   for the lifetime of the strategy; if the environment needs to be closed for
   some reason, the strategy should be recreated.
*/

gzochid_oid_allocation_strategy *gzochid_storage_oid_strategy_new
(gzochid_storage_engine_interface *, gzochid_storage_context *,
 gzochid_storage_store *);

#endif /* GZOCHID_OIDS_STORAGE_H */
