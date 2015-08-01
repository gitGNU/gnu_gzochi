/* lrucache.h: Prototypes and declarations for lrucache.c
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

#ifndef GZOCHID_LRU_CACHE_H
#define GZOCHID_LRU_CACHE_H

#include <glib.h>

/* Typedef for a function pointer type to represent the caller-supplied "value 
   generator" function that is called on a cache miss to obtain a value to store
   in the cache. The uncached key will be passed as the first argument. 

   Implementations may optionally set the second argument to a copy of the key
   for storage in the cache. */

typedef gpointer (*gzochid_lru_cache_new_func) (gpointer, gpointer *);

typedef struct _gzochid_lru_cache gzochid_lru_cache;

/* Construct a new LRU cache with the specified hash and equality functions
   governing the behavior of the hash table backing the cache, the specified
   `gzochid_lru_cache_new_func' to be used as the value generator, the maximum
   size of the cache, and the optionally NULL `GDestroyNotify' functions to be 
   called when keys and values are evicted from the cache. */

gzochid_lru_cache *
gzochid_lru_cache_new_full (GHashFunc, GEqualFunc, gzochid_lru_cache_new_func,
			    guint, GDestroyNotify, GDestroyNotify);

/* Return the value mapped to the specified key in the specified LRU cache. If
   no value is currently mapped to the key, the cache's associated "value 
   generator" will be called, and the value it returns will be added to the
   cache and returned from this function; this may trigger the eviction of less
   recently accessed values in the cache.

   Accessing a value that is already mapped in the cache (i.e., a "cache hit")
   causes it to be marked as recently accessed. */

gpointer
gzochid_lru_cache_lookup (gzochid_lru_cache *, gpointer);

/* Frees all memory associated with the specified LRU cache, evicting all keys
   and values. */

void
gzochid_lru_cache_destroy (gzochid_lru_cache *);

#endif /* GZOCHID_LRU_CACHE_H */
