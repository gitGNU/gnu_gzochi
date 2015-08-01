/* lrucache.c: Simple GLib-based LRU cache implementation for gzochid
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "lrucache.h"

/* A wrapper around values stored in the cache to assist with recency 
   determination and eviction. */

struct _gzochid_lru_cache_value
{
  gpointer key; /* The key stored in the cache. */
  gpointer value; /* The value stored in the cache. */

  /* The "last access counter, used to sort the LRU index. */
  
  guint64 access_counter; 
};

typedef struct _gzochid_lru_cache_value gzochid_lru_cache_value;

/* The private LRU cache structure. */

struct _gzochid_lru_cache
{
  GHashTable *cache; /* The table of keys and values currently in the cache. */

  /* The values currently in the cache, sorted by last access. */

  GSequence *lru_index; 

  GMutex mutex; /* Synchronizes membership checks on values in the cache. */  

  /* The maximum number of elements that may be in the cache without triggering
     an eviction. */

  guint max_size; 

  guint64 next_access_counter; /* The global access counter. */
  
  GDestroyNotify value_evict_func; /* The value eviction function, if any. */
  gzochid_lru_cache_new_func new_func; /* The value generator function. */
};

gzochid_lru_cache *
gzochid_lru_cache_new_full (GHashFunc hash_func, GEqualFunc key_equal_func,
			    gzochid_lru_cache_new_func cache_new_func,
			    guint max_size, GDestroyNotify key_evict_func,
			    GDestroyNotify value_evict_func)
{
  gzochid_lru_cache *cache = malloc (sizeof (gzochid_lru_cache));

  assert (hash_func != NULL);
  assert (key_equal_func != NULL);
  assert (cache_new_func != NULL);
  
  cache->cache = g_hash_table_new_full
    (hash_func, key_equal_func, key_evict_func, NULL);
  cache->lru_index = g_sequence_new (free);
  cache->max_size = max_size;
  cache->next_access_counter = 0;
  
  cache->new_func = cache_new_func;
  cache->value_evict_func = value_evict_func;
  
  g_mutex_init (&cache->mutex);

  return cache;
}

/* A `GCompareDataFunc' implementation for sorting the LRU index. */

static gint
lru_cache_value_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
  const gzochid_lru_cache_value *value_a = a;
  const gzochid_lru_cache_value *value_b = b;

  if (value_a->access_counter < value_b->access_counter)
    return -1;
  else if (value_a->access_counter > value_b->access_counter)
    return 1;
  else return 0;
}

gpointer
gzochid_lru_cache_lookup (gzochid_lru_cache *cache, gpointer key)
{
  gpointer ret = NULL;
  
  g_mutex_lock (&cache->mutex);

  /* If the value is already in the cache, bump the access counter and return 
     it. */
  
  if (g_hash_table_contains (cache->cache, key))
    {
      gzochid_lru_cache_value *value = g_hash_table_lookup (cache->cache, key);
      GSequenceIter *iter = g_sequence_lookup
	(cache->lru_index, value, lru_cache_value_compare, NULL);

      value->access_counter = cache->next_access_counter++;
      
      g_sequence_sort_changed (iter, lru_cache_value_compare, NULL);

      ret = value->value;
    }

  /* Otherwise, use the value generator to produce it. */
  
  else
    {
      gzochid_lru_cache_value *value =
	malloc (sizeof (gzochid_lru_cache_value));
      gpointer key_copy = NULL;
           
      value->value = cache->new_func (key, &key_copy);
      value->key = key_copy != NULL ? key_copy : key;      
      value->access_counter = cache->next_access_counter++;

      g_sequence_insert_sorted
	(cache->lru_index, value, lru_cache_value_compare, NULL);
      g_hash_table_insert (cache->cache, value->key, value);

      ret = value->value;

      /* ...and possibly evict the oldest value from the cache if the cache has
	 exceeded its maximum size. */
      
      if (g_hash_table_size (cache->cache) > cache->max_size)
	{
	  GSequenceIter *first = g_sequence_get_begin_iter (cache->lru_index);
	  gzochid_lru_cache_value *old_value = g_sequence_get (first);	  

	  /* If the value eviction function exists, use it to evict the 
	     value. */
	  
	  if (cache->value_evict_func != NULL)
	    cache->value_evict_func (old_value->value);

	  /* GHashTable will handle the key eviction. */
	  
	  g_hash_table_remove (cache->cache, old_value->key);
	  g_sequence_remove (first);
	}
    }
  
  g_mutex_unlock (&cache->mutex);
  return ret;
}

/* A `Func' implementation to apply the `GDestroyNotify' value eviction function
   to every value. */

static void
evict_value (gpointer data, gpointer user_data)
{
  gzochid_lru_cache_value *value = data;
  ((GDestroyNotify) user_data) (value->value);
}

void
gzochid_lru_cache_destroy (gzochid_lru_cache *cache)
{
  g_hash_table_destroy (cache->cache);

  if (cache->value_evict_func != NULL)
    g_sequence_foreach (cache->lru_index, evict_value, cache->value_evict_func);

  g_sequence_free (cache->lru_index);
  g_mutex_clear (&cache->mutex);

  free (cache);
}
