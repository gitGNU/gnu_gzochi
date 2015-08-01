/* test-lrucache.c: Test routines for lrucache.c in gzochid.
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "lrucache.h"

static guint new_value_counter;
static guint key_eviction_counter;
static guint value_eviction_counter;

static gpointer
test_new_func (gpointer key, gpointer *key_copy)
{
  new_value_counter++;
  return strdup (key);
}

static void
test_key_evict_func (gpointer key)
{
  key_eviction_counter++;
}

static void
test_value_evict_func (gpointer value)
{
  value_eviction_counter++;
  free (value);
}

static void
reset_counters ()
{
  new_value_counter = 0;
  key_eviction_counter = 0;
  value_eviction_counter = 0;
}

static void
test_lrucache_lookup_simple ()
{
  gzochid_lru_cache *cache = NULL;

  reset_counters ();
  
  cache = gzochid_lru_cache_new_full
    (g_str_hash, g_str_equal, test_new_func, 1, test_key_evict_func,
     test_value_evict_func);

  g_assert_cmpstr ("aaa", ==, gzochid_lru_cache_lookup (cache, "aaa"));
  g_assert_cmpstr ("aaa", ==, gzochid_lru_cache_lookup (cache, "aaa"));

  g_assert_cmpint (1, ==, new_value_counter);
  
  gzochid_lru_cache_destroy (cache);
}

static void
test_lrucache_lookup_eviction ()
{
  gzochid_lru_cache *cache = NULL;

  reset_counters ();
  
  cache = gzochid_lru_cache_new_full
    (g_str_hash, g_str_equal, test_new_func, 3, test_key_evict_func,
     test_value_evict_func);

  gzochid_lru_cache_lookup (cache, "aaa");
  gzochid_lru_cache_lookup (cache, "bbb");
  gzochid_lru_cache_lookup (cache, "ccc");
  gzochid_lru_cache_lookup (cache, "ddd");

  g_assert_cmpint (1, ==, key_eviction_counter);
  g_assert_cmpint (1, ==, value_eviction_counter);

  gzochid_lru_cache_lookup (cache, "bbb");

  g_assert_cmpint (1, ==, key_eviction_counter);
  g_assert_cmpint (1, ==, value_eviction_counter);

  gzochid_lru_cache_destroy (cache);
}

static void
test_lrucache_destroy_eviction ()
{
  gzochid_lru_cache *cache = NULL;

  reset_counters ();
  
  cache = gzochid_lru_cache_new_full
    (g_str_hash, g_str_equal, test_new_func, 3, test_key_evict_func,
     test_value_evict_func);

  gzochid_lru_cache_lookup (cache, "aaa");
  gzochid_lru_cache_lookup (cache, "bbb");
  gzochid_lru_cache_lookup (cache, "ccc");
  
  gzochid_lru_cache_destroy (cache);

  g_assert_cmpint (3, ==, key_eviction_counter);
  g_assert_cmpint (3, ==, value_eviction_counter);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/lrucache/lookup/simple", test_lrucache_lookup_simple);
  g_test_add_func ("/lrucache/lookup/eviction", test_lrucache_lookup_eviction);
  g_test_add_func ("/lrucache/destroy/eviction",
		   test_lrucache_destroy_eviction);
  
  return g_test_run ();
}
