/* test-itree.c: Test routines for itree.c in gzochid.
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
#include <stddef.h>

#include "itree.h"

static gboolean
search (gpointer from, gpointer to, gpointer data, gpointer user_data)
{
  int *counter = user_data;

  (*counter)++;
  return FALSE;
}

static gboolean
search_abort (gpointer from, gpointer to, gpointer data, gpointer user_data)
{
  int *counter = user_data;

  (*counter)++;
  return TRUE;
}

static void
test_itree_insert_simple ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key3, NULL);
  gzochid_itree_search (itree, key2, search, &counter);

  g_assert_cmpint (counter, ==, 1);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
}

static void
test_itree_remove_simple ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key3, NULL);
  gzochid_itree_remove (itree, key1, key3);
  gzochid_itree_search (itree, key2, search, &counter);

  g_assert_cmpint (counter, ==, 0);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
}

static void
test_itree_search_simple ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  GBytes *key4 = g_bytes_new_static ("foo4", 4);
  GBytes *key5 = g_bytes_new_static ("foo5", 4);
  GBytes *key6 = g_bytes_new_static ("foo6", 4);
  GBytes *key7 = g_bytes_new_static ("foo7", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key7, NULL);
  gzochid_itree_insert (itree, key2, key6, NULL);
  gzochid_itree_insert (itree, key3, key5, NULL);
  gzochid_itree_search (itree, key4, search, &counter);

  g_assert_cmpint (counter, ==, 3);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
  g_bytes_unref (key4);
  g_bytes_unref (key5);
  g_bytes_unref (key6);
  g_bytes_unref (key7);
}

static void
test_itree_search_abort ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  GBytes *key4 = g_bytes_new_static ("foo4", 4);
  GBytes *key5 = g_bytes_new_static ("foo5", 4);
  GBytes *key6 = g_bytes_new_static ("foo6", 4);
  GBytes *key7 = g_bytes_new_static ("foo7", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key7, NULL);
  gzochid_itree_insert (itree, key2, key6, NULL);
  gzochid_itree_insert (itree, key3, key5, NULL);
  gzochid_itree_search (itree, key4, search_abort, &counter);

  g_assert_cmpint (counter, ==, 1);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
  g_bytes_unref (key4);
  g_bytes_unref (key5);
  g_bytes_unref (key6);
  g_bytes_unref (key7);
}

static void
test_itree_search_interval_simple ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  GBytes *key4 = g_bytes_new_static ("foo4", 4);
  GBytes *key5 = g_bytes_new_static ("foo5", 4);
  GBytes *key6 = g_bytes_new_static ("foo6", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key6, NULL);
  gzochid_itree_insert (itree, key2, key5, NULL);
  gzochid_itree_search_interval (itree, key3, key4, search, &counter);

  g_assert_cmpint (counter, ==, 2);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
  g_bytes_unref (key4);
  g_bytes_unref (key5);
  g_bytes_unref (key6);
}

static void
test_itree_search_interval_abort ()
{
  GBytes *key1 = g_bytes_new_static ("foo1", 4);
  GBytes *key2 = g_bytes_new_static ("foo2", 4);
  GBytes *key3 = g_bytes_new_static ("foo3", 4);
  GBytes *key4 = g_bytes_new_static ("foo4", 4);
  GBytes *key5 = g_bytes_new_static ("foo5", 4);
  GBytes *key6 = g_bytes_new_static ("foo6", 4);
  gzochid_itree *itree = gzochid_itree_new (g_bytes_compare, g_bytes_compare);
  int counter = 0;
  
  gzochid_itree_insert (itree, key1, key6, NULL);
  gzochid_itree_insert (itree, key2, key5, NULL);
  gzochid_itree_search_interval (itree, key3, key4, search_abort, &counter);

  g_assert_cmpint (counter, ==, 1);
  
  gzochid_itree_free (itree);

  g_bytes_unref (key1);
  g_bytes_unref (key2);
  g_bytes_unref (key3);
  g_bytes_unref (key4);
  g_bytes_unref (key5);
  g_bytes_unref (key6);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/itree/insert/simple", test_itree_insert_simple);
  g_test_add_func ("/itree/remove/simple", test_itree_remove_simple);
  g_test_add_func ("/itree/search/simple", test_itree_search_simple);
  g_test_add_func ("/itree/search/abort", test_itree_search_abort);
  g_test_add_func
    ("/itree/search-interval/simple", test_itree_search_interval_simple);
  g_test_add_func
    ("/itree/search-interval/abort", test_itree_search_interval_abort);
    
  return g_test_run ();
}
