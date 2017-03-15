/* test-lock-mem.c: Test routines for lock-mem.c in gzochid.
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

#include "lock.h"

struct _test_lock_table_fixture
{
  gzochid_lock_table *lock_table;
};

typedef struct _test_lock_table_fixture test_lock_table_fixture;

static void
setup_lock_table (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  fixture->lock_table = gzochid_lock_table_new (NULL);
}

static void
teardown_lock_table (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  gzochid_lock_table_free (fixture->lock_table);
}

static void
test_check (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  g_assert (! gzochid_lock_check (fixture->lock_table, 1, key, FALSE));
  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 1, key, FALSE, NULL));
  g_assert (gzochid_lock_check (fixture->lock_table, 1, key, FALSE));

  g_bytes_unref (key);
}

static void
test_read_single (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 1, key, FALSE, NULL));

  g_bytes_unref (key);
}

static void
test_read_multiple (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, FALSE, NULL);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, FALSE, NULL));

  g_bytes_unref (key);
}

static void
test_read_with_range_lock (test_lock_table_fixture *fixture,
			   gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_range_check_and_set (fixture->lock_table, 1, from, to, NULL);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, from, FALSE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_read_conflict_with_write (test_lock_table_fixture *fixture,
			       gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, TRUE, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, FALSE, NULL));

  g_bytes_unref (key);
}

static void
test_write (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 1, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_write_conflict_with_read (test_lock_table_fixture *fixture,
			       gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, FALSE, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_write_conflict_with_write (test_lock_table_fixture *fixture,
				gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, TRUE, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_write_conflict_with_range_lock (test_lock_table_fixture *fixture,
				     gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_range_check_and_set (fixture->lock_table, 1, from, to, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, from, TRUE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_upgrade (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, FALSE, NULL);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 1, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_upgrade_conflict_with_read (test_lock_table_fixture *fixture,
				 gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, FALSE, NULL);
  gzochid_lock_check_and_set (fixture->lock_table, 2, key, FALSE, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 1, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_upgrade_conflict_with_range_lock (test_lock_table_fixture *fixture,
				       gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_check_and_set (fixture->lock_table, 1, from, FALSE, NULL);
  gzochid_lock_range_check_and_set (fixture->lock_table, 2, from, to, NULL);

  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 1, from, TRUE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_range_lock (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  g_assert (gzochid_lock_range_check_and_set
	    (fixture->lock_table, 1, from, to, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_range_lock_lower_null (test_lock_table_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  g_assert (gzochid_lock_range_check_and_set
	    (fixture->lock_table, 1, NULL, to, NULL));
  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));

  g_bytes_unref (to);
  g_bytes_unref (key);
}

static void
test_range_lock_upper_null (test_lock_table_fixture *fixture,
			    gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo2", 5);
  GBytes *from = g_bytes_new_static ("foo", 4);

  g_assert (gzochid_lock_range_check_and_set
	    (fixture->lock_table, 1, from, NULL, NULL));
  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (key);
}

static void
test_range_lock_null_null (test_lock_table_fixture *fixture,
			   gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);
  
  g_assert (gzochid_lock_range_check_and_set
	    (fixture->lock_table, 1, NULL, NULL, NULL));
  g_assert (! gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));
    
  g_bytes_unref (key);
}

static void
test_range_lock_conflict_with_range_lock (test_lock_table_fixture *fixture,
					  gconstpointer user_data)
{
  GBytes *from1 = g_bytes_new_static ("foo1", 5);
  GBytes *to1 = g_bytes_new_static ("foo3", 5);
  GBytes *from2 = g_bytes_new_static ("foo2", 5);
  GBytes *to2 = g_bytes_new_static ("foo4", 5);

  gzochid_lock_range_check_and_set (fixture->lock_table, 1, from1, to1, NULL);

  g_assert (! gzochid_lock_range_check_and_set
	    (fixture->lock_table, 2, from2, to2, NULL));

  g_bytes_unref (from1);
  g_bytes_unref (to1);
  g_bytes_unref (from2);
  g_bytes_unref (to2);

}

static void
test_range_lock_compatible_with_write (test_lock_table_fixture *fixture,
				       gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_check_and_set (fixture->lock_table, 1, from, TRUE, NULL);

  g_assert (gzochid_lock_range_check_and_set
	    (fixture->lock_table, 1, from, to, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_range_lock_conflict_with_write (test_lock_table_fixture *fixture,
				     gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_check_and_set (fixture->lock_table, 1, from, TRUE, NULL);

  g_assert (! gzochid_lock_range_check_and_set
	    (fixture->lock_table, 2, from, to, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_range_lock_rebalance (test_lock_table_fixture *fixture,
			   gconstpointer user_data)
{
  GBytes *range1 = g_bytes_new_static ("foo1", 5);
  GBytes *range2 = g_bytes_new_static ("foo2", 5);
  GBytes *range3 = g_bytes_new_static ("foo3", 5);
  GBytes *range4 = g_bytes_new_static ("foo4", 5);
  GBytes *range5 = g_bytes_new_static ("foo5", 5);
  GBytes *range6 = g_bytes_new_static ("foo6", 5);

  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range1, range1, NULL);
  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range2, range2, NULL);
  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range3, range3, NULL);
  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range4, range4, NULL);
  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range5, range5, NULL);
  gzochid_lock_range_check_and_set
    (fixture->lock_table, 1, range6, range6, NULL);

  gzochid_lock_release_all (fixture->lock_table, 1);

  g_bytes_unref (range1);
  g_bytes_unref (range2);
  g_bytes_unref (range3);
  g_bytes_unref (range4);
  g_bytes_unref (range5);
  g_bytes_unref (range6);
}

static void
test_release_read_lock (test_lock_table_fixture *fixture,
			gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, FALSE, NULL);
  gzochid_lock_release (fixture->lock_table, 1, key);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, TRUE, NULL));

  g_bytes_unref (key);
}

static void
test_release_write_lock (test_lock_table_fixture *fixture,
			 gconstpointer user_data)
{
  GBytes *key = g_bytes_new_static ("foo", 4);

  gzochid_lock_check_and_set (fixture->lock_table, 1, key, TRUE, NULL);
  gzochid_lock_release (fixture->lock_table, 1, key);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, key, FALSE, NULL));

  g_bytes_unref (key);
}

static void
test_release_range_lock (test_lock_table_fixture *fixture,
			 gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_range_check_and_set (fixture->lock_table, 1, from, to, NULL);
  gzochid_lock_release_range (fixture->lock_table, 1, from, to);

  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, from, TRUE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

static void
test_release_all (test_lock_table_fixture *fixture, gconstpointer user_data)
{
  GBytes *from = g_bytes_new_static ("foo", 4);
  GBytes *to = g_bytes_new_static ("foo2", 5);

  gzochid_lock_range_check_and_set (fixture->lock_table, 1, from, to, NULL);
  gzochid_lock_check_and_set (fixture->lock_table, 1, from, TRUE, NULL);

  gzochid_lock_release_all (fixture->lock_table, 1);
  
  g_assert (gzochid_lock_check_and_set
	    (fixture->lock_table, 2, from, TRUE, NULL));

  g_bytes_unref (from);
  g_bytes_unref (to);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/lock-mem/check-lock", test_lock_table_fixture, NULL, setup_lock_table,
     test_check, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/read/single", test_lock_table_fixture, NULL,
     setup_lock_table, test_read_single, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/read/multiple", test_lock_table_fixture, NULL,
     setup_lock_table, test_read_multiple, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/read/with-range", test_lock_table_fixture, NULL,
     setup_lock_table, test_read_with_range_lock, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/read/conflict-with-write", test_lock_table_fixture, NULL,
     setup_lock_table, test_read_conflict_with_write, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/write", test_lock_table_fixture, NULL, setup_lock_table,
     test_write, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/write/conflict-with-read", test_lock_table_fixture, NULL,
     setup_lock_table, test_write_conflict_with_read, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/write/conflict-with-write", test_lock_table_fixture, NULL,
     setup_lock_table, test_write_conflict_with_write, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/write/conflict-with-range", test_lock_table_fixture, NULL,
     setup_lock_table, test_write_conflict_with_range_lock,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/upgrade", test_lock_table_fixture, NULL, setup_lock_table,
     test_upgrade, teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/upgrade/conflict-with-read", test_lock_table_fixture,
     NULL, setup_lock_table, test_upgrade_conflict_with_read,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/lock/upgrade/conflict-with-range", test_lock_table_fixture,
     NULL, setup_lock_table, test_upgrade_conflict_with_range_lock,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/single", test_lock_table_fixture, NULL,
     setup_lock_table, test_range_lock, teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/lower-null", test_lock_table_fixture, NULL,
     setup_lock_table, test_range_lock_lower_null, teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/upper-null", test_lock_table_fixture, NULL,
     setup_lock_table, test_range_lock_upper_null, teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/null-null", test_lock_table_fixture, NULL,
     setup_lock_table, test_range_lock_null_null, teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/conflict-with-range-lock", test_lock_table_fixture,
     NULL, setup_lock_table, test_range_lock_conflict_with_range_lock,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/compatible-with-write-lock", test_lock_table_fixture,
     NULL, setup_lock_table, test_range_lock_compatible_with_write,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/conflict-with-write-lock", test_lock_table_fixture,
     NULL, setup_lock_table, test_range_lock_conflict_with_write,
     teardown_lock_table);
  g_test_add
    ("/lock-mem/range-lock/rebalance", test_lock_table_fixture, NULL,
     setup_lock_table, test_range_lock_rebalance, teardown_lock_table);
  g_test_add
    ("/lock-mem/release/read", test_lock_table_fixture, NULL, setup_lock_table,
     test_release_read_lock, teardown_lock_table);
  g_test_add
    ("/lock-mem/release/write", test_lock_table_fixture, NULL,
     setup_lock_table, test_release_write_lock, teardown_lock_table);
  g_test_add
    ("/lock-mem/release-range-lock", test_lock_table_fixture, NULL,
     setup_lock_table, test_release_range_lock, teardown_lock_table);
  g_test_add
    ("/lock-mem/release-all", test_lock_table_fixture, NULL, setup_lock_table,
     test_release_all, teardown_lock_table);
  
  return g_test_run ();
}
