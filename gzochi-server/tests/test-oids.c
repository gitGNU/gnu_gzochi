/* test-oids.c: Test routines for oids.c in gzochid.
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

#include "oids.h"

#define BLOCK_SIZE 123

static int block_start;
static gboolean destroy_notifications;

static void
reset_counters ()
{
  block_start = 1;
  destroy_notifications = 0;
}

static void
cleanup (gpointer user_data)
{
  destroy_notifications++;
}

static gboolean
allocate (gpointer user_data, gzochid_data_oids_block *block, GError **err)
{
  mpz_init_set_ui (block->block_start, block_start);
  block->block_size = BLOCK_SIZE;

  block_start += BLOCK_SIZE;
} 

static void
test_oid_strategy_allocation ()
{
  gzochid_data_oids_block block;
  gzochid_oid_allocation_strategy *strategy =
    gzochid_oid_allocation_strategy_new (allocate, NULL, NULL);

  reset_counters ();

  g_assert_true (gzochid_oids_reserve_block (strategy, &block, NULL));

  g_assert_true (mpz_cmp_ui (block.block_start, 1) == 0);
  g_assert_cmpint (block.block_size, ==, BLOCK_SIZE);

  mpz_clear (block.block_start);
  
  g_assert_true (gzochid_oids_reserve_block (strategy, &block, NULL));
  
  g_assert_true (mpz_cmp_ui (block.block_start, 124) == 0);
  g_assert_cmpint (block.block_size, ==, BLOCK_SIZE);

  mpz_clear (block.block_start);

  gzochid_oid_allocation_strategy_free (strategy);
}

static void
test_oid_strategy_free ()
{
  gzochid_oid_allocation_strategy *strategy =
    gzochid_oid_allocation_strategy_new (allocate, NULL, cleanup);

  reset_counters ();

  gzochid_oid_allocation_strategy_free (strategy);

  g_assert_cmpint (destroy_notifications, ==, 1);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/oids/strategy/allocation", test_oid_strategy_allocation);
  g_test_add_func ("/oids/strategy/free", test_oid_strategy_free);
  
  return g_test_run ();
}
