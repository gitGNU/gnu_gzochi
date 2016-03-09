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
#include <stddef.h>
#include <stdlib.h>

#include "oids.h"
#include "storage-mem.h"

struct _test_oids_storage
{
  gzochid_storage_engine_interface *interface;
  gzochid_storage_context *context;
  gzochid_storage_store *meta;
};

typedef struct _test_oids_storage test_oids_storage;

static void
setup_storage (test_oids_storage *fixture, gconstpointer user_data)
{
  fixture->interface = &gzochid_storage_engine_interface_mem;
  fixture->context = fixture->interface->initialize ("test");
  fixture->meta = fixture->interface->open (fixture->context, "meta", 0);
}

static void
teardown_storage (test_oids_storage *fixture, gconstpointer user_data)
{
  fixture->interface->close_store (fixture->meta);
  fixture->interface->close_context (fixture->context);
}

static char *
get_oid_block_counter (test_oids_storage *fixture)
{
  char key[] = { 0 };
  char *value = NULL;
  gzochid_storage_transaction *transaction =
    fixture->interface->transaction_begin (fixture->context);

  value = fixture->interface->transaction_get
    (transaction, fixture->meta, key, 1, NULL);
  
  fixture->interface->transaction_rollback (transaction);
  return value;
}

static void
test_oids_reserve_first (test_oids_storage *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  gzochid_data_oids_block oids_block;
  char *new_counter_value = NULL;
  
  g_assert_true
    (gzochid_oids_reserve_block
     (fixture->interface, fixture->context, fixture->meta, &oids_block, &err));
  g_assert_no_error (err);

  new_counter_value = get_oid_block_counter (fixture);
  g_assert_cmpstr (new_counter_value, ==, "64");
  free (new_counter_value);

  mpz_clear (oids_block.block_start);
}

static void
test_oids_reserve_next (test_oids_storage *fixture, gconstpointer user_data)
{
  GError *err = NULL;
  gzochid_data_oids_block oids_block;
  char *new_counter_value = NULL;

  char key[] = { 0 };
  char value[] = { '1' };
  gzochid_storage_transaction *transaction =
    fixture->interface->transaction_begin (fixture->context);

  fixture->interface->transaction_put
    (transaction, fixture->meta, key, 1, value, 2);

  fixture->interface->transaction_prepare (transaction);
  fixture->interface->transaction_commit (transaction);
  
  g_assert_true
    (gzochid_oids_reserve_block
     (fixture->interface, fixture->context, fixture->meta, &oids_block, &err));
  g_assert_no_error (err);

  new_counter_value = get_oid_block_counter (fixture);
  g_assert_cmpstr (new_counter_value, ==, "65");
  free (new_counter_value);

  mpz_clear (oids_block.block_start);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/oids/reserve/first", test_oids_storage, NULL, setup_storage,
     test_oids_reserve_first, teardown_storage);
  g_test_add
    ("/oids/reserve/next", test_oids_storage, NULL, setup_storage,
     test_oids_reserve_next, teardown_storage);

  return g_test_run ();
}
