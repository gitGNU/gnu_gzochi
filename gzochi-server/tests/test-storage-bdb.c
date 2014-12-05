/* test-storage-bdb.c: Test routines for storage-bdb.c in gzochid.
 * Copyright (C) 2014 Julian Graham
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
#include <string.h>
#include <sys/stat.h>

#include "storage.h"

static gzochid_storage_context *
test_storage_open_create_storage_context = NULL;

struct test_storage_fixture
{
  gchar *dir;
  gzochid_storage_context *context;
};

static void 
test_storage_fixture_setup 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  fixture->dir = g_dir_make_tmp (NULL, NULL);
  fixture->context = gzochid_storage_initialize (fixture->dir);
}

static void
test_storage_fixture_teardown
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gzochid_storage_context_close (fixture->context);
  gzochid_storage_context_destroy (fixture->dir);
  g_free (fixture->dir);
}

static void
test_storage_open_create 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gchar *db = g_strconcat (fixture->dir, "/oids", NULL);
  gzochid_storage_store *store = NULL;

  g_assert_null (gzochid_storage_open (fixture->context, db, 0));
  store = gzochid_storage_open (fixture->context, db, GZOCHID_STORAGE_CREATE);
  g_assert_nonnull (store);

  gzochid_storage_close (store);
  gzochid_storage_destroy (fixture->context, db);
  g_free (db);
}

static void
test_storage_open_excl 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gchar *db = g_strconcat (fixture->dir, "/oids", NULL);
  gzochid_storage_store *store = gzochid_storage_open 
    (fixture->context, db, GZOCHID_STORAGE_CREATE);

  gzochid_storage_close (store);  
  store = gzochid_storage_open 
    (fixture->context, db, GZOCHID_STORAGE_CREATE | GZOCHID_STORAGE_EXCL);
  g_assert_null (store);

  gzochid_storage_destroy (fixture->context, db);
  g_free (db);
}

int 
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add ("/storage-bdb/open/create", struct test_storage_fixture, NULL, 
	      test_storage_fixture_setup, test_storage_open_create, 
	      test_storage_fixture_teardown);
  g_test_add ("/storage-bdb/open/excl", struct test_storage_fixture, NULL,
	      test_storage_fixture_setup, test_storage_open_excl,
	      test_storage_fixture_teardown);

  return g_test_run ();
}
