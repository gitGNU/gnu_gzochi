/* test-bdb.c: Test routines for storage/bdb.c in gzochid.
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "gzochid-storage.h"

#define Q(x) #x
#define QUOTE(x) Q(x)

#ifndef BDB_MODULE_LOCATION
#error "BDB_MODULE_LOCATION is required!"
#endif /* BDB_MODULE_LOCATION */

struct test_storage_fixture
{
  gchar *dir;
  gzochid_storage_engine_interface *bdb_interface;
  gzochid_storage_context *context;
};

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
                 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static void 
test_storage_fixture_setup 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gzochid_storage_engine engine;
  gint (*initializer) (gzochid_storage_engine *);
  
  GModule *module = g_module_open 
    (QUOTE (BDB_MODULE_LOCATION), G_MODULE_BIND_LOCAL);

  g_assert (module != NULL);
  g_assert (g_module_symbol (module, "gzochid_storage_init_engine", 
			     (gpointer *) &initializer));
  initializer (&engine);

  fixture->dir = g_dir_make_tmp (NULL, NULL);
  fixture->bdb_interface = engine.interface;
  fixture->context = engine.interface->initialize (fixture->dir);
}

static void
test_storage_fixture_teardown
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  fixture->bdb_interface->close_context (fixture->context);
  fixture->bdb_interface->destroy_context (fixture->dir);
  g_free (fixture->dir);
}

static void
test_storage_open_create 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gchar *db = g_strconcat (fixture->dir, "/oids", NULL);
  gzochid_storage_store *store = NULL;

  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  g_assert (fixture->bdb_interface->open (fixture->context, db, 0) == NULL);
  store = fixture->bdb_interface->open
    (fixture->context, db, GZOCHID_STORAGE_CREATE);
  g_assert (store != NULL);

  fixture->bdb_interface->close_store (store);
  fixture->bdb_interface->destroy_store (fixture->context, db);
  g_free (db);
}

static void
test_storage_open_excl 
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gchar *db = g_strconcat (fixture->dir, "/oids", NULL);
  gzochid_storage_store *store = fixture->bdb_interface->open 
    (fixture->context, db, GZOCHID_STORAGE_CREATE);

  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  fixture->bdb_interface->close_store (store);
  store = fixture->bdb_interface->open
    (fixture->context, db, GZOCHID_STORAGE_CREATE | GZOCHID_STORAGE_EXCL);
  g_assert (store == NULL);

  fixture->bdb_interface->destroy_store (fixture->context, db);
  g_free (db);
}

int 
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  if (g_module_supported ())
    {
      g_test_add 
	("/storage-bdb/open/create", struct test_storage_fixture, NULL, 
	 test_storage_fixture_setup, test_storage_open_create, 
	 test_storage_fixture_teardown);

      g_test_add ("/storage-bdb/open/excl", struct test_storage_fixture, NULL,
		  test_storage_fixture_setup, test_storage_open_excl,
		  test_storage_fixture_teardown);
    }

  return g_test_run ();
}
