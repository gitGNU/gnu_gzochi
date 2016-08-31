/* test-oids-dataclient.c: Tests for oids-dataclient.c in gzochid.
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
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>

#include "dataclient.h"
#include "oids-dataclient.h"

struct _GzochidDataClient
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
}

static gpointer
reserve_oids_inner (gpointer data)
{
  gpointer *args = data;
  gzochid_data_oids_block block;
  gzochid_dataclient_oids_callback callback = args[0];
  gpointer callback_data = args[1];
  
  block.block_start = 1;
  block.block_size = 100;
  
  callback (block, callback_data);

  free (args);
}

void
gzochid_dataclient_reserve_oids (GzochidDataClient *client, char *app,
				 gzochid_dataclient_oids_callback callback,
				 gpointer data)
{
  gpointer *args = malloc (sizeof (gpointer) * 2);

  args[0] = callback;
  args[1] = data;
  
  g_thread_new ("fake-main-loop", reserve_oids_inner, args);
}

static void
test_oids_dataclient_reserve ()
{
  GzochidDataClient *dataclient = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  gzochid_oid_allocation_strategy *strategy =
    gzochid_dataclient_oid_strategy_new (dataclient, "test");
  gzochid_data_oids_block block;
  GError *err = NULL;
  
  g_assert_true (gzochid_oids_reserve_block (strategy, &block, &err));
  g_assert_no_error (err);
  g_assert_cmpint (block.block_start, ==, 1);
  g_assert_cmpint (block.block_size, ==, 100);

  gzochid_oid_allocation_strategy_free (strategy);  
  g_object_unref (dataclient);
}

int
main (int argc, char *argv[])
{
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  g_test_init (&argc, &argv, NULL);

  g_test_add_func
    ("/oids-dataclient/reserve/simple", test_oids_dataclient_reserve);

  return g_test_run ();
}
