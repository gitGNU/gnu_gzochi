/* test-nodemap-mem.c: Test routines for nodemap-mem.c in gzochi-metad.
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

#include "nodemap.h"
#include "nodemap-mem.h"

static void
test_nodemap_mem_map_simple ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);
  
  iface->map_session (nodemap, "foo", 1, 1, &err);
  g_assert_no_error (err);
  
  iface->map_session (nodemap, "bar", 1, 1, &err);
  g_assert_no_error (err);  
  
  gzochi_metad_nodemap_mem_free (nodemap);
}

static void
test_nodemap_mem_map_error ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);

  iface->map_session (nodemap, "foo", 1, 1, &err);
  g_assert_no_error (err);
  iface->map_session (nodemap, "foo", 1, 2, &err);
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_ALREADY_MAPPED);
  g_error_free (err);
  
  gzochi_metad_nodemap_mem_free (nodemap);
}

static void
test_nodemap_mem_unmap_simple ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);

  iface->map_session (nodemap, "foo", 1, 1, NULL);
  iface->unmap_session (nodemap, "foo", 1);
  g_assert_cmpint (iface->lookup_session (nodemap, "foo", 1, &err), ==, 0);
  
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED);
  g_error_free (err);
  
  gzochi_metad_nodemap_mem_free (nodemap);
}

static void
test_nodemap_mem_lookup_simple ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);

  iface->map_session (nodemap, "foo", 1, 2, NULL);
  g_assert_cmpint (iface->lookup_session (nodemap, "foo", 1, &err), ==, 2);
  g_assert_no_error (err);

  gzochi_metad_nodemap_mem_free (nodemap);
}

static void
test_nodemap_mem_lookup_error ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);

  g_assert_cmpint (iface->lookup_session (nodemap, "foo", 1, &err), ==, 0);  
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED);
  g_error_free (err);
  
  gzochi_metad_nodemap_mem_free (nodemap);
}

static void
test_nodemap_mem_unmap_all_simple ()
{
  GError *err = NULL;
  gzochi_metad_nodemap *nodemap = gzochi_metad_nodemap_mem_new ();
  gzochi_metad_nodemap_iface *iface = GZOCHI_METAD_NODEMAP_IFACE (nodemap);

  iface->map_session (nodemap, "foo", 1, 1, NULL);
  iface->map_session (nodemap, "foo", 2, 1, NULL);
  iface->map_session (nodemap, "bar", 1, 1, NULL);

  iface->unmap_all (nodemap, 1);

  g_assert_cmpint (iface->lookup_session (nodemap, "foo", 1, &err), ==, 0);  
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED);

  g_clear_error (&err);
  
  g_assert_cmpint (iface->lookup_session (nodemap, "foo", 2, &err), ==, 0);  
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED);

  g_clear_error (&err);

  g_assert_cmpint (iface->lookup_session (nodemap, "bar", 1, &err), ==, 0);  
  g_assert_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		  GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED);

  g_error_free (err);
  
  gzochi_metad_nodemap_mem_free (nodemap);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/nodemap/mem/map/simple", test_nodemap_mem_map_simple);
  g_test_add_func ("/nodemap/mem/map/error", test_nodemap_mem_map_error);
  g_test_add_func ("/nodemap/mem/unmap/simple", test_nodemap_mem_unmap_simple);
  g_test_add_func
    ("/nodemap/mem/lookup/simple", test_nodemap_mem_lookup_simple);
  g_test_add_func ("/nodemap/mem/lookup/error", test_nodemap_mem_lookup_error);
  g_test_add_func
    ("/nodemap/mem/unmap-all/simple", test_nodemap_mem_unmap_all_simple);
    
  return g_test_run ();
}
