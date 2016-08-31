/* test-config.c: Test routines for config.c in gzochid.
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

#include "config.h"

static void
test_config_extract_group ()
{
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = NULL;
  GHashTable *props = NULL;
  
  g_key_file_set_value (key_file, "group_1", "foo", "bar");
  g_key_file_set_value (key_file, "group_2", "baz", "quux");

  configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);

  props = gzochid_configuration_extract_group (configuration, "group_1");

  g_assert (props != NULL);
  g_assert_cmpint (g_hash_table_size (props), ==, 1);
  g_assert (g_hash_table_contains (props, "foo"));
  g_assert_cmpstr (g_hash_table_lookup (props, "foo"), ==, "bar");

  g_hash_table_destroy (props);
  g_object_unref (configuration);
  g_key_file_unref (key_file);
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

  g_test_add_func ("/config/extract-group", test_config_extract_group);
  
  return g_test_run ();
}
