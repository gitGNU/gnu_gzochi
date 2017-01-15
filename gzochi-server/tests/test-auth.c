/* test-auth.c: Test routines for auth.c in gzochid.
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
#include <glib-object.h>
#include <stddef.h>

#include "auth_int.h"
#include "config.h"
#include "gzochid-auth.h"

static void
test_identity_ref ()
{
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("[TEST]");

  g_assert (identity == gzochid_auth_identity_ref (identity));

  gzochid_auth_identity_unref (identity);
  gzochid_auth_identity_unref (identity);
}

static void
test_identity_cache_lookup ()
{
  gzochid_auth_identity_cache *cache = gzochid_auth_identity_cache_new ();
  gzochid_auth_identity *aaa = gzochid_auth_identity_from_name (cache, "aaa");

  g_assert (aaa == gzochid_auth_identity_from_name (cache, "aaa"));

  gzochid_auth_identity_unref (aaa);
  gzochid_auth_identity_unref (aaa);
  
  gzochid_auth_identity_cache_destroy (cache);
}

static void
test_identity_system ()
{
  gzochid_auth_identity *identity = gzochid_auth_system_identity ();

  g_assert_cmpstr ("[SYSTEM]", ==, gzochid_auth_identity_name (identity));
}

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
test_registry_probe ()
{
  gchar *cwd = g_get_current_dir ();
  gchar *auth_dir = g_build_filename (cwd, "auth", NULL);
  GKeyFile *key_file = g_key_file_new ();
  GzochidConfiguration *configuration = g_object_new
    (GZOCHID_TYPE_CONFIGURATION, "key_file", key_file, NULL);
  GzochidAuthPluginRegistry *registry = NULL;

  g_key_file_set_value (key_file, "game", "auth.plugin.dir", auth_dir);
  
  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  registry = g_object_new
    (GZOCHID_TYPE_AUTH_PLUGIN_REGISTRY, "configuration", configuration, NULL);

  g_assert (gzochid_auth_plugin_registry_lookup (registry, "secret") != NULL);
  
  g_object_unref (registry);
  g_object_unref (configuration);
  g_key_file_unref (key_file);

  g_free (auth_dir);
  g_free (cwd);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/auth/identity/ref", test_identity_ref);
  g_test_add_func ("/auth/identity/cache/lookup", test_identity_cache_lookup);
  g_test_add_func ("/auth/identity/system", test_identity_system);

  g_test_add_func ("/auth/registry/probe", test_registry_probe);
  
  return g_test_run ();
}
