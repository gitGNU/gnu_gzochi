/* test-password_file.c: Test routines for the password file auth plugin.
 * Copyright (C) 2015 Julian Graham
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

#include "gzochid-auth.h"

gint gzochid_auth_init_plugin (gzochid_auth_plugin *);

static void 
test_initialize ()
{
  GError *error = NULL;
  GHashTable *properties = g_hash_table_new (g_str_hash, g_str_equal);
  GHashTable *passwords = NULL;
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));

  g_hash_table_insert (properties, "path", "passwords.txt");

  gzochid_auth_init_plugin (plugin);
  passwords = (GHashTable *) plugin->info->initialize (properties, &error);

  g_assert (passwords != NULL);
  g_assert_no_error (error);
  g_assert_cmpint (g_hash_table_size (passwords), ==, 3);

  g_assert_cmpstr (g_hash_table_lookup (passwords, "foo"), ==, "bar");
  g_assert_cmpstr (g_hash_table_lookup (passwords, "bar"), ==, "baz");
  g_assert_cmpstr (g_hash_table_lookup (passwords, "baz"), ==, "qux");

  free (plugin);
  g_hash_table_destroy (properties);
  g_hash_table_destroy (passwords);
}

static void 
test_initialize_missing_path ()
{
  GError *error = NULL;
  GHashTable *properties = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));

  gzochid_auth_init_plugin (plugin);
  g_assert (plugin->info->initialize (properties, &error) == NULL);
  g_assert_error 
    (error, GZOCHID_AUTH_PLUGIN_ERROR, GZOCHID_AUTH_PLUGIN_ERROR_INIT);

  g_clear_error (&error);
  free (plugin);
  g_hash_table_destroy (properties);
}

static void 
test_initialize_missing_file ()
{
  GError *error = NULL;
  GHashTable *properties = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));

  g_hash_table_insert (properties, "path", "passwords2.txt");

  gzochid_auth_init_plugin (plugin);
  g_assert (plugin->info->initialize (properties, &error) == NULL);
  g_assert_error (error, G_FILE_ERROR, G_FILE_ERROR_NOENT);

  g_clear_error (&error);
  free (plugin);
  g_hash_table_destroy (properties);
}

static void 
test_authenticate ()
{
  GError *error = NULL;
  GHashTable *passwords = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));
  gzochid_auth_identity *identity = NULL;

  g_hash_table_insert (passwords, "foo", "bar");

  gzochid_auth_init_plugin (plugin);
  identity = plugin->info->authenticate ("foo\0bar", 7, passwords, &error);
  g_assert (identity != NULL);
  g_assert_no_error (error);
  
  g_assert_cmpstr (identity->name, ==, "foo");

  free (identity);
  free (plugin);
  g_hash_table_destroy (passwords);
}

static void 
test_authenticate_fail ()
{
  GError *error = NULL;
  GHashTable *passwords = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));

  gzochid_auth_init_plugin (plugin);
  g_assert 
    (plugin->info->authenticate ("foo\0bar", 7, passwords, &error) == NULL);
  g_assert_no_error (error);

  free (plugin);
  g_hash_table_destroy (passwords);
}

static void 
test_authenticate_error ()
{
  GError *error = NULL;
  GHashTable *passwords = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));

  gzochid_auth_init_plugin (plugin);
  g_assert 
    (plugin->info->authenticate ("foobar", 6, passwords, &error) == NULL);
  g_assert_error 
    (error, GZOCHID_AUTH_PLUGIN_ERROR, GZOCHID_AUTH_PLUGIN_ERROR_AUTH);
  g_clear_error (&error);

  free (plugin);
  g_hash_table_destroy (passwords);
}

int 
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/auth/password_file/initialize", test_initialize);
  g_test_add_func 
    ("/auth/password_file/initialize/missing_path", 
     test_initialize_missing_path);
  g_test_add_func 
    ("/auth/password_file/initialize/missing_file", 
     test_initialize_missing_file);

  g_test_add_func ("/auth/password_file/authenticate", test_authenticate);
  g_test_add_func 
    ("/auth/password_file/authenticate/fail", test_authenticate_fail);
  g_test_add_func
    ("/auth/password_file/authenticate/error", test_authenticate_error);

  return g_test_run ();
}
