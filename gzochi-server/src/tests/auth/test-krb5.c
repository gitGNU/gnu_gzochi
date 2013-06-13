/* test-krb5.c: Test routines for the Kerberos v5 auth plugin.
 * Copyright (C) 2013 Julian Graham
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
#include <krb5.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../auth.h"

gint gzochid_auth_init_plugin (gzochid_auth_plugin *);

static void test_initialize ()
{
  GError *error = NULL;
  GHashTable *properties = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));
  gpointer context = NULL;

  gzochid_auth_init_plugin (plugin);
  context = plugin->info->initialize (properties, &error);
  g_assert_no_error (error);
  free (plugin);
}

static void test_authenticate ()
{
  GError *error = NULL;
  GHashTable *properties = g_hash_table_new (g_str_hash, g_str_equal);
  gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));
  gpointer context = NULL;

  gzochid_auth_identity *identity = NULL;

  gzochid_auth_init_plugin (plugin);
  context = plugin->info->initialize (properties, NULL);
  identity = plugin->info->authenticate ("ABC", 4, context, &error);

  g_assert (identity != NULL);
  g_assert_no_error (error);
  g_assert_cmpstr (identity->name, ==, "DEF");

  free (identity);
  free (plugin);
}

int main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/auth/krb5/initialize", test_initialize);
  g_test_add_func ("/auth/krb5/authenticate", test_authenticate);

  return g_test_run ();
}
