/* test-auth.c: Test routines for auth.c in gzochid.
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

#include "auth_int.h"
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

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/auth/identity/ref", test_identity_ref);
  g_test_add_func ("/auth/identity/cache/lookup", test_identity_cache_lookup);
  g_test_add_func ("/auth/identity/system", test_identity_system);

  
  return g_test_run ();
}
