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
test_identity_cache_lookup ()
{
  gzochid_auth_identity_cache *cache = gzochid_auth_identity_cache_new ();
  gzochid_auth_identity *aaa = gzochid_auth_identity_from_name (cache, "aaa");

  g_assert (aaa == gzochid_auth_identity_from_name (cache, "aaa"));
  
  gzochid_auth_identity_cache_destroy (cache);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/auth/identity/cache/lookup", test_identity_cache_lookup);
  
  return g_test_run ();
}
