/* secret.c: A toy authentication plugin implementation for gzochid.
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
#include <string.h>

#include "gzochid-auth.h"

/* The following functions comprise a "toy" storage engine implementation for
   use in testing the authentication plugin registry bootstrap (see 
   `test-auth.c' in the parent directory). */

/* The plugin initialize function. */

static gpointer
initialize (GHashTable *properties, GError **error)
{
  return NULL;
}

/* The plugin authentication function. Returns an identity named "secret" if and
   only if the specified credentials begin with the `NULL'-terminated string
   "secret". */

static gzochid_auth_identity *
authenticate (unsigned char *credentials, short len, gpointer auth_data,
	      GError **error)
{
  if (memcmp (credentials, "secret", MIN (len, 7)) == 0)
    return gzochid_auth_identity_new ("secret");
  else return NULL;
}

static gzochid_auth_plugin_info info = { "secret", initialize, authenticate };

GZOCHID_AUTH_INIT_PLUGIN (info)
