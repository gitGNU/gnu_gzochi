/* auth.c: Authorization management routines for gzochid
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
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "auth.h"

gzochid_auth_identity *gzochid_auth_identity_new (char *name)
{
  gzochid_auth_identity *identity = calloc (1, sizeof (gzochid_auth_identity));

  identity->name = name;

  return identity;
}

void gzochid_auth_identity_free (gzochid_auth_identity *identity)
{
  free (identity->name);
  free (identity);
}

gzochid_auth_identity *gzochid_auth_function_pass_thru 
(gzochid_application_context *context, unsigned char *cred, short cred_len)
{
  if (cred_len <= 0)
    return NULL;
  return gzochid_auth_identity_new (strndup ((char *) cred, cred_len));
}

void gzochid_auth_identity_serializer 
(gzochid_application_context *context, void *ptr, GString *out)
{
  gzochid_auth_identity *identity = (gzochid_auth_identity *) ptr;
  g_string_append_len (out, identity->name, strlen (identity->name) + 1);
}

void *gzochid_auth_identity_deserializer
(gzochid_application_context *context, GString *in)
{
  char *name = strndup (in->str, in->len);
  gzochid_auth_identity *identity = gzochid_auth_identity_new (name);

  g_string_erase (in, 0, strlen (name) + 1);
  return identity;
}
