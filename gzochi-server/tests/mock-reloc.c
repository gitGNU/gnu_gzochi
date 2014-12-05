/* mock-reloc.c: Test-time replacements for reloc.c routines.
 * Copyright (C) 2014 Julian Graham
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

#include <libguile.h>
#include <stdlib.h>

#include "app.h"
#include "reloc.h"

static void
noop_serialize 
(gzochid_application_context *context, void *data, GString *out, GError **error)
{
}

static void *
noop_deserialize 
(gzochid_application_context *context, GString *in, GError **error)
{
  return NULL;
}

static void
noop_finalize (gzochid_application_context *context, void *data)
{
}

gzochid_io_serialization gzochid_scm_location_aware_serialization =
  { noop_serialize, noop_deserialize, noop_finalize };

gzochid_scm_location_info *gzochid_scm_location_get 
(gzochid_application_context *context, SCM obj)
{
  gzochid_scm_location_info *value = 
    malloc (sizeof (gzochid_scm_location_info));

  value->bits = SCM_UNPACK (obj);

  return value;
}

SCM gzochid_scm_location_resolve 
(gzochid_application_context *context, gzochid_scm_location_info *location)
{
  return SCM_PACK (location->bits);
}
