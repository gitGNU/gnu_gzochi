/* reloc.h: Prototypes and declarations for reloc.c
 * Copyright (C) 2011 Julian Graham
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

#ifndef GZOCHID_RELOC_H
#define GZOCHID_RELOC_H

#include <glib.h>
#include <libguile.h>

#include "app.h"
#include "io.h"

gzochid_io_serialization gzochid_scm_location_aware_serialization;

typedef struct _gzochid_scm_location_info
{
  scm_t_bits bits;
} gzochid_scm_location_info;

typedef struct _gzochid_scm_location_transaction_context
{
  gzochid_application_context *context;
  GHashTable *bits_cache;
} gzochid_scm_location_transaction_context;

gzochid_scm_location_info *gzochid_scm_location_get 
(gzochid_application_context *, SCM);
SCM gzochid_scm_location_resolve 
(gzochid_application_context *, gzochid_scm_location_info *);

#endif /* GZOCHID_RELOC_H */
