/* descriptor.h: Prototypes and declarations for descriptor.c
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

#ifndef GZOCHID_DESCRIPTOR_H
#define GZOCHID_DESCRIPTOR_H

#include <glib.h>
#include <stdio.h>

#include "callback.h"

struct _gzochid_application_descriptor
{
  char *name;
  char *description;

  GList *load_paths; /* Descriptor-specified module load paths. */
 
  gzochid_application_callback *initialized;
  gzochid_application_callback *logged_in;
  gzochid_application_callback *ready;

  char *auth_type;
  GHashTable *auth_properties;

  GHashTable *properties;
};

typedef struct _gzochid_application_descriptor gzochid_application_descriptor;

gzochid_application_descriptor *gzochid_config_parse_application_descriptor 
(FILE *);

#endif /* GZOCHID_DESCRIPTOR_H */
