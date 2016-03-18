/* config.h: Prototypes and declarations for config.c
 * Copyright (C) 2016 Julian Graham
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

#ifndef GZOCHID_CONFIG_H
#define GZOCHID_CONFIG_H

#include <glib.h>
#include <glib-object.h>

/* Injectable configuration type definitions. */

#define GZOCHID_TYPE_CONFIGURATION gzochid_configuration_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_configuration_get_type (void);

typedef struct _GzochidConfiguration GzochidConfiguration;

struct _GzochidConfigurationClass
{
  GObjectClass parent_class;
};

/* Construct a new instance of a `GzochidConfiguration' via 
   `g_object_new', passing a `GKeyFile' for the "key_file" property. Note that
   because this property is not injectable, `GzochidConfiguration' instances
   cannot be constructed by the resolver, but an explicitly-constructed
   `GzochidConfiguration' can be added to a require resolution context via
   `gzochid_resolver_provide'. */

typedef struct _GzochidConfigurationClass GzochidConfigurationClass;

static inline GzochidConfiguration *
GZOCHID_CONFIGURATION (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_configuration_get_type (), GzochidConfiguration);
}

/* End boilerplate. */

gboolean gzochid_config_to_boolean (char *, gboolean);
int gzochid_config_to_int (char *, int);
long gzochid_config_to_long (char *, long);

GHashTable *gzochid_config_keyfile_extract_config (GKeyFile *, const char *);

/* Extracts the key-value group with the specified name from the 
   specified `GzochidConfiguration' object and returns it as a `GHashTable',
   which should be freed via `g_hashtable_unref' or `g_hash_table_destroy' when
   no longer necessary. */   

GHashTable *gzochid_configuration_extract_group
(GzochidConfiguration *, const char *);

#endif /* GZOCHID_CONFIG_H */
