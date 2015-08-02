/* gzochid-auth.h: Prototypes and declarations for auth.c
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

#ifndef GZOCHID_AUTH_H
#define GZOCHID_AUTH_H

#include <glib.h>
#include <gmodule.h>

/* The identity struct typedef. (The contents of the struct are private.) */

typedef struct _gzochid_auth_identity gzochid_auth_identity;

/* Create and return a new identity with the specified name. The returned 
   identity has an initial reference count of 1; call 
   `gzochid_auth_identity_unref' on it when it is no longer needed. */

gzochid_auth_identity *gzochid_auth_identity_new (char *);

/* Returns the name of the specified identity. This pointer should not be freed
   nor the memory to which it points be modified. */

char *gzochid_auth_identity_name (gzochid_auth_identity *);

/* Increases the reference count of the specified identity and returns the
   identity. */

gzochid_auth_identity *gzochid_auth_identity_ref (gzochid_auth_identity *);

/* Decreases the reference count of the specified identity. When the reference
   count reaches zero, the memory associated with the identity will be freed. */

void gzochid_auth_identity_unref (gzochid_auth_identity *);

struct _gzochid_auth_plugin_info
{
  char *name;
  gpointer (*initialize) (GHashTable *, GError **);
  gzochid_auth_identity *(*authenticate) 
  (unsigned char *, short, gpointer, GError **);
};

typedef struct _gzochid_auth_plugin_info gzochid_auth_plugin_info;

struct _gzochid_auth_plugin
{
  GModule *handle;
  gzochid_auth_plugin_info *info;
};

typedef struct _gzochid_auth_plugin gzochid_auth_plugin;

gzochid_auth_identity *gzochid_auth_function_pass_thru
(unsigned char *, short, gpointer, GError **);

#define GZOCHID_AUTH_INIT_PLUGIN(info) \
  G_MODULE_EXPORT gint gzochid_auth_init_plugin(gzochid_auth_plugin *plugin); \
  G_MODULE_EXPORT gint gzochid_auth_init_plugin(gzochid_auth_plugin *plugin) \
  { \
    plugin->info = &(info); \
    return 0; \
  }

#define GZOCHID_AUTH_PLUGIN_ERROR gzochid_auth_plugin_error_quark ()

GQuark gzochid_auth_plugin_error_quark (void);

enum _GzochidAuthPluginError
  {
    GZOCHID_AUTH_PLUGIN_ERROR_INIT,
    GZOCHID_AUTH_PLUGIN_ERROR_AUTH
  };

typedef enum _GzochidAuthPluginError GzochidAuthPluginError;

#endif /* GZOCHID_AUTH_H */
