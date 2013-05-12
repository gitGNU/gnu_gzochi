/* auth.h: Prototypes and declarations for auth.c
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

#ifndef GZOCHID_AUTH_H
#define GZOCHID_AUTH_H

#include <glib.h>
#include <gmodule.h>

typedef struct _gzochid_auth_identity
{
  char *name;
} gzochid_auth_identity;

typedef struct _gzochid_auth_plugin_info
{
  char *name;
  gpointer (*initialize) (GHashTable *, GError **);
  gzochid_auth_identity *(*authenticate) 
  (unsigned char *, short, gpointer, GError **);
} gzochid_auth_plugin_info;

typedef struct _gzochid_auth_plugin
{
  GModule *handle;
  gzochid_auth_plugin_info *info;
} gzochid_auth_plugin;
struct _gzochid_application_context;

gzochid_auth_identity *gzochid_auth_function_pass_thru
(unsigned char *, short, gpointer, GError **);
gzochid_auth_identity *gzochid_auth_function_scheme
(struct _gzochid_application_context *, unsigned char *, short);

#define GZOCHID_AUTH_INIT_PLUGIN(info) \
  G_MODULE_EXPORT gint gzochid_auth_init_plugin(gzochid_auth_plugin *plugin); \
  G_MODULE_EXPORT gint gzochid_auth_init_plugin(gzochid_auth_plugin *plugin) \
  { \
    plugin->info = &(info); \
    return 0; \
  }

#define GZOCHID_AUTH_PLUGIN_ERROR gzochid_auth_plugin_error_quark ()

GQuark gzochid_auth_plugin_error_quark (void);

typedef enum 
  {
    GZOCHID_AUTH_PLUGIN_ERROR_INIT,
    GZOCHID_AUTH_PLUGIN_ERROR_AUTH
  }
  GzochidAuthPluginError;

#endif /* GZOCHID_AUTH_H */
