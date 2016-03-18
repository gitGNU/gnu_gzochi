/* config.c: Configuration management routines for gzochid
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

#include <glib.h>
#include <glib-object.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"

/* Boilerplate setup for the configuration object. */

struct _GzochidConfiguration
{
  GObject parent_instance;
  
  GKeyFile *key_file; /* Ref to the configuration key file. */
};

G_DEFINE_TYPE (GzochidConfiguration, gzochid_configuration, G_TYPE_OBJECT);

enum gzochid_configuration_properties
  {
    PROP_KEY_FILE = 1,
    N_PROPERTIES
  };

static void
gzochid_configuration_finalize (GObject *gobject)
{
  GzochidConfiguration *configuration = GZOCHID_CONFIGURATION (gobject);

  g_key_file_unref (configuration->key_file);
  
  G_OBJECT_CLASS (gzochid_configuration_parent_class)->finalize (gobject);
}

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_configuration_set_property (GObject *object, guint property_id,
				    const GValue *value, GParamSpec *pspec)
{
  GzochidConfiguration *self = GZOCHID_CONFIGURATION (object);

  switch (property_id)
    {
    case PROP_KEY_FILE:
      self->key_file = g_key_file_ref (g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_configuration_class_init (GzochidConfigurationClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  
  object_class->finalize = gzochid_configuration_finalize;
  object_class->set_property = gzochid_configuration_set_property;

  obj_properties[PROP_KEY_FILE] = g_param_spec_pointer
    ("key_file", "Configuration key file", "Set the configuration key file",
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_configuration_init (GzochidConfiguration *self)
{
}

gboolean gzochid_config_to_boolean (char *str, gboolean def)
{
  if (str == NULL)
    return def;
  else if (strcasecmp (str, "true") == 0)
    return TRUE;
  else if (strcasecmp (str, "false") == 0)
    return FALSE;
  return def;
}

int gzochid_config_to_int (char *str, int def)
{
  int ret = 0;
  if (str == NULL)
    return def;
  if (sscanf (str, "%d", &ret) == 1)
    return ret;
  return def;
}

long gzochid_config_to_long (char *str, long def)
{
  long ret = 0;
  if (str == NULL)
    return def;
  if (sscanf (str, "%ld", &ret) == 1)
    return ret;
  return def;
}

GHashTable *
gzochid_config_keyfile_extract_config (GKeyFile *key_file, const char *group)
{
  unsigned int i = 0;
  gsize num_keys = 0;
  char **keys = g_key_file_get_keys (key_file, group, &num_keys, NULL);
  GHashTable *config = g_hash_table_new_full 
    (g_str_hash, g_str_equal, free, free);

  for (; i < num_keys; i++) 
    g_hash_table_insert 
      (config, keys[i], g_key_file_get_value (key_file, group, keys[i], NULL));
  
  free (keys);
  return config;
}

GHashTable *
gzochid_configuration_extract_group (GzochidConfiguration *config,
				     const char *group)
{
  return gzochid_config_keyfile_extract_config (config->key_file, group);
}
