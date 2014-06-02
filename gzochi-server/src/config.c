/* config.c: Configuration management routines for gzochid
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

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"

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

GHashTable *gzochid_config_keyfile_extract_config 
(GKeyFile *key_file, char *group)
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
