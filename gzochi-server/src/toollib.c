/* toollib.c: Assorted utility routines to support command-line tools
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
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "config.h"
#include "storage.h"

#define Q(x) #x
#define QUOTE(x) Q(x)

#ifndef GZOCHI_CONF_LOCATION
#define GZOCHI_CONF_LOCATION "/etc/gzochid.conf"
#endif /* GZOCHI_CONF_LOCATION */

static GHashTable *
load_game_config (const char *path)
{
  GKeyFile *key_file = g_key_file_new ();
  GHashTable *game_config = NULL;
  GError *err = NULL;

  g_key_file_load_from_file (key_file, path, G_KEY_FILE_NONE, &err);
  
  if (err != NULL)
    {
      g_critical ("Failed to open %s: %s", path, err->message);
      exit (EXIT_FAILURE);
    }

  game_config = gzochid_config_keyfile_extract_config (key_file, "game");
  g_key_file_free (key_file);
  return game_config;
}

gzochid_storage_store *
gzochid_tool_open_store (gzochid_storage_context *context, char *path)
{
  gzochid_storage_store *store = gzochid_storage_open (context, path, 0);

  if (store == NULL)
    {
      g_critical ("Failed to open store in %s", path);
      exit (EXIT_FAILURE);
    }
  else return store;
}

char *
gzochid_tool_probe_data_dir (char *app_or_dir)
{
  if (g_file_test (app_or_dir, G_FILE_TEST_IS_DIR))
    {
      g_debug ("%s is a directory; skipping name resolution.", app_or_dir);
      return strdup (app_or_dir);
    }
  else
    {
      GHashTable *config = NULL;
      char *work_dir = NULL, *data_dir = NULL;
      
      g_debug ("Probing for an application with name %s.", app_or_dir);
      config = load_game_config (QUOTE (GZOCHID_CONF_LOCATION));

      if (! g_hash_table_contains (config, "server.fs.data"))
	{
	  g_critical ("server.fs.data must be set.");
	  exit (EXIT_FAILURE);
	}
      
      work_dir = g_hash_table_lookup (config, "server.fs.data");
      data_dir = g_strconcat (work_dir, "/", app_or_dir, NULL);
      g_hash_table_destroy (config);

      if (g_file_test (data_dir, G_FILE_TEST_IS_DIR))
	return data_dir;
      else
	{
	  g_critical 
	    ("%s is neither an application name nor a directory.", app_or_dir);
	  exit (EXIT_FAILURE);
	  return NULL;
	}
    }
}

char **
gzochid_tool_parse_targets (char *target)
{
  char **ret = calloc (3, sizeof (char *));
  char *last_colon = rindex (target, ':');

  ret[0] = strdup (target);

  if (last_colon != NULL)
    {
      char *db = strdup (last_colon + 1);
      
      if (strcmp (db, "meta") == 0 
	  || strcmp (db, "oids") == 0 
	  || strcmp (db, "names") == 0)
	{
	  ret[0] = strndup (target, last_colon - target);
	  ret[1] = db;
	}
    }

  return ret;
}
