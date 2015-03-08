/* password_file.c: Password file-based authentication plugin for gzochid
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

#include <glib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../gzochid-auth.h"

#define PATH_PROPERTY "path"

static gpointer 
initialize (GHashTable *properties, GError **error)
{
  gchar *path = NULL;
  gchar *line = NULL;
  GIOChannel *channel = NULL;
  GError *channel_error = NULL;
  GHashTable *passwords = g_hash_table_new (g_str_hash, g_str_equal);

  if (! g_hash_table_contains (properties, PATH_PROPERTY))
    {
      g_set_error_literal 
	(error, 
	 GZOCHID_AUTH_PLUGIN_ERROR, 
	 GZOCHID_AUTH_PLUGIN_ERROR_INIT,
	 "Required 'path' property not set.");
		   
      return NULL;
    }
  
  path = (gchar *) g_hash_table_lookup (properties, PATH_PROPERTY);
  channel = g_io_channel_new_file (path, "r", &channel_error);
  
  if (channel == NULL)
    {
      g_propagate_error (error, channel_error);
      return NULL;
    }

  while (g_io_channel_read_line (channel, &line, NULL, NULL, NULL) 
	 == G_IO_STATUS_NORMAL)
    {
      gchar **tokens = g_strsplit (line, "=", 2);
      if (tokens[0] != NULL && tokens[1] != NULL)
	{
	  g_hash_table_insert (passwords, tokens[0], g_strchomp (tokens[1]));
	  g_free (tokens);
	}
      else g_strfreev (tokens);
      g_free (line);
    }
  
  g_io_channel_shutdown (channel, FALSE, NULL);
  g_io_channel_unref (channel);

  return passwords;
}

static gzochid_auth_identity *
authenticate (unsigned char *credentials, short len, gpointer auth_data, 
	      GError **error)
{
  GHashTable *passwords = (GHashTable *) auth_data;
  unsigned char *pivot = (unsigned char *) memchr (credentials, '\0', len);

  if (pivot == NULL)
    {
      g_set_error_literal 
	(error,
	 GZOCHID_AUTH_PLUGIN_ERROR, 
	 GZOCHID_AUTH_PLUGIN_ERROR_AUTH,
	 "Improperly formatted credentials.");

      return NULL;
    }

  if (g_hash_table_contains (passwords, credentials))
    {
      char *password = g_hash_table_lookup (passwords, credentials);
      int cmplen = MIN (len - (pivot - credentials), strlen (password));

      if (strncmp (password, (char *) pivot + 1, cmplen) == 0)
	{
	  gzochid_auth_identity *identity = 
	    malloc (sizeof (gzochid_auth_identity));

	  identity->name = strdup ((char *) credentials);
	  return identity;
	}
      else return NULL;
    }
  else return NULL;
}

static gzochid_auth_plugin_info info = 
  { "password_file", initialize, authenticate };

GZOCHID_AUTH_INIT_PLUGIN (info)
