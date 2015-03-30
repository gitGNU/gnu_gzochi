/* toollib.h: Prototypes and declarations for toollib.c
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

#ifndef GZOCHID_TOOLLIB_H
#define GZOCHID_TOOLLIB_H

#include <glib.h>

#include "gzochid-storage.h"

/**
   Return a GHashTable containing the "game" section of the .INI file located at
   the specified path or in the default location (which is often 
   '/etc/gzochid.conf') when the specified path is NULL. If the file cannot be 
   opened for reading (because, e.g., it does not exist), this function logs an
   error and causes the process to exit.
 */
GHashTable *gzochid_tool_load_game_config (const char *);

/**
   Open the store with the specified filename template in the specified storage
   context using the specified storage engine_interface. (The actual name of the
   file backing the returned store is dependent on the implementation of the 
   storage engine.) If the store cannot be opened, this function logs an error 
   and causes the process to exit.
 */
gzochid_storage_store *gzochid_tool_open_store 
(gzochid_storage_engine_interface *, gzochid_storage_context *, char *);

/**
   Attempts to resolve a data directory from its arguments. The string 
   `app_or_dir', which may be the name of an application or the directory 
   itself. If the specified string names an existing directory, that directory 
   is returned; otherwise, the specified hash table - which should in most cases
   be the result of calling `gzochid_tool_load_game_config' - is consulted, 
   using the key `server.fs.data'.
   
   The `create_data_dir' flag indicates that the data directory for the resolved
   application need not exist yet; otherwise, if neither of these strategies
   produces a directory, this function logs an error and causes the process to 
   exit.

   The returned string should be freed by the caller.
 */
char *gzochid_tool_probe_data_dir (GHashTable *, char *, gboolean);

/**
   Attempts to resolve the name of a storage engine module from its arguments,
   and load that module. The string `name', if given, should be the unqualified
   name of a storage engine module, e.g., `bdb'. If this argument is NULL, the
   specified hash table - which should in most cases be the result of calling
   `gzochid_tool_load_game_config' - is consulted, using the key 
   `storage.engine'. 

   If neither of these strategies produces a usable storage engine, this 
   function logs an error and causes the process to exit.

   The returned struct should be freed by the caller after its associated module
   handle is closed.
 */
gzochid_storage_engine *gzochid_tool_probe_storage_engine 
(GHashTable *, char *);

/**
   Attempts to parse a string of the form
  
   <APP_NAME OR DATA_DIR>[:<DB>]
   
   (where <DB> is optional) into a 2-element string vector such that the first 
   element is the application name or database directory, and the second 
   element is NULL or a database name, which must be one of "meta," "oids," or 
   "names."

   The returned string vector should be freed by the caller using g_strfreev.
 */
char **gzochid_tool_parse_targets (char *);

#endif /* GZOCHID_TOOLLIB_H */
