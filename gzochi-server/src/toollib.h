/* toollib.h: Prototypes and declarations for toollib.c
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

#ifndef GZOCHID_TOOLLIB_H
#define GZOCHID_TOOLLIB_H

#include <glib.h>

#include "storage.h"

/**
   Open the store with the specified filename template in the specified storage
   context. (The actual name of the file backing the returned store is dependent
   on the implementation of the storage engine.) If the store cannot be opened,
   this function logs an error and causes the process to exit.
 */
gzochid_storage_store *
gzochid_tool_open_store (gzochid_storage_context *, char *);

/**
   Attempts to resolve a data directory from its argument, which may be the name
   of an application or the directory itself. If the specified string names an
   existing directory, that directory is returned; otherwise, the "gzochid.conf"
   file is consulted to find the server's data directory, which is scanned for
   a matching application name. If neither of these strategies produces a
   directory, this function logs an error and causes the process to exit.

   The returned string should be freed by the caller.
 */
char *
gzochid_tool_probe_data_dir (char *);

/**
   Attempts to parse a string of the form
  
   <APP_NAME OR DATA_DIR>[:<DB>]
   
   (where <DB> is optional) into a 2-element string vector such that the first 
   element is the application name or database directory, and the second 
   element is NULL or a database name, which must be one of "meta," "oids," or 
   "names."

   The returned string vector should be freed by the caller using g_strfreev.
 */
char **
gzochid_tool_parse_targets (char *);

#endif /* GZOCHID_TOOLLIB_H */
