/* storage.h: Prototypes and declarations for storage.c
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

#ifndef GZOCHID_STORAGE_INTERNAL_H
#define GZOCHID_STORAGE_INTERNAL_H

#include "gzochid-storage.h"

/* Attempts to load the storage engine module with the specified name, which
   should be the basename of a the module file, with no extension or path
   prefix. If a storage engine module is present in the storage directory, it
   will be opened, loaded, and initialized.

   This function returns a pointer to a newly-allocated 
   `gzochid_storage_engine' structure (which should be freed with `free') on 
   success, or NULL on failure. 
*/
gzochid_storage_engine *gzochid_storage_load_engine (const char *);

#endif /* GZOCHID_STORAGE_INTERNAL_H */
