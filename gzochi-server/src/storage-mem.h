/* storage-mem.h: Prototypes and declarations for storage-mem.c
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

#ifndef GZOCHID_STORAGE_MEM_H
#define GZOCHID_STORAGE_MEM_H

#include "storage.h"

/* This is the storage interface for the in-memory B*tree-based storage engine.
   
   It can be used to construct a "fake" storage engine for use by the gzochid
   container in the event that an external storage engine module implementation
   is unavailable or undesirable. */

extern gzochid_storage_engine_interface gzochid_storage_engine_interface_mem;

#endif /* GZOCHID_STORAGE_MEM_H */
