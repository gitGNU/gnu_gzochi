/* nodemap-mem.h: Prototypes and declarations for nodemap-mem.c
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

#ifndef GZOCHI_METAD_NODEMAP_MEM_H
#define GZOCHI_METAD_NODEMAP_MEM_H

#include "nodemap.h"

/* Construct and return a pointer to a new instance of a memory-backed 
   implementation of `gzochi_metad_nodemap'. The memory used by this object 
   should be freed via `gzochi_metad_nodemap_mem_free' when no longer in use. */

gzochi_metad_nodemap *gzochi_metad_nodemap_mem_new (void);

/* Frees the resources associated with the specified `gzochi_metad_nodemap'
   instance, which must have been returned by `gzochi_metad_nodemap_mem_new'. */

void gzochi_metad_nodemap_mem_free (gzochi_metad_nodemap *);

#endif /* GZOCHI_METAD_NODEMAP_MEM_H */
