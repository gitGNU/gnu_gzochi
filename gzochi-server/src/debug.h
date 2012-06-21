/* debug.h: Prototypes and declarations for debug.c
 * Copyright (C) 2012 Julian Graham
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

#ifndef GZOCHID_DEBUG_H
#define GZOCHID_DEBUG_H

#include "context.h"

enum gzochid_debug_state
  {
    GZOCHID_DEBUG_STATE_INITIALIZING,
    GZOCHID_DEBUG_STATE_RUNNING,
    GZOCHID_DEBUG_STATE_STOPPED
  };

typedef struct _gzochid_debug_context
{
  gzochid_context base;
  int port;
} gzochid_debug_context;

gzochid_debug_context *gzochid_debug_context_new (void);
void gzochid_debug_context_free (gzochid_debug_context *);
void gzochid_debug_context_init
(gzochid_debug_context *, gzochid_context *, int);

#endif /* GZOCHID_DEBUG_H */
