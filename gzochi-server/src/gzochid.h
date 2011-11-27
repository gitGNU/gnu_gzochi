/* gzochid.h: Prototypes and declarations for gzochid.c
 * Copyright (C) 2011 Julian Graham
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

#ifndef GZOCHID_H
#define GZOCHID_H

#include <glib.h>

#include "context.h"
#include "fsm.h"

typedef struct _gzochid_server_context
{
  gzochid_context base;
  GThreadPool *pool;
} gzochid_server_context;

enum gzochid_state 
  {
    GZOCHID_STATE_INITIALIZING,
    GZOCHID_STATE_RUNNING,
    GZOCHID_STATE_STOPPED
  };

gzochid_server_context *gzochid_server_context_new (void);
void gzochid_server_context_init (gzochid_server_context *);

#endif /* GZOCHID_H */
