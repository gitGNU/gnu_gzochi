/* context.h: Prototypes and declarations for context.c
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

#ifndef GZOCHID_CONTEXT_H
#define GZOCHID_CONTEXT_H

#include <glib.h>

#include "fsm.h"

typedef struct _gzochid_context
{
  gzochid_fsm *fsm;
  struct _gzochid_context *parent;
  GMutex *mutex;
  GList *children;
} gzochid_context;

void gzochid_context_free (gzochid_context *);
void gzochid_context_init (gzochid_context *, gzochid_context *, gzochid_fsm *);
void gzochid_context_until (gzochid_context *, int);

#endif /* GZOCHID_CONTEXT_H */
