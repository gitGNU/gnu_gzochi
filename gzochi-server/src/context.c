/* context.c: Context lifecycle management routines for gzochid
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

#include <glib.h>
#include <stdlib.h>

#include "context.h"
#include "fsm.h"

void gzochid_context_free (gzochid_context *context)
{
  g_mutex_free (context->mutex);
  g_list_free (context->children);
}

void gzochid_context_init 
(gzochid_context *context, gzochid_context *parent, gzochid_fsm *fsm)
{
  context->mutex = g_mutex_new ();
  context->parent = parent;
  context->fsm = fsm;

  if (parent != NULL)
    {
      g_mutex_lock (parent->mutex);
      parent->children = g_list_append (parent->children, context);
      g_mutex_unlock (parent->mutex);
    }

  gzochid_fsm_start (context->fsm);
}

void gzochid_context_until (gzochid_context *context, int state)
{
  gzochid_fsm_until (context->fsm, state);
}
