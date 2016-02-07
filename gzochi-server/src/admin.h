/* admin.h: Prototypes and declarations for admin.c
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

#ifndef GZOCHID_ADMIN_H
#define GZOCHID_ADMIN_H

#include <glib.h>

#include "context.h"

enum gzochid_admin_state 
  {
    GZOCHID_ADMIN_STATE_INITIALIZING,
    GZOCHID_ADMIN_STATE_RUNNING,
    GZOCHID_ADMIN_STATE_STOPPED
  };

typedef struct _gzochid_admin_context gzochid_admin_context;

gzochid_admin_context *gzochid_admin_context_new (void);
void gzochid_admin_context_free (gzochid_admin_context *);
void gzochid_admin_context_init 
(gzochid_admin_context *, gzochid_context *, GHashTable *);

#endif /* GZOCHID_ADMIN_H */
