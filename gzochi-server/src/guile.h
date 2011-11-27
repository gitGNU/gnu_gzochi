/* guile.h: Prototypes and declarations for guile.c
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

#ifndef GZOCHID_GUILE_H
#define GZOCHID_GUILE_H

#include <glib.h>

#include "app.h"
#include "threads.h"

SCM gzochid_guile_resolve_module (gzochid_application_context *, char *);

void gzochid_guile_run (gzochid_thread_worker, gpointer);

void gzochid_guile_thread_pool_push 
(GThreadPool *, gzochid_thread_worker, gpointer, GError **);

#endif /* GZOCHID_GUILE_H */
