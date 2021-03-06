/* threads.h: Prototypes and declarations for threads.c
 * Copyright (C) 2013 Julian Graham
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

#ifndef GZOCHID_THREADS_H
#define GZOCHID_THREADS_H

#include <glib.h>

typedef void (*gzochid_thread_worker) (gpointer, gpointer);

typedef struct _gzochid_thread_work
{
  gzochid_thread_worker worker;
  gpointer data;
} gzochid_thread_work;

GThreadPool *gzochid_thread_pool_new (gpointer, gint, gboolean, GError **);
void gzochid_thread_pool_push 
(GThreadPool *, gzochid_thread_worker, gpointer, GError **);

#endif /* GZOCHID_THREADS_H */
