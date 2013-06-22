/* threads.c: Threadpool implementation and thread support routines for gzochid
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

#include <assert.h>
#include <glib.h>
#include <stdlib.h>

#include "threads.h"
#include "tx.h"

static void dispatcher (gpointer data, gpointer user_data)
{
  gzochid_thread_work *work = (gzochid_thread_work *) data;
  work->worker (work->data, user_data);
  assert (!gzochid_transaction_active ());
  free (work);
}

GThreadPool *gzochid_thread_pool_new 
(gpointer user_data, gint max_threads, gboolean exclusive, GError **error)
{
  return g_thread_pool_new 
    (dispatcher, user_data, max_threads, exclusive, error);
}

void gzochid_thread_pool_push 
(GThreadPool *pool, gzochid_thread_worker worker, gpointer data, GError **error)
{
  gzochid_thread_work *work = calloc (1, sizeof (gzochid_thread_work));
  
  work->worker = worker;
  work->data = data;
  
  g_thread_pool_push (pool, work, error);
}
