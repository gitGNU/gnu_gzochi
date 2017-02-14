/* test-event.c: Test routines for event.c in gzochid.
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "event.h"

struct handler_data
{
  GMutex mutex;
  GCond cond;
  
  gboolean handled;
};

static void test_handler 
(GzochidEvent *event, gpointer user_data)
{
  struct handler_data *handler_data = user_data;

  g_mutex_lock (&handler_data->mutex);

  handler_data->handled = TRUE;
  
  g_cond_signal (&handler_data->cond);
  g_mutex_unlock (&handler_data->mutex);
}

static void test_event_dispatch ()
{
  GzochidEventLoop *event_loop = g_object_new (GZOCHID_TYPE_EVENT_LOOP, NULL);
  gzochid_event_source *event_source = gzochid_event_source_new ();

  struct handler_data handler_data;

  g_mutex_init (&handler_data.mutex);
  g_cond_init (&handler_data.cond);  
  handler_data.handled = FALSE;
  
  gzochid_event_source_attach (event_loop, event_source);
  gzochid_event_attach (event_source, test_handler, &handler_data);
  gzochid_event_dispatch (event_source, g_object_new
			  (GZOCHID_TYPE_TRANSACTION_EVENT,
			   "type", TRANSACTION_START, NULL));

  g_mutex_lock (&handler_data.mutex);
  gzochid_event_loop_start (event_loop);

  g_cond_wait (&handler_data.cond, &handler_data.mutex);
  
  g_assert (handler_data.handled);

  g_mutex_unlock (&handler_data.mutex);

  g_mutex_clear (&handler_data.mutex);
  g_cond_clear (&handler_data.cond);

  gzochid_event_loop_stop (event_loop);
  g_source_destroy ((GSource *) event_source);
  g_source_unref ((GSource *) event_source);
  
  g_object_unref (event_loop);
}

int main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */
  
  g_test_add_func ("/event-loop/dispatch", test_event_dispatch);

  return g_test_run ();
}
