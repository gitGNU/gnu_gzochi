/* test-event.c: Test routines for event.c in gzochid.
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "../app.h"
#include "../event.h"

static void test_handler 
(gzochid_application_context *context, gzochid_application_event *event, 
 gpointer user_data)
{
  *((gboolean *) user_data) = TRUE;
}

static gboolean quit_main_loop (gpointer user_data)
{
  GMainLoop *main_loop = (GMainLoop *) user_data;
  g_main_loop_quit (main_loop);
  return TRUE;
}

static void test_event_dispatch ()
{
  GMainLoop *main_loop = g_main_loop_new (NULL, FALSE);
  GMainContext *main_context = g_main_loop_get_context (main_loop);

  gzochid_application_event_source *event_source = 
    gzochid_application_event_source_new (gzochid_application_context_new ());
  GSource *timeout_source = g_timeout_source_new (100);
  gzochid_application_event *event = 
    malloc (sizeof (gzochid_application_event));
  gboolean handled = FALSE;

  g_source_attach ((GSource *) event_source, main_context);
  g_source_attach (timeout_source, main_context);
  g_source_set_callback (timeout_source, quit_main_loop, main_loop, NULL);

  event->type = TRANSACTION_START;
  gettimeofday (&event->timestamp, NULL);

  gzochid_application_event_attach (event_source, test_handler, &handled);
  gzochid_application_event_dispatch (event_source, event);

  g_main_loop_run (main_loop);

  g_assert (handled);
}

int main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/event/dispatch", test_event_dispatch);

  return g_test_run ();
}
