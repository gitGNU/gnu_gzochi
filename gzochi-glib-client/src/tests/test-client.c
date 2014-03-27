/* test-client.c: Test routines for client.c in libgzochi-glib-client.
 * Copyright (C) 2014 Julian Graham
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
#include <gzochi-client-common.h>

#include "../libgzochi-glib.h"

static void test_client_check ()
{
  gzochi_glib_client_session *session = 
    (gzochi_glib_client_session *) gzochi_client_common_session_new ();
  GzochiSource *source = gzochi_source_new (session);
  GMainContext *context = g_main_context_new ();
  
  gint timeout, priority;
  GPollFD pollfds[1];

  g_source_attach ((GSource *) source, context);  
  g_main_context_prepare (context, &priority);
  g_main_context_query (context, priority, &timeout, pollfds, 1);

  pollfds[0].revents |= G_IO_IN;

  /* By default, the session is not connected and has not acknowledged a
     disconnection, so it should be dispatchable. */

  g_assert (g_main_context_check (context, priority, pollfds, 1));

  g_main_context_unref (context);
  gzochi_client_common_session_free (session);
}

int main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/client/check", test_client_check);

  return g_test_run ();
}
