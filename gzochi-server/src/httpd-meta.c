/* httpd-meta.c: Handler implementations for the gzochi-metad admin web console
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
#include <stddef.h>

#include "httpd.h"
#include "httpd-meta.h"

#define GREETING \
  "<h1>Hello, browser!</h1>\n" \
  "<p>This is the administrative web console for the gzochid meta server. " \
  "Nothing to see here right now.</p>"

static void
hello_world (const GMatchInfo *match_info, gzochid_http_response_sink *sink,
	     gpointer request_context, gpointer user_data)
{
  GString *response_str = g_string_new (NULL);

  g_string_append (response_str, "<html><body>");
  g_string_append (response_str, GREETING);
  g_string_append (response_str, "</body></html>");

  gzochid_http_write_response
    (sink, 200, response_str->str, response_str->len);

  g_string_free (response_str, TRUE);
}

void
gzochid_httpd_meta_register_handlers (GzochidHttpServer *httpd_context)
{
  gzochid_httpd_add_terminal (httpd_context, "/", hello_world, NULL);
}
