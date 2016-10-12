/* test-httpd.c: Test routines for httpd.c in gzochid.
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
#include <netinet/in.h>
#include <stddef.h>
#include <string.h>

#include "httpd.h"

struct _test_httpd_fixture
{
  GzochidHttpServer *http_server;
  GIOChannel *client_channel;
  int client_socket_fd;
};

typedef struct _test_httpd_fixture test_httpd_fixture;

static void
test_terminal_handler (const GMatchInfo *match_info,
		       gzochid_http_response_sink *sink,
		       gpointer request_context, gpointer user_data)
{
  gzochid_http_write_response (sink, 200, "SUCCESS\n", 8);
}

static void
test_continuation_terminal (const GMatchInfo *match_info,
			    gzochid_http_response_sink *sink,
			    gpointer request_context, gpointer user_data)
{
  g_assert_cmpstr (request_context, ==, "foo");
  gzochid_http_write_response (sink, 200, "SUCCESS\n", 8);
}

static gpointer
test_continuation_handler (const GMatchInfo *match_info,
			   gzochid_http_response_sink *sink,
			   gpointer request_context, gpointer user_data)
{
  return "foo";
}

static void
test_httpd_fixture_set_up (test_httpd_fixture *fixture, gconstpointer user_data)
{
  struct sockaddr addr;
  socklen_t addrlen = sizeof (struct sockaddr);

  fixture->client_socket_fd = socket (AF_INET, SOCK_STREAM, 0);
  fixture->http_server = g_object_new (GZOCHID_TYPE_HTTP_SERVER, NULL);

  gzochid_http_server_start (fixture->http_server, 0, NULL);
  
  _gzochid_http_server_getsockname (fixture->http_server, &addr, &addrlen);
  connect (fixture->client_socket_fd, &addr, addrlen);

  fixture->client_channel = g_io_channel_unix_new (fixture->client_socket_fd);
}

static void
test_httpd_fixture_tear_down (test_httpd_fixture *fixture,
			      gconstpointer user_data)
{
  g_object_unref (fixture->http_server);
  g_io_channel_unref (fixture->client_channel);
}

static void
assert_next_line (GIOChannel *channel, char *reference)
{
  GError *err = NULL;
  char *response_line = NULL;
  
  g_io_channel_read_line (channel, &response_line, NULL, NULL, &err);
  g_assert_no_error (err);

  g_assert_cmpstr (response_line, ==, reference);
  g_free (response_line);
}

static void
skip_headers (GIOChannel *channel)
{
  GError *err = NULL;
  char *response_line = NULL;

  while (TRUE)
    {      
      g_io_channel_read_line (channel, &response_line, NULL, NULL, &err);
      g_assert_no_error (err);

      if (strcmp ("\r\n", response_line) == 0)
	{
	  g_free (response_line);
	  break;
	}
      else g_free (response_line);
    }
}

static void
test_add_terminal_simple (test_httpd_fixture *fixture, gconstpointer user_data)
{
  GError *err = NULL;

  gzochid_httpd_add_terminal
    (fixture->http_server, "/", test_terminal_handler, NULL);

  g_io_channel_write_chars
    (fixture->client_channel, "GET / HTTP/1.1\r\n\r\n", 18, NULL, &err);
  g_assert_no_error (err);
  
  g_io_channel_flush (fixture->client_channel, &err);
  g_assert_no_error (err);

  assert_next_line (fixture->client_channel, "HTTP/1.1 200 OK\r\n");
  skip_headers (fixture->client_channel);
  assert_next_line (fixture->client_channel, "SUCCESS\n");
}

static void
test_add_continuation_simple (test_httpd_fixture *fixture,
			      gconstpointer user_data)
{
  GError *err = NULL;
  char *response_line = NULL;
  gzochid_httpd_partial *partial = gzochid_httpd_add_continuation
    (fixture->http_server, "/foo", test_continuation_handler, NULL);

  gzochid_httpd_append_terminal
    (partial, "/", test_continuation_terminal, NULL);  

  g_io_channel_write_chars
    (fixture->client_channel, "GET /foo/ HTTP/1.1\r\n\r\n", 22, NULL, NULL);
  
  g_io_channel_flush (fixture->client_channel, NULL);

  assert_next_line (fixture->client_channel, "HTTP/1.1 200 OK\r\n");
  skip_headers (fixture->client_channel);
  assert_next_line (fixture->client_channel, "SUCCESS\n");
}

static void
test_get_base_url (test_httpd_fixture *fixture, gconstpointer user_data)
{
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof (struct sockaddr_in);

  const char *base_url = gzochid_http_server_get_base_url
    (fixture->http_server);
  gchar *expected_url = NULL;

  _gzochid_http_server_getsockname
    (fixture->http_server, (struct sockaddr *) &addr, &addrlen);
  expected_url = g_strdup_printf
    ("http://127.0.0.1:%d/", ntohs (addr.sin_port));

  g_assert_cmpstr (base_url, ==, expected_url);
  g_free (expected_url);
}

int
main (int argc, char *argv[])
{
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  g_test_init (&argc, &argv, NULL);

  g_test_add
    ("/httpd/add_terminal/simple", test_httpd_fixture, NULL,
     test_httpd_fixture_set_up, test_add_terminal_simple,
     test_httpd_fixture_tear_down);
  g_test_add
    ("/httpd/add_continuation/simple", test_httpd_fixture, NULL,
     test_httpd_fixture_set_up, test_add_continuation_simple,
     test_httpd_fixture_tear_down);
  g_test_add
    ("/httpd/get_base_url/simple", test_httpd_fixture, NULL,
     test_httpd_fixture_set_up, test_get_base_url,
     test_httpd_fixture_tear_down);
  
  return g_test_run ();
}
