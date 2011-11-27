/* httpd.c: Embedded informational web server for gzochid
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <stdint.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <microhttpd.h>

#include "admin.h"
#include "context.h"
#include "fsm.h"
#include "log.h"
#include "httpd.h"
#include "threads.h"

static int dispatch (void *cls, struct MHD_Connection *connection, 
		     const char *url, const char *method, const char *version, 
		     const char *upload_data, size_t *upload_data_size, 
		     void **con_cls)
{
  int ret = MHD_NO;
  struct MHD_Response *response = NULL;

  if (strcmp (method, "GET") != 0)
    return ret;

  if (strcmp (url, "/") == 0)
    {
      const char *page = "<html><body>Hello, browser!</body></html>";
      response = MHD_create_response_from_data 
	(strlen (page), (void *) page, TRUE, TRUE);
      ret = MHD_queue_response (connection, MHD_HTTP_OK, response);
    }
  else 
    {
      const char *page = "<html><body>Not found.</body></html>";
      response = MHD_create_response_from_data 
	(strlen (page), (void *) page, TRUE, TRUE);
      ret = MHD_queue_response (connection, 404, response);
    }

  MHD_destroy_response (response);

  return ret;
}

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_HTTPD_STATE_RUNNING);
}

static void initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_httpd_context *httpd_context = (gzochid_httpd_context *) context;
  gzochid_admin_context *admin_context = 
    (gzochid_admin_context *) context->parent;

  httpd_context->daemon = MHD_start_daemon 
    (MHD_USE_SELECT_INTERNALLY, httpd_context->port, NULL, NULL, &dispatch, 
     NULL, MHD_OPTION_END);

  gzochid_notice ("HTTP server listening on port %d", httpd_context->port);

  gzochid_thread_pool_push 
    (admin_context->pool, initialize_async, context, NULL);
}

gzochid_httpd_context *gzochid_httpd_context_new (void)
{
  return calloc (1, sizeof (gzochid_httpd_context));
}

void gzochid_httpd_context_free (gzochid_httpd_context *context)
{
  gzochid_context_free ((gzochid_context *) context);
  free (context);
}

void gzochid_httpd_context_init 
(gzochid_httpd_context *context, gzochid_context *parent, int port)
{
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("httpd", GZOCHID_HTTPD_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_HTTPD_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_HTTPD_STATE_STOPPED, "STOPPED");

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_HTTPD_STATE_INITIALIZING, initialize, context);
   
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_HTTPD_STATE_INITIALIZING, GZOCHID_HTTPD_STATE_RUNNING);
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_HTTPD_STATE_RUNNING, GZOCHID_HTTPD_STATE_STOPPED);

  context->port = port;

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}
