/* Administrative context routines for gzochid
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
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "admin.h"
#include "config.h"
#include "context.h"
#include "debug.h"
#include "gzochid.h"
#include "httpd.h"
#include "httpd-app.h"
#include "resolver.h"
#include "threads.h"

#include "api/admin.h"

struct _gzochid_admin_context
{
  gzochid_context base;

  GzochidRootContext *root_context;
  GzochidHttpServer *http_server;
  
  GThreadPool *pool;
  GHashTable *config;
};

static void *
initialize_guile (void *data)
{
  gzochid_api_admin_init (data);
  return NULL;
}

static void 
initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_admin_context *admin_context = (gzochid_admin_context *) context;

  admin_context->pool = 
    gzochid_thread_pool_new 
    (context, gzochid_config_to_int 
     (g_hash_table_lookup 
      (admin_context->config, "thread_pool.max_threads"), 4), 
     TRUE, NULL);

  if (gzochid_config_to_boolean 
      (g_hash_table_lookup 
       (admin_context->config, "module.httpd.enabled"), FALSE))
    {
      int port = gzochid_config_to_int 
	(g_hash_table_lookup (admin_context->config, "module.httpd.port"), 
	 8000);
      
      admin_context->http_server = gzochid_resolver_require_full
	(admin_context->root_context->resolution_context,
	 GZOCHID_TYPE_HTTP_SERVER, NULL);
      
      gzochid_httpd_app_register_handlers
	(admin_context->http_server, (gzochid_game_context *)
	 admin_context->root_context->game_server,
	 admin_context->root_context->resolution_context);

      gzochid_http_server_start (admin_context->http_server, port, NULL);
    }
  if (gzochid_config_to_boolean 
      (g_hash_table_lookup 
       (admin_context->config, "module.debug.enabled"), FALSE))
    {
      int port = gzochid_config_to_int 
	(g_hash_table_lookup (admin_context->config, "module.debug.port"), 
	 37146);

      gzochid_debug_context *debug_context = gzochid_debug_context_new ();
      gzochid_debug_context_init (debug_context, context, port);
    }
  
  scm_with_guile (initialize_guile, admin_context->root_context->game_server);
  gzochid_fsm_to_state (context->fsm, GZOCHID_ADMIN_STATE_RUNNING);
}

gzochid_admin_context *
gzochid_admin_context_new (void)
{
  return calloc (1, sizeof (gzochid_admin_context));
}

void 
gzochid_admin_context_free (gzochid_admin_context *context)
{
  gzochid_context_free ((gzochid_context *) context);

  if (context->root_context != NULL)
    g_object_unref (context->root_context);
  if (context->http_server != NULL)
    g_object_unref (context->http_server);

  free (context);
}

void 
gzochid_admin_context_init (gzochid_admin_context *context, 
			    GzochidRootContext *root_context)
{
  gzochid_fsm *fsm = gzochid_fsm_new
    ("admin", GZOCHID_ADMIN_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_ADMIN_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_ADMIN_STATE_STOPPED, "STOPPED");

  context->root_context = g_object_ref (root_context);
  context->config = gzochid_configuration_extract_group
    (root_context->configuration, "admin");

  gzochid_fsm_add_transition 
    (fsm, GZOCHID_ADMIN_STATE_INITIALIZING, GZOCHID_ADMIN_STATE_RUNNING);
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_ADMIN_STATE_RUNNING, GZOCHID_ADMIN_STATE_STOPPED);

  gzochid_fsm_on_enter
    (fsm, GZOCHID_ADMIN_STATE_INITIALIZING, initialize, context);

  gzochid_context_init ((gzochid_context *) context, NULL, fsm);
}
