/* Administrative context routines for gzochid
 * Copyright (C) 2015 Julian Graham
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
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "admin.h"
#include "config.h"
#include "context.h"
#include "debug.h"
#include "gzochid.h"
#include "httpd.h"
#include "threads.h"

#include "api/admin.h"

static void 
initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_ADMIN_STATE_RUNNING);
}

static void *
initialize_guile (void *data)
{
  gzochid_context *context = data;
  gzochid_server_context *server_context = 
    (gzochid_server_context *) context->parent;

  gzochid_api_admin_init 
    ((gzochid_game_context *) server_context->game_context);

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

      gzochid_httpd_context *httpd_context = gzochid_httpd_context_new ();
      gzochid_httpd_context_init (httpd_context, context, port);
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
  
  scm_with_guile (initialize_guile, context);
  gzochid_thread_pool_push (admin_context->pool, initialize_async, NULL, NULL);
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
  free (context);
}

void 
gzochid_admin_context_init (gzochid_admin_context *context, 
			    gzochid_context *parent, GHashTable *config)
{
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("admin", GZOCHID_ADMIN_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_ADMIN_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_ADMIN_STATE_STOPPED, "STOPPED");

  context->config = g_hash_table_ref (config);

  gzochid_fsm_add_transition 
    (fsm, GZOCHID_ADMIN_STATE_INITIALIZING, GZOCHID_ADMIN_STATE_RUNNING);
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_ADMIN_STATE_RUNNING, GZOCHID_ADMIN_STATE_STOPPED);

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_ADMIN_STATE_INITIALIZING, initialize, context);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}
