/* debug.c: Remote debugging module for gzochid
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

#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "admin.h"
#include "context.h"
#include "debug.h"
#include "fsm.h"
#include "guile.h"
#include "log.h"
#include "threads.h"

static SCM scm_run_repl_server;

static void 
initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_DEBUG_STATE_RUNNING);
}

static void *
initialize_guile (void *data)
{
  SCM gzochi_private_debug = scm_c_resolve_module ("gzochi private debug");
  SCM run_repl_server_var = scm_c_module_lookup 
    (gzochi_private_debug, "gzochi:run-repl-server");
  
  scm_run_repl_server = scm_variable_ref (run_repl_server_var);
  scm_gc_protect_object (scm_run_repl_server);

  return NULL;
}

static void 
initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_admin_context *admin_context =
    (gzochid_admin_context *) context->parent;

  scm_with_guile (initialize_guile, NULL);

  gzochid_thread_pool_push
    (admin_context->pool, initialize_async, context, NULL);
}

static void *
run_repl_server (void *data)
{
  gzochid_debug_context *context = data;

  SCM port = scm_from_int (context->port);

  gzochid_guile_invoke 
    (scm_run_repl_server, 
     scm_list_2 (scm_from_locale_keyword ("port"), port), 
     SCM_BOOL_F);

  return NULL;
}

static void 
run_async (gpointer data, gpointer user_data)
{
  scm_with_guile (run_repl_server, data);
}

static void 
run (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = user_data;
  gzochid_debug_context *debug_context = (gzochid_debug_context *) context;
  gzochid_admin_context *admin_context =
    (gzochid_admin_context *) context->parent;

  gzochid_notice ("Debug server listening on port %d", debug_context->port);
  gzochid_thread_pool_push (admin_context->pool, run_async, user_data, NULL);  
}

static void 
stop (int from_state, int to_state, gpointer user_data)
{
}

gzochid_debug_context *
gzochid_debug_context_new (void)
{
  return calloc (1, sizeof (gzochid_debug_context));
}

void 
gzochid_debug_context_free (gzochid_debug_context *context)
{
  gzochid_context_free ((gzochid_context *) context);
}

void 
gzochid_debug_context_init (gzochid_debug_context *context, 
			    gzochid_context *parent, int port)
{
  gzochid_fsm *fsm = gzochid_fsm_new
    ("debug", GZOCHID_DEBUG_STATE_INITIALIZING, "INITIALIZING");
  
  gzochid_fsm_add_state (fsm, GZOCHID_DEBUG_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_DEBUG_STATE_STOPPED, "STOPPED");
  
  gzochid_fsm_add_transition
    (fsm, GZOCHID_DEBUG_STATE_INITIALIZING, GZOCHID_DEBUG_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_DEBUG_STATE_RUNNING, GZOCHID_DEBUG_STATE_STOPPED);

  gzochid_fsm_on_enter
    (fsm, GZOCHID_DEBUG_STATE_INITIALIZING, initialize, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_DEBUG_STATE_RUNNING, run, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_DEBUG_STATE_STOPPED, stop, context);

  context->port = port;

  gzochid_context_init ((gzochid_context *) context, parent, fsm);  
}
