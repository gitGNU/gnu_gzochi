/* gzochid.c: Main server bootstrapping routines for gzochid
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

#include <stdio.h>
#include <stdlib.h>

#include "admin.h"
#include "config.h"
#include "context.h"
#include "fsm.h"
#include "game.h"
#include "gzochid.h"
#include "threads.h"

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;

  gzochid_admin_context *admin_context = gzochid_admin_context_new ();
  gzochid_game_context *game_context = gzochid_game_context_new ();

  GKeyFile *key_file = g_key_file_new ();
  GHashTable *admin_config = NULL;
  GHashTable *game_config = NULL;  
  
  g_key_file_load_from_file 
    (key_file, "/etc/gzochid/server.conf", G_KEY_FILE_NONE, NULL);

  admin_config = gzochid_config_keyfile_extract_config (key_file, "admin");
  game_config = gzochid_config_keyfile_extract_config (key_file, "game");

  gzochid_admin_context_init (admin_context, context, admin_config);
  gzochid_game_context_init (game_context, context, game_config);

  gzochid_context_until 
    ((gzochid_context *) admin_context, GZOCHID_ADMIN_STATE_RUNNING);
  gzochid_context_until 
    ((gzochid_context *) game_context, GZOCHID_GAME_STATE_RUNNING);

  gzochid_fsm_to_state (context->fsm, GZOCHID_STATE_RUNNING);
}

static void initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_server_context *context = (gzochid_server_context *) user_data;
  gzochid_thread_pool_push (context->pool, initialize_async, NULL, NULL);
}

gzochid_server_context *gzochid_server_context_new (void)
{
  return calloc (1, sizeof (gzochid_server_context));
}

void gzochid_server_context_init (gzochid_server_context *context)
{
  gzochid_fsm *fsm = gzochid_fsm_new 
    ("main", GZOCHID_STATE_INITIALIZING, "INITIALIZING");

  gzochid_fsm_add_state (fsm, GZOCHID_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_STATE_STOPPED, "STOPPED");

  context->pool = gzochid_thread_pool_new (context, -1, FALSE, NULL);

  gzochid_fsm_on_enter 
    (fsm, GZOCHID_STATE_INITIALIZING, initialize, context);
  
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_STATE_INITIALIZING, GZOCHID_STATE_RUNNING);
  gzochid_fsm_add_transition 
    (fsm, GZOCHID_STATE_RUNNING, GZOCHID_STATE_STOPPED);

  gzochid_context_init ((gzochid_context *) context, NULL, fsm);
}

void gzochid_server_context_free (gzochid_server_context *context)
{
  gzochid_context *root_context = (gzochid_context *) context;
  gzochid_fsm_free (root_context->fsm);

  free (context);
}

int main (int argc, char *argv[])
{
  gzochid_server_context *context = gzochid_server_context_new ();

  g_thread_init (NULL);

  gzochid_server_context_init (context);
  gzochid_fsm_until (((gzochid_context *) context)->fsm, GZOCHID_STATE_STOPPED);
  gzochid_server_context_free (context);
	   
  return 0;
}
