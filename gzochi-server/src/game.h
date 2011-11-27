/* game.h: Prototypes and declarations for game.c
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

#ifndef GZOCHID_GAME_H
#define GZOCHID_GAME_H

#include <glib.h>

#include "app.h"
#include "context.h"
#include "socket.h"

enum gzochid_game_state 
  {
    GZOCHID_GAME_STATE_INITIALIZING,
    GZOCHID_GAME_STATE_RUNNING,
    GZOCHID_GAME_STATE_STOPPED
  };

typedef struct _gzochid_game_context 
{
  gzochid_context base;
  GThreadPool *pool;
  
  int port;
  char *apps_dir;
  char *work_dir;

  GHashTable *applications;
  gzochid_socket_server_context *server;

} gzochid_game_context;

gzochid_game_context *gzochid_game_context_new (void);
void gzochid_game_context_free (gzochid_game_context *);
void gzochid_game_context_init 
(gzochid_game_context *, gzochid_context *, GHashTable *);

void gzochid_game_context_register_application
(gzochid_game_context *, char *, gzochid_application_context *);
void gzochid_game_context_unregister_application
(gzochid_game_context *, char *);
gzochid_application_context *gzochid_game_context_lookup_application
(gzochid_game_context *, char *);

#endif /* GZOCHID_GAME_H */
