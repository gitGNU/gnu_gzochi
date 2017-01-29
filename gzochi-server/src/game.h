/* game.h: Prototypes and declarations for game.c
 * Copyright (C) 2017 Julian Graham
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
#include <glib-object.h>

#include "app.h"

/* The core game server type definitions. */

#define GZOCHID_TYPE_GAME_SERVER gzochid_game_server_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_game_server_get_type (void);

typedef struct _GzochidGameServer GzochidGameServer;

struct _GzochidGameServerClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidGameServerClass GzochidGameServerClass;

static inline GzochidGameServer *
GZOCHID_GAME_SERVER (gconstpointer ptr)
{
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_game_server_get_type (), GzochidGameServer);
}

/* End boilerplate. */

/*
  Starts the specified game server, preparing it to begin servicing requests 
  from clients. The sides effects of this process include:

  - The server binds itself to the configured `server.port' and begins listening
  for connections.

  - The application deployment directory is scanned for applications, and
  any applications discovered will be bootstrapped. 

  An error is signaled if the server cannot be started.
*/

void gzochid_game_server_start (GzochidGameServer *, GError **);

gzochid_application_context *gzochid_game_server_lookup_application
(GzochidGameServer *, const char *);
GList *gzochid_game_server_get_applications (GzochidGameServer *);

#endif /* GZOCHID_GAME_H */
