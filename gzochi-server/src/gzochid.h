/* gzochid.h: Prototypes and declarations for gzochid.c
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

#ifndef GZOCHID_H
#define GZOCHID_H

#include <glib.h>
#include <glib-object.h>

#include "config.h"
#include "metaclient.h"
#include "event.h"
#include "resolver.h"
#include "socket.h"

#define GZOCHID_TYPE_ROOT_CONTEXT gzochid_root_context_get_type ()

/* Boilerplate setup for the gzochid root context. */

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_root_context_get_type (void);

struct _GzochidRootContextClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidRootContextClass GzochidRootContextClass;

/* The root context object. */

struct _GzochidRootContext
{
  GObject parent_intance;
  
  const char *gzochid_conf_path;
  GzochidConfiguration *configuration; /* The server configuration. */
  GzochidSocketServer *socket_server; /* The global socket server. */  
  GzochidEventLoop *event_loop; /* The global event loop. */

  /*
    A reference to the resolution context that "owns" the root context, for
    use in bootstrapping other components that don't yet support automatic
    resolution.
  */

  GzochidResolutionContext *resolution_context;

  /* The metaclient container. */

  GzochidMetaClientContainer *metaclient_container; 
  
  /* Components that can't yet be auto-resolved. */

  gzochid_context *admin_context; /* The admin service context. */
  gzochid_context *game_server; /* The game server context. */
};

typedef struct _GzochidRootContext GzochidRootContext;

static inline GzochidRootContext *
GZOCHID_ROOT_CONTEXT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_root_context_get_type (), GzochidRootContext);
}

/* End boilerplate. */

#endif /* GZOCHID_H */
