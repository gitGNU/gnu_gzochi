/* httpd-app.h: Prototypes and declarations for httpd-app.c
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

#ifndef GZOCHID_HTTPD_APP_H
#define GZOCHID_HTTPD_APP_H

#include "game.h"
#include "httpd.h"

/* Configure the specified HTTP server context with a RESTful hierarchy of 
   handlers for providing feedback about the state of a running gzochi 
   application server.

   The resource tree looks like:

   / - Server root
     /app/ - List of running applicatons
       /[appname] - Application status summary
         /names/ - List of name bindings
         /oids/ - List of object ids
	   [oid] - Object contents, in hex dump format.

  The specified `gzochid_game_context' is used to resolve application name
  references. */

void gzochid_httpd_app_register_handlers
(GzochidHttpServer *, gzochid_game_context *);

#endif /* GZOCHID_HTTPD_APP_H */
