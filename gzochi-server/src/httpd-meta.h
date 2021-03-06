/* httpd-meta.h: Prototypes and declarations for httpd-meta.c
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

#ifndef GZOCHID_HTTPD_META_H
#define GZOCHID_HTTPD_META_H

#include "event.h"
#include "httpd.h"
#include "resolver.h"

/*
  Configure the specified HTTP server context with a RESTful hierarchy of 
  handlers for providing feedback about the state of a running gzochi meta
  server.

  The resource tree looks like:

  / - Server root

  The specified `GzochidResolutionContext' is used to resolve other parts of
  the meta server's infrastructure. 
*/

void gzochid_httpd_meta_register_handlers (GzochidHttpServer *,
					   gzochid_event_source *,
					   GzochidResolutionContext *);

#endif /* GZOCHID_HTTPD_META_H */
