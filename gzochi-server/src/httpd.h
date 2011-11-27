/* httpd.h: Prototypes and declarations for httpd.c
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

#ifndef GZOCHID_HTTPD_H
#define GZOCHID_HTTPD_H

#include <stdint.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <microhttpd.h>

#include "context.h"

enum gzochid_httpd_state 
  {
    GZOCHID_HTTPD_STATE_INITIALIZING,
    GZOCHID_HTTPD_STATE_RUNNING,
    GZOCHID_HTTPD_STATE_STOPPED
  };

typedef struct _gzochid_httpd_context 
{
  gzochid_context base;
  
  struct MHD_Daemon *daemon;
  int port;
} gzochid_httpd_context;

gzochid_httpd_context *gzochid_httpd_context_new (void);
void gzochid_httpd_context_free (gzochid_httpd_context *);
void gzochid_httpd_context_init 
(gzochid_httpd_context *, gzochid_context *, int);

#endif /* GZOCHID_HTTPD_H */
