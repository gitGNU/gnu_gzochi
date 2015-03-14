/* lifecycle.h: Prototypes and declarations for lifecycle.c
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

#ifndef GZOCHID_LIFECYCLE_H
#define GZOCHID_LIFECYCLE_H

#include "app.h"
#include "protocol.h"

void gzochid_application_client_logged_in (gzochid_application_context *, 
					   gzochid_protocol_client *);

void gzochid_application_client_disconnected (gzochid_application_context *, 
					      gzochid_protocol_client *);

void gzochid_application_session_received_message 
(gzochid_application_context *, gzochid_protocol_client *, unsigned char *, 
 short);

#endif /* GZOCHID_LIFECYCLE_H */
