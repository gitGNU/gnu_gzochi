/* auth_int.h: Internal API and prototypes for auth.c
 * Copyright (C) 2013 Julian Graham
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

#ifndef GZOCHID_AUTH_INT_H
#define GZOCHID_AUTH_INT_H

#include <glib.h>

struct _gzochid_application_context;
struct _gzochid_game_context;

void gzochid_auth_identity_serializer 
(struct _gzochid_application_context *, void *, GString *);
void *gzochid_auth_identity_deserializer
(struct _gzochid_application_context *, GString *);
void gzochid_auth_identity_finalizer
(struct _gzochid_application_context *, void *);

#endif /* GZOCHID_AUTH_INT_H */
