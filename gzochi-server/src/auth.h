/* auth.h: Prototypes and declarations for auth.c
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

#ifndef GZOCHID_AUTH_H
#define GZOCHID_AUTH_H

#include <glib.h>

typedef struct _gzochid_auth_identity
{
  char *name;
} gzochid_auth_identity;

struct _gzochid_application_context;

gzochid_auth_identity *gzochid_auth_function_pass_thru
(struct _gzochid_application_context *, unsigned char *, short);
gzochid_auth_identity *gzochid_auth_function_scheme
(struct _gzochid_application_context *, unsigned char *, short);

void gzochid_auth_identity_serializer 
(struct _gzochid_application_context *, void *, GString *);
void *gzochid_auth_identity_deserializer
(struct _gzochid_application_context *, GString *);

#endif /* GZOCHID_AUTH_H */
