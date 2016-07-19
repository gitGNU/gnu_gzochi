/* auth_int.h: Internal API and prototypes for auth.c
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

#ifndef GZOCHID_AUTH_INT_H
#define GZOCHID_AUTH_INT_H

#include <glib.h>

#include "gzochid-auth.h"
#include "lrucache.h"

struct _gzochid_application_context;
struct _gzochid_game_context;

void gzochid_auth_identity_serializer 
(struct _gzochid_application_context *, void *, GByteArray *, GError **);
void *gzochid_auth_identity_deserializer
(struct _gzochid_application_context *, GByteArray *, GError **);
void gzochid_auth_identity_finalizer
(struct _gzochid_application_context *, void *);

/* The identity cache struct typedef. (The contents of the struct are private.) 
   
   The identity cahce stores recently-used instances of `gzochid_auth_identity'
   to avoid unnecessary re-allocation and provide a single point of access for
   non-plugin-originated identities. */

typedef struct _gzochid_auth_identity_cache gzochid_auth_identity_cache;

/* Returns the "system identity," a special identity to be used for executing
   tasks not related to any one user, such as sweeping expired sessions on 
   startup. 

   The system identity is always available and does not participate in identity
   caching. As such, the `ref' and `unref' operations have no effect on it. */

gzochid_auth_identity *gzochid_auth_system_identity ();

/* Create and return a new identity cache. */

gzochid_auth_identity_cache *gzochid_auth_identity_cache_new ();

/* Destroy the specified identity cache, decreasing the reference count for (but
   not necessarily freeing) all currently-cached identities. */

void gzochid_auth_identity_cache_destroy (gzochid_auth_identity_cache *);

/* Look up and return from the cache an identity corresponding to the specified
   name. If an existing identity with this name is not already cached, a new one
   will be constructed and added to the cache, possibly triggering the eviction
   of a less recently accessed identity from the cache. */

gzochid_auth_identity *gzochid_auth_identity_from_name
(gzochid_auth_identity_cache *, char *);

void gzochid_auth_init (struct _gzochid_game_context *);

#endif /* GZOCHID_AUTH_INT_H */
