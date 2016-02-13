/* resolver.h: Prototypes and declarations for resolver.c
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

#ifndef GZOCHID_RESOLVER_H
#define GZOCHID_RESOLVER_H

#include <glib.h>
#include <glib-object.h>

/* 
   The following API provides a minimal constructor dependency injection 
   framework based on GObject. Only GObject-derived dependencies, expressed in
   the form of writable constructor parameters, are eligible for injection; and
   only these types will be injected. Within a particular resolution context 
   (see below) all injected dependencies are singletons, drawn from the context
   "instance cache."

   Types that cannot be injected include: Types with circular dependencies, 
   types with non-GObject "construct only" constructor parameters.
*/

/* The resolution context is an opaque type that maintains the state of 
   dependency resolution, in particular the cache of constructed instances that
   are candidates for being injected into new objects. Note that a resolution 
   context is always registered in its own instance cache, so that it is always
   available as a candidate for injection. */

#define GZOCHID_TYPE_RESOLUTION_CONTEXT gzochid_resolution_context_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_resolution_context_get_type (void);

typedef struct _GzochidResolutionContext GzochidResolutionContext;

struct _GzochidResolutionContextClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidResolutionContextClass GzochidResolutionContextClass;

static inline GzochidResolutionContext *
GZOCHID_RESOLUTION_CONTEXT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_resolution_context_get_type (), GzochidResolutionContext);
}

/* End boilerplate. */

enum GzochidResolutionError
  {
    /* A circular dependency was discovered during resolution. */
    
    GZOCHID_RESOLUTION_ERROR_CIRCULAR_DEPENDENCY,

    /* One or more of the types required during resolution has a constructor 
       that cannot be injected because it has a parameter that can only be set
       at construction time but which cannot be created by the resolution
       framework (i.e., it is not a `GObject'). */
    
    GZOCHID_RESOLUTION_ERROR_INCOMPATIBLE_CONSTRUCTOR,

    /* Generic resolution failure. */
    
    GZOCHID_RESOLUTION_ERROR_FAILED
  };

/* The error domain for resolution errors. Error codes for errors in this domain
   will be values from the `GzochidResolutionError' enum above. */

#define GZOCHID_RESOLUTION_ERROR gzochid_resolution_error_quark ()

/* Construct and return an object of the specified type, recursively 
   constructing and injecting any eligible dependencies as necessary.

   This function creates a "single use" resolution context and then delegates to
   `gzochid_resolver_require_full' below. */

gpointer gzochid_resolver_require (GType, GError **);

/* Construct and return an object of the specified type, recursively 
   constructing and injecting any eligible dependencies as necessary, using the
   instance cache in the specified resolution context. */

gpointer gzochid_resolver_require_full
(GzochidResolutionContext *, GType, GError **);

GQuark gzochid_resolution_error_quark ();

#endif /* GZOCHID_RESOLVER_H */
