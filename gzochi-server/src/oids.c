/* oids.c: Object id block allocation strategy framework code for gzochid
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>

#include "oids.h"

/* The oid allocation strategy structure: An allocation function plus an opaque
   user data pointer - and a cleanup function. */

struct _gzochid_oid_allocation_strategy
{
  /* The oid allocation function. */
  
  gzochid_oid_allocation_func allocation_function; 

  /* The user data pointer / "closure" for the allocation function. */

  gpointer user_data; 

  /* The free function for the user data pointer; optionally `NULL'. */
  
  GDestroyNotify free_func; 
};

gzochid_oid_allocation_strategy *
gzochid_oid_allocation_strategy_new
(gzochid_oid_allocation_func allocation_function, gpointer user_data,
 GDestroyNotify free_func)
{
  gzochid_oid_allocation_strategy *strategy =
    malloc (sizeof (gzochid_oid_allocation_strategy));

  strategy->allocation_function = allocation_function;
  strategy->user_data = user_data;

  strategy->free_func = free_func;
  
  return strategy;
}

void
gzochid_oid_allocation_strategy_free (gzochid_oid_allocation_strategy *strategy)
{
  /* Apply the `GDestroyNotify' - if there is one - to the user data pointer. */
  
  if (strategy->free_func != NULL)
    strategy->free_func (strategy->user_data);

  free (strategy); /* Then free the strategy itself. */
}

gboolean
gzochid_oids_reserve_block (gzochid_oid_allocation_strategy *strategy,
			    gzochid_data_oids_block *ret, GError **err)
{
  /* Invoke the allocation strategy "closure." */
  
  return strategy->allocation_function (strategy->user_data, ret, err);
}

GQuark
gzochid_oids_error_quark ()
{
  return g_quark_from_static_string ("gzochid-oids-error-quark");
}
