/* itree.h: Prototypes and declarations for itree.c
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

#ifndef GZOCHID_ITREE_H
#define GZOCHID_ITREE_H

#include <glib.h>

typedef struct _gzochid_itree gzochid_itree;

/* 
   Typedef for the interval search tree function. Called with the lower and 
   upper bounds of the overlapping interval, the data associated with the
   overlapping interval, and the "user data" pointer passed to the search
   function. 

   Return `TRUE' from this function to halt a traversal, `FALSE' to continue.
*/

typedef gboolean (*gzochid_itree_search_func) (gpointer, gpointer, gpointer,
					       gpointer);

/* Create a new interval tree. */

gzochid_itree *gzochid_itree_new (GCompareFunc, GCompareFunc);

/* Free the interval tree structure, including the internal nodes. */

void gzochid_itree_free (gzochid_itree *);

/* Insert the specified interval into the tree and associate it with the 
   specified value. If the interval is already in the tree, the associated value
   is replaced. */

void gzochid_itree_insert (gzochid_itree *, gpointer, gpointer, gpointer);

/* Remove the specified interval and its associated data from the tree. */

void gzochid_itree_remove (gzochid_itree *, gpointer, gpointer);

/* Search the specified interval tree for intervals overlapping the specified 
   point (as indicated by the tree's comparator) invoking the specified search 
   function with the specified "user data" pointer for every match. */

void gzochid_itree_search (gzochid_itree *, gpointer, gzochid_itree_search_func,
			   gpointer);

/* Search the specified interval tree for intervals overlapping the specified 
   interval (as indicated by the tree's comparator) invoking the specified 
   search function with the specified "user data" pointer for every match. */

void gzochid_itree_search_interval (gzochid_itree *, gpointer, gpointer,
				    gzochid_itree_search_func, gpointer);


#endif /* GZOCHID_ITREE_H */
