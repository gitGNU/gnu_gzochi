/* lock.h: SPI for gzochid lock table
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

#ifndef GZOCHID_LOCK_H
#define GZOCHID_LOCK_H

#include <glib.h>
#include <sys/time.h>

/* The following data structures and prototypes describe an asynchronous lock
   table, a resource-management strategy used by the data server to control
   access by game application nodes to objects in a game application data store 
   without requiring the nodes to wait or sleep.

   Like the fine-grained locking provided by the storage system, locks on keys 
   in the lock table can be declared with intention (to read or write) and locks
   can be created on ranges of keys that may or may not currently exist. */

/* The lock table structure. */

typedef struct _gzochid_lock_table gzochid_lock_table;

/* 
   Create and return a pointer to a new lock table structure, which will use the
   specified `GDestroyNotify' (if provided) to free the memory associated with
   keys when they are removed from the table. The interpretation of the path 
   argument is implementation-specific.  

   The pointer returned from this function should be freed via
   `gzochid_lock_table_free'.
*/

gzochid_lock_table *gzochid_lock_table_new (const char *, GDestroyNotify);

/* Frees the resources (including keys) associated with the specified lock 
   table. */

void gzochid_lock_table_free (gzochid_lock_table *);

/* 
   Attempts to obtain a read or write lock on the specified key on behalf of the
   specified node. Returns `TRUE' if the attempt was successful; returns `FALSE'
   if the lock could not be obtained and sets value of the specified timestamp 
   (if provided) to the most recent creation or modification timestamp of the 
   conflicting lock, which may be another point lock or a range lock held by 
   another node. 

   A read lock can only be obtained when no other nodes have a write lock on the
   specified key.

   A write lock can only be obtained when no other nodes have any kind of lock
   on the specified key and no other nodes have obtained a range lock on an
   interval that covers the key. 
*/

gboolean gzochid_lock_check_and_set
(gzochid_lock_table *, guint, GBytes *, gboolean, struct timeval *);


/* 
   Attempts to obtain a range lock on the specified key interval on behalf of 
   the specified node. Returns `TRUE' if the attempt was successful; returns 
   `FALSE' if the lock could not be obtained and sets the value of the specified
   timestamp (if provided) to the most recent creation or modification timestamp
   of the conflicting lock, which may be an overlapping range lock or a point 
   lock held by another node. 

   Range locks are (currently) exclusive and can only be obtained when no other
   nodes hold range locks that overlap the specified interval and no other nodes
   hold write locks on keys that fall within the interval.
*/

gboolean gzochid_lock_range_check_and_set
(gzochid_lock_table *, guint, GBytes *, GBytes *, struct timeval *);


/* Completely releases the specified node's lock on the specified point lock. */

void gzochid_lock_release (gzochid_lock_table *, guint, GBytes *);

/* Releases the specified node's range lock on the specified key interval. */

void gzochid_lock_release_range
(gzochid_lock_table *, guint, GBytes *, GBytes *);

/* Releases all the point and range locks held by the specified node. */

void gzochid_lock_release_all (gzochid_lock_table *, guint);

#endif /* GZOCHID_LOCK_H */
