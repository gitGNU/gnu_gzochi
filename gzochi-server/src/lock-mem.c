/* lock-mem.c: In-memory lock table implementation for gzochi-metad
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "lock.h"

/*
  The following data structures and functions provide an implementation of an
  interval tree, which can be used to associate opaque pointers with comparable 
  intervals of an arbitrary, opaque type. Intersections between the intervals in
  the tree and specified external intervals and points of the same type can be 
  computed. Intervals are sorted on lower bound followed by upper bound; subtree
  parents maintain a max upper bound over their children, which can be used for
  subtree pruning during search.

  This tree uses the AVL tree balancing algorithms to maintain an optimal 
  height for the interval tree on key insertion and removal.
*/

/* The interval tree node structure. */

struct _itree_node
{
  /* A pointer to the parent, or NULL if this is the root. */

  struct _itree_node *parent; 

  struct _itree_node *left; /* The left child, or NULL. */
  struct _itree_node *right; /* The right child, or NULL. */
  
  gpointer lower; /* The interval lower bound. */
  gpointer upper; /* The interval upper bound. */

  /* The maximum upper bound across this node and the maximum upper bound of 
     each of the left and right children. */

  gpointer max_upper; 

  gpointer data; /* The data associated with the interval. */

  /* The height difference between the left and right subtrees. As per AVL, if 
     this exceeds [ -1, 1 ], a rebalance is necessary. */

  gint balance_factor; 
};

typedef struct _itree_node itree_node;

/* The top-level interval tree structure. */

struct _itree
{
  itree_node *root; /* The root node. */
  GCompareFunc lower_comparator; /* The interval bound comparison function. */
  GCompareFunc upper_comparator; /* The interval bound comparison function. */
};

typedef struct _itree itree;

/* 
   Typedef for the interval search tree function. Called with the lower and 
   upper bounds of the overlapping interval, the data associated with the
   overlapping interval, and the "user data" pointer passed to the search
   function. 

   Return `TRUE' from this function to halt a traversal, `FALSE' to continue.
*/

typedef gboolean (*itree_search_func) (gpointer, gpointer, gpointer, gpointer);

/* Create a new interval tree node with the specified bounds and data. */

static itree_node *
itree_node_new (gpointer lower, gpointer upper, gpointer data)
{
  itree_node *node = calloc (1, sizeof (itree_node));

  node->lower = lower;
  node->upper = upper;
  node->max_upper = upper;
  node->data = data;

  return node;
}

/* Free the interval tree node. */

static void
free_itree_node (gpointer data)
{
  free (data);
}

/* Create a new interval tree. */

static itree *
itree_new (GCompareFunc lower_comparator, GCompareFunc upper_comparator)
{
  itree *t = malloc (sizeof (itree));

  t->root = NULL;
  t->lower_comparator = lower_comparator;
  t->upper_comparator = upper_comparator;
  
  return t;
}

/* Free the interval tree structure, including the internal nodes. */

static void
itree_free (itree *itree)
{
  if (itree->root != NULL)
    {
      GList *src = g_list_prepend (NULL, itree->root);
      GList *dst = NULL;

      /* Flatten the tree into a list via a pre-order traversal... */
      
      while (src != NULL)
	{
	  itree_node *node = src->data;

	  src = g_list_delete_link (src, src);
	  dst = g_list_prepend (dst, node);	  
	  
	  if (node->left != NULL)
	    {
	      src = g_list_prepend (src, node->left);
	      dst = g_list_prepend (dst, node->left);
	    }
	  if (node->right != NULL)
	    {
	      src = g_list_prepend (src, node->right);
	      dst = g_list_prepend (dst, node->right);
	    }
	}

      /* ...then free all the nodes. */
      
      g_list_free_full (dst, free_itree_node);
    }
  
  free (itree);
}

/* 
   Find and return the node in the interval tree at which the specified interval
   should be added. This node will be an exact match for that interval if it is
   already in the tree, or the node that should be the interval's parent, as an
   in-order successor or predecessor. 

   Returns `NULL' if the tree is empty, indicating that the interval should be
   added as the new root.
*/

static itree_node *
itree_location (itree *itree, gpointer lower, gpointer upper)
{
  itree_node *node = itree->root;

  if (node == NULL)
    return NULL;

  while (TRUE)
    {
      gint lower_comparison = itree->lower_comparator (lower, node->lower);
      gint upper_comparison = itree->upper_comparator (upper, node->upper);

      if (lower_comparison == 0 && upper_comparison == 0)
	return node;
      else if (lower_comparison < 0)
	{
	  if (node->left == NULL)
	    return node;
	  else node = node->left;
	}
      else if (node->right == NULL)
	return node;
      else node = node->right;
    }
}

/* 
   Update the maximum upper bound at the specified node, based on an examination
   of the node itself and, non-recursively, the left and right children. 

   Returns `TRUE' if the maximum upper bound was changed, `FALSE' otherwise.
*/

static gboolean
update_max_upper (itree *itree, itree_node *node)
{
  gpointer new_max_upper = node->upper;

  if (node->left != NULL
      && itree->upper_comparator (node->left->max_upper, new_max_upper) > 0)
    new_max_upper = node->left->max_upper;
  if (node->right != NULL
      && itree->upper_comparator (node->right->max_upper, new_max_upper) > 0)
    new_max_upper = node->right->max_upper;

  if (node->max_upper != new_max_upper)
    {
      node->max_upper = new_max_upper;
      return TRUE;
    }
  else return FALSE;
}

/* Perform an upward rotation of the specified node in the specified direction
   (`TRUE' for left, `FALSE' for right) re-attaching the displaced ancestor 
   nodes to the specified node's descendants accordingly. */

static void
itree_rotate (itree *itree, itree_node *node, gboolean left)
{
  itree_node *parent = node->parent;

  if (parent == NULL)
    {
      if (left)
	node->left = itree->root;
      else node->left = itree->root;
      
      itree->root = node;
      node->parent = NULL;
      
      /* Update the max upper bound of the node in its new location. */

      update_max_upper (itree, node); 
      return;
    }

  if (left)
    {
      parent->right = node->left;
      node->left = parent;
    }
  else
    {
      parent->left = node->right;
      node->right = parent;
    }
  
  /* Update the max upper bound of the node and that of its former parent, since
     it is likely that both have been invalidated. */

  update_max_upper (itree, parent);
  update_max_upper (itree, node);
  
  if (parent->parent == NULL)
    {
      itree->root = node;
      node->parent = NULL;
    }
  else
    {
      if (parent->parent->left == parent)
	parent->parent->left = node;
      else parent->parent->right = node;

      /* ...and update the max upper bound of the grandparent for good 
	 measure. */
      
      update_max_upper (itree, parent->parent);
    }

  parent->parent = node;
}

/* Perform the AVL "retrace insert" process from the specified node up to the 
   root, rotating nodes and recalculating their maximum upper bounds as 
   necessary. */

static void
itree_retrace_insert (itree *itree, itree_node *node)
{
  if (node->balance_factor < -1 || node->balance_factor > 1)
    while (node->parent != NULL)
      {
	itree_node *parent = node->parent;

	gboolean left = parent->left == node;
	gint needs_rotation = left ? 1 : -1;
	gint can_absorb = left ? -1 : 1;
	
	if (parent->balance_factor == needs_rotation)
	  {
	    if (node->balance_factor == can_absorb)
	      itree_rotate (itree, node, left); 
		
	    itree_rotate (itree, parent, !left);
	    break;
	  }
	else if (parent->balance_factor == can_absorb)
	  {
	    parent->balance_factor = 0;
	    break;
	  }
	else parent->balance_factor = needs_rotation; 

	node = node->parent;	
      }

  /* Once we've broken out of the loop, no more rotations are necessary, but
     there might still be max upper bound recalculations to do. So keep doing
     them until they stop having an effect. */
  
  while (node != NULL)
    {
      if (!update_max_upper (itree, node))
	break;

      node = node->parent;
    }
}

/* Insert the specified interval into the tree and associate it with the 
   specified value. If the interval is already in the tree, the associated value
   is replaced. */

static void
itree_insert (itree *itree, gpointer lower, gpointer upper, gpointer data)
{
  itree_node *node = itree_location (itree, lower, upper);

  if (node == NULL)
    itree->root = itree_node_new (lower, upper, data);
  else if (itree->lower_comparator (node->lower, lower) == 0
	   && itree->upper_comparator (node->upper, upper) == 0)
    node->data = data;
  else
    {
      itree_node *new_node = itree_node_new (lower, upper, data);

      new_node->parent = node;      
      
      if (itree->lower_comparator (lower, node->lower) < 0)
	{
	  node->left = new_node;
	  node->balance_factor++; /* Adjust the balance factor. */
	}
      else
	{
	  node->right = new_node;
	  node->balance_factor--; /* Adjust the balance factor. */
	}

      /* Adjust the tree structure as necessary. */
      
      itree_retrace_insert (itree, node);
    }
}

/* Perform the AVL "retrace remove" process from the specified node up to the 
   root, rotating nodes and recalculating their maximum upper bounds as 
   necessary. */

static void
itree_retrace_remove (itree *itree, itree_node *node)
{
  while (node->parent != NULL)
    {
      itree_node *parent = node->parent;

      gboolean right = parent->right == node;
      itree_node *sibling = right ? parent->left : parent->right;
      gint needs_rotation = right ? 1 : -1;
      gint can_absorb = right ? -1 : 1;
      
      if (parent->balance_factor == needs_rotation)
	{
	  if (sibling->balance_factor == can_absorb)
	    itree_rotate (itree, sibling, right);
	  itree_rotate (itree, parent, !right);

	  if (sibling->balance_factor == 0)
	    break;
	}
      if (parent->balance_factor == 0)
	{
	  parent->balance_factor = needs_rotation;
	  break;
	}
      else parent->balance_factor = 0;

      node = node->parent;
    }

  /* As with "retrace insert," there may be some additional bounds 
     recalculations to perform once the rotations are complete. */
  
  while (node != NULL)
    {
      if (!update_max_upper (itree, node))
	break;
      
      node = node->parent;
    }
}

/* Remove the specified interval and its associated data from the tree. */

static void
itree_remove (itree *itree, gpointer lower, gpointer upper)
{
  itree_node *node = itree_location (itree, lower, upper);

  if (node != NULL
      && itree->lower_comparator (node->lower, lower) == 0
      && itree->upper_comparator (node->upper, upper) == 0)
    {
      itree_node *target = NULL;
      itree_node *parent = NULL;
      itree_node *new_child = NULL;

      /* If the target node has both left and right children, it's not feasible
	 to remove it from the structure of the tree. Instead, "swap" its value
         with the value of a node that *is* safe to remove. */
      
      if (node->left != NULL && node->right != NULL)
	{	  
	  target = node->left;
	  
	  while (target->right != NULL)
	    target = target->right;	  

	  node->lower = target->lower;
	  node->upper = target->upper;
	  node->data = target->data;
	}

      /* ...but if it is safe to remove the node, do so. */
      
      else target = node;

      parent = target->parent;
      
      if (target->left != NULL)
	new_child = target->left;
      else if (target->right != NULL)
	new_child = target->right;
      
      if (new_child != NULL)
	new_child->parent = parent;

      if (parent == NULL)
	itree->root = new_child;
      else
	{
	  if (parent->left == target)
	    parent->left = new_child;
	  else if (parent->right == target)
	    parent->right = new_child;
	}

      free_itree_node (target);

      /* If the tree is not empty at this point, do a retrace to the root. */
      
      if (new_child != NULL)
	itree_retrace_remove (itree, new_child);
    }
}

/* Search the specified interval tree for intervals overlapping the specified 
   interval (as indicated by the tree's comparator) invoking the specified 
   search function with the specified "user data" pointer for every match. */

static void
itree_search_interval (itree *itree, gpointer from, gpointer to,
		       itree_search_func search_func, gpointer user_data)
{
  GList *stack = NULL;

  if (itree->root == NULL)
    return;
  
  stack = g_list_prepend (NULL, itree->root);

  while (stack != NULL)
    {
      itree_node *node = stack->data;

      stack = g_list_delete_link (stack, stack);

      if (itree->upper_comparator (from, node->max_upper) > 0
	  && itree->upper_comparator (to, node->max_upper) > 0)

	/* If the interval is entirely outside of the max upper bound in this
	   sub-tree, there's no point in expanding it any further. */
	
	continue;

      if ((itree->lower_comparator (from, node->lower) >= 0
	   && itree->upper_comparator (from, node->upper) <= 0)
	  || (itree->lower_comparator (to, node->lower) >= 0
	      && itree->upper_comparator (to, node->upper) <= 0)
	  || (itree->lower_comparator (from, node->lower) <= 0
	      && itree->upper_comparator (to, node->upper) >= 0))
	if (search_func (node->lower, node->upper, node->data, user_data))
	  {
	    g_list_free (stack);
	    return;
	  }
      
      if (node->right != NULL)
	stack = g_list_prepend (stack, node->right);
      if (node->left != NULL)
	stack = g_list_prepend (stack, node->left);
    }
}

/* Search the specified interval tree for intervals overlapping the specified 
   point (as indicated by the tree's comparator) invoking the specified search 
   function with the specified "user data" pointer for every match. */

static void
itree_search (itree *itree, gpointer point, itree_search_func search_func,
	      gpointer user_data)
{
  GList *stack = NULL;

  if (itree->root == NULL)
    return;
  
  stack = g_list_prepend (NULL, itree->root);

  while (stack != NULL)
    {
      itree_node *node = stack->data;

      stack = g_list_delete_link (stack, stack);

      if (itree->upper_comparator (point, node->max_upper) > 0)

	/* If the point falls beyond the max upper bound in this sub-tree, 
	   there's no point in expanding it any further. */
		
	continue;

      if (itree->lower_comparator (point, node->lower) >= 0
	  && itree->upper_comparator (point, node->upper) <= 0)
	if (search_func (node->lower, node->upper, node->data, user_data))
	  {
	    g_list_free (stack);
	    return;
	  }
      
      if (node->right != NULL)
	stack = g_list_prepend (stack, node->right);
      if (node->left != NULL)
	stack = g_list_prepend (stack, node->left);
    }
}

/* 
   The following data structures and functions provide an implementation of the
   lock table described in `lock.h', capable of intention-oriented locking for 
   individual keys, as well as exclusive locking for ranges of keys. 

   This implementation provides no durable persistence; the state of the lock
   table is kept entirely in process memory.
*/

/* The lock structure for ann individual key. */

struct _gzochid_lock
{
  guint node_id; /* Node id of the locker. */
  
  GBytes *key; /* The key being locked. */
  gboolean for_write; /* Locked for write? */

  /* The last-modified timestamp of the lock, which may be different than the 
     creation timestamp in cases in which a read lock was upgraded for 
     write. */

  struct timeval timestamp; 

  /* A pointer to this lock in the timestamp-ordered lock sequence. */

  GSequenceIter *timestamp_iter; 
};

typedef struct _gzochid_lock gzochid_lock;

/* The lock structure for a range of keys, closed at the boundaries. */

struct _gzochid_range_lock
{
  guint node_id; /* Node id of the locker. */

  GBytes *from; /* The lower bound of the locked range, inclusive. */
  GBytes *to; /* The upper bound of the locked rang, inclusive. */

  struct timeval timestamp; /* The lock creation timestamp. */

  /* A pointer to this lock in the timestamp-ordered range lock sequence. */

  GSequenceIter *timestamp_iter;
};

typedef struct _gzochid_range_lock gzochid_range_lock;

/* A wrapper around a list of compatible (i.e., read) single-key locks, with the
   timestamp of the most recent lock provided as a convenience. */

struct _gzochid_locks
{
  GList *locks; /* The list of `gzochid_lock' structures. */
  struct timeval most_recent_timestamp; /* The most recent lock timestamp. */
};

typedef struct _gzochid_locks gzochid_locks;

/* Represents the complete set of individual and range locks held by a 
   particular locker. In addition to convenience, this structure is the single 
   "owner" of the memory allocated for locks and keys; this memory should only
   be freed as a side effect of operations targeting this structure. */
   
struct _gzochid_node_locks
{
  GList *locks; /* The list of read-write locks held by the node. */
  GList *range_locks; /* The list of range locks held by the node. */
};

typedef struct _gzochid_node_locks gzochid_node_locks;

/* The private lock table structure. */

struct _gzochid_lock_table
{
  /* A sequence of read-write locks, ordered by key, ascending. */

  GSequence *locks; 

  /* A sequence of read-write locks, ordered by timestamp, ascending. */

  GSequence *locks_by_timestamp; 
  
  itree *range_locks; /* The interval tree of range locks. */

  /* A sequence of range locks, ordered by timestamp, ascending. */

  GSequence *range_locks_by_timestamp;

  /* A mapping of node id `guint' pointers to `gzochid_node_locks'. */

  GHashTable *nodes_to_locks; 
};

/* Convenience function to ensure the allocation of a `gzochid_node_locks' 
   structure upon lookup against the lock table with intent to write. */

static gzochid_node_locks *
ensure_node_registration (gzochid_lock_table *lock_table, guint node_id)
{
  if (g_hash_table_contains (lock_table->nodes_to_locks, &node_id))
    return g_hash_table_lookup (lock_table->nodes_to_locks, &node_id);
  else
    {
      guint *node_id_ptr = malloc (sizeof (guint));
      gzochid_node_locks *node_locks = calloc (1, sizeof (gzochid_node_locks));

      *node_id_ptr = node_id;
      
      g_hash_table_insert (lock_table->nodes_to_locks, node_id_ptr, node_locks);

      return node_locks;
    }
}

/* Convenience function for returning a properly-initialized "failure" response
   to a caller from a lock table API function. The specified timestamp gives the
   timestamp at which the conflicting lock was created or last modified. */   

static gboolean
lock_failure (struct timeval lock_timestamp, struct timeval *ret_timestamp)
{
  if (ret_timestamp != NULL)
    {
      ret_timestamp->tv_sec = lock_timestamp.tv_sec;
      ret_timestamp->tv_usec = lock_timestamp.tv_usec;
    }

  return FALSE;
}

/* A `GCompareFunc' implementation for `gzochid_lock' structures, in terms of
   `GBytes' structures. */

static gint
compare_locks (gconstpointer a, gconstpointer b)
{
  const gzochid_lock *lock_a = a;
  const gzochid_lock *lock_b = b;

  return g_bytes_compare (lock_a->key, lock_b->key);
}

/* A `GCompareFunc' implementation for `gzochid_locks' structures, in terms of
   their first `gzochid_lock' element. */

static gint
compare_lock_lists (gconstpointer a, gconstpointer b)
{
  const gzochid_locks *locks_a = a;
  const gzochid_locks *locks_b = b;

  assert (locks_a->locks != NULL);
  assert (locks_b->locks != NULL);

  return compare_locks (locks_a->locks->data, locks_b->locks->data);
}

/* 
   Create and return a pointer to a new `gzochid_lock' structure to lock the
   specified key on behalf of the specified node, for read or write.

   The memory allocated for this structure should be freed via a call to
   `lock_free' when no logner in use.
*/

static gzochid_lock *
lock_new (GBytes *key, guint node_id, gboolean for_write)
{
  gzochid_lock *lock = malloc (sizeof (gzochid_lock));

  lock->key = g_bytes_ref (key);
  lock->node_id = node_id;
  lock->for_write = for_write;

  gettimeofday (&lock->timestamp, NULL);
  lock->timestamp_iter = NULL;

  return lock;
}

/* Free the memory associated with the specifed `gzochid_lock'. */

static void
lock_free (gzochid_lock *lock)
{
  g_bytes_unref (lock->key);
  free (lock);
}

/* 
   Create and return a pointer to a new `gzochid_range_lock' structure to lock 
   the specified range of keys on behalf of the specified node.

   The memory allocated for this structure should be freed via a call to
   `range_lock_free' when no logner in use.
*/

static gzochid_range_lock *
range_lock_new (GBytes *from, GBytes *to, guint node_id)
{
  gzochid_range_lock *range_lock = malloc (sizeof (gzochid_range_lock));

  range_lock->from = from == NULL ? NULL : g_bytes_ref (from);
  range_lock->to = to == NULL ? NULL : g_bytes_ref (to);
  range_lock->node_id = node_id;

  gettimeofday (&range_lock->timestamp, NULL);
  range_lock->timestamp_iter = NULL;  
  
  return range_lock;
}

/* Free the memory associated with the specifed `gzochid_range_lock'. */

static void
range_lock_free (gzochid_range_lock *range_lock)
{
  if (range_lock->from != NULL)
    g_bytes_unref (range_lock->from);
  if (range_lock->to != NULL)
    g_bytes_unref (range_lock->to);
  
  free (range_lock);
}

/* 
   Free the memory associated with the specified `gzochid_locks' structure.

   Note that this function does not free the constituent `gzochid_lock' objects,
   only the structure of the list that contains them. Releasing the memory used
   for the locks must be done separately.
*/

static void
locks_free (gpointer data)
{
  gzochid_locks *locks = data;

  g_list_free (locks->locks);

  free (locks);
}

/* Free the memory associated with the specifed `gzochid_node_locks' 
   structure. */

static void
node_locks_free (gzochid_node_locks *node_locks)
{
  g_list_free_full (node_locks->locks, (GDestroyNotify) lock_free);
  g_list_free_full (node_locks->range_locks, (GDestroyNotify) range_lock_free);
  
  free (node_locks);
}

/* Comparison function that sorts `NULL' before all non-`NULL' values. Use for
   comparing the lower bounds of range locks. */

static gint
bytes_compare_null_first (gconstpointer o1, gconstpointer o2)
{
  if (o1 == NULL)
    return o2 == NULL ? 0 : -1;
  else return o2 == NULL ? 1 : g_bytes_compare (o1, o2);
}

/* Comparison function that sorts `NULL' after all non-`NULL' values. Use for
   comparing the upper bounds of range locks. */

static gint
bytes_compare_null_last (gconstpointer o1, gconstpointer o2)
{
  if (o1 == NULL)
    return o2 == NULL ? 0 : 1;
  else return o2 == NULL ? -1 : g_bytes_compare (o1, o2);
}

/* 
   Create and return a pointer to a new `gzochid_lock_table' structure.

   The memory allocated for this structure should be freed via a call to
   `gzochid_lock_table_free' when no logner in use.
*/

gzochid_lock_table *
gzochid_lock_table_new (const char *scope)
{
  gzochid_lock_table *lock_table = malloc (sizeof (gzochid_lock_table));

  lock_table->locks = g_sequence_new (locks_free);
  lock_table->locks_by_timestamp = g_sequence_new (NULL);

  lock_table->range_locks =
    itree_new (bytes_compare_null_first, bytes_compare_null_last);
  lock_table->range_locks_by_timestamp = g_sequence_new (NULL);
  
  lock_table->nodes_to_locks = g_hash_table_new_full
    (g_int_hash, g_int_equal, (GDestroyNotify) free, NULL);
  
  return lock_table;
}

/* Release the memory allocated for the specified `gzochid_lock_table'. */

void
gzochid_lock_table_free (gzochid_lock_table *lock_table)
{
  GList *node_lock_lists = g_hash_table_get_values (lock_table->nodes_to_locks);
  
  /* `locks_free' takes care of freeing the lock lists; the loop over
     `gzochid_node_locks' below takes care of freeing the individual point 
     locks. */

  g_sequence_free (lock_table->locks); 
  g_sequence_free (lock_table->locks_by_timestamp);

  /* The loop over `gzochid_node_locks' below takes care of freeing the
     individual range locks. */
  
  itree_free (lock_table->range_locks);
  g_sequence_free (lock_table->range_locks_by_timestamp);

  g_list_free_full (node_lock_lists, (GDestroyNotify) node_locks_free);
  g_hash_table_destroy (lock_table->nodes_to_locks);
  
  free (lock_table);
}

/*
  A `GCompareFunc' to be used to find a point lock already held by a node id. 
  
   Despite the prototype, when used with `g_list_find_custom', this function 
   will always be called with a `guint' pointer (to the target node id) as its 
   second argument. 
*/

static gint
find_lock_with_node_id (gconstpointer a, gconstpointer b)
{
  const gzochid_lock *lock = a;
  const guint *node_id_ptr = b;

  guint node_id = *node_id_ptr;
  
  return lock->node_id < node_id ? -1 : lock->node_id > node_id ? 1 : 0;
}

/* Adapts `compare_lock_lists' to a `GCompareDataFunc' for use by 
   `g_sequence_lookup' in `find_locks_by_key'. */

static gint
compare_lock_lists_data (gconstpointer a, gconstpointer b, gpointer user_data)
{
  return compare_lock_lists (a, b);
}

/* Encapsulates contextual data used during the interval tree search for the 
   most recently-acquired range lock over an interval that covers the specified
   key or key range. */

struct _range_lock_search_context
{
  /* The node id to ignore during the search; range locks held by this node 
     don't block the acquisition of a point lock on the specified key or key 
     range. */

  guint excluded_node_id; 

  gzochid_range_lock *range_lock; /* Pointer to store the candidate result. */
};

typedef struct _range_lock_search_context range_lock_search_context;

/* An `itree_search_func' implementation to locate the most recently-acquired 
   range lock covering a specified key. Use with `range_lock_search_context'. */

static gboolean
find_most_recent_covering_range_lock (gpointer from, gpointer to,
				      gpointer data, gpointer user_data)
{
  gzochid_range_lock *range_lock = data;
  range_lock_search_context *search_context = user_data;

  /* Only match range locks that aren't owned by the node id in the search 
     context. */

  if (range_lock->node_id != search_context->excluded_node_id
      && (search_context->range_lock == NULL
	  || timercmp (&search_context->range_lock->timestamp,
		       &range_lock->timestamp, <)))
    search_context->range_lock = range_lock;

  return FALSE;
}

/* Returns the most recently-acquired `gzochid_range_lock' not held by the 
   specified node and covering the specified `GBytes' in the specified
   `gzochid_lock_table', or `NULL' if no such range lock exists. */

static gzochid_range_lock *
most_recent_covering_range_lock (gzochid_lock_table *lock_table,
				 GBytes *key, guint excluded_node_id)
{
  range_lock_search_context search_context;

  search_context.excluded_node_id = excluded_node_id;
  search_context.range_lock = NULL;
  
  itree_search
    (lock_table->range_locks, key, find_most_recent_covering_range_lock,
     &search_context);

  return search_context.range_lock;
}

/* A `GCompareDataFunc' for comparing `gzochid_lock' structures by their lock
   timestamps. (The data argument is ignored.) */

static gint
compare_timestamp_data (gconstpointer a, gconstpointer b, gpointer data)
{
  const gzochid_lock *lock_a = a;
  const gzochid_lock *lock_b = b;

  if (timercmp (&lock_a->timestamp, &lock_b->timestamp, <))
    return -1;
  else if (timercmp (&lock_a->timestamp, &lock_b->timestamp, >))
    return 1;
  else return 0;
}

/* Returns a `GSequenceIter' pointing to the location of the lock for the
   specified key in the specified lock table's point lock sequence, or `NULL' if
   no such lock exists. */

static GSequenceIter *
find_locks_by_key (gzochid_lock_table *lock_table, GBytes *key)
{
  gzochid_lock lock;  
  gzochid_locks locks;
  GSequenceIter *iter = NULL;

  /* Create a fake lock list for comparison with the lock lists in the lock 
     sequence. (The comparison ultimately takes place by key.) */
  
  lock.key = key;
  locks.locks = g_list_prepend (NULL, &lock);
  
  iter = g_sequence_lookup
    (lock_table->locks, &locks, compare_lock_lists_data, NULL);

  g_list_free (locks.locks);

  return iter;
}

gboolean
gzochid_lock_check_and_set (gzochid_lock_table *lock_table, guint node_id,
			    GBytes *key, gboolean for_write,
			    struct timeval *ret_timestamp)
{
  GSequenceIter *iter = find_locks_by_key (lock_table, key);
  
  if (iter != NULL)
    {
      gzochid_locks *locks = g_sequence_get (iter);
      GList *existing_lock_ptr = NULL;

      assert (locks->locks != NULL);      

      /* Find an existing lock for the specified key held by the specified 
	 node. */
      
      existing_lock_ptr = g_list_find_custom
	(locks->locks, &node_id, find_lock_with_node_id);

      if (existing_lock_ptr != NULL)
	{
	  gzochid_lock *existing_lock = existing_lock_ptr->data;

	  /* If it's already locked for write, or if we're not currently trying
	     to lock it for write, then we already have the level of access we
	     need. */
	  
	  if (existing_lock->for_write || !for_write)
	    return TRUE;
	  else if (for_write)
	    {
	      /* If we're trying to lock it for write, though, and there's
		 currently more than one locker, it must mean that there are
		 multiple readers. Find the first one who isn't the node 
		 currently attempting the lock, and return its timestamp as the
		 failure timestamp. */
	      
	      if (g_list_length (locks->locks) > 1)
		{
		  GList *locks_ptr = locks->locks;
		  while (locks_ptr != NULL)
		    {
		      gzochid_lock *lock = locks_ptr->data;

		      if (lock->node_id != node_id)
			return lock_failure (lock->timestamp, ret_timestamp);
		      
		      locks_ptr = locks_ptr->next;
		    }

		  assert (1 == 0);
		}
	      else
		{
		  /* A covering range lock by anotehr node could also prevent 
		     us from acquiring a write lock on the key. */
		  
		  gzochid_range_lock *range_lock =
		    most_recent_covering_range_lock (lock_table, key, node_id);

		  if (range_lock != NULL)
		    return lock_failure (range_lock->timestamp, ret_timestamp);
		}
	    }

	  existing_lock->for_write = TRUE;
	  gettimeofday (&existing_lock->timestamp, NULL);
	  locks->most_recent_timestamp = existing_lock->timestamp;

	  /* If we're upgrading the lock from read to write, count it as "new"
	     insofar as its timestamp is concerned, and update the timestamp
	     sequence accordingly. */
	  
	  g_sequence_sort_changed
	    (existing_lock->timestamp_iter, compare_timestamp_data, NULL);
	      
	  return TRUE;
	}
      else
	{
 	  gzochid_lock *lock = locks->locks->data;

	  if (for_write || lock->for_write)

	    /* If someone has the lock and we don't, there's no way we can lock
	       it for write. */
 	    
	    return lock_failure (lock->timestamp, ret_timestamp);

	  else 
	    {
	      gzochid_node_locks *node_locks =
		ensure_node_registration (lock_table, node_id);
	  
	      gzochid_lock *lock = lock_new (key, node_id, for_write);
	  
	      lock->timestamp_iter = g_sequence_insert_sorted
		(lock_table->locks_by_timestamp, lock, compare_timestamp_data,
		 NULL);
	  
	      locks->locks = g_list_prepend (locks->locks, lock);
	      locks->most_recent_timestamp = lock->timestamp;
	  
	      node_locks->locks = g_list_prepend (node_locks->locks, lock);
	      return TRUE;
	    }
	}
    }
  else if (for_write)
    {
      /* If there are no competitors for write access to the key in the 
	 individual lock space, ensure that there are no range locks held by
	 another node that cover the target key. */
      
      gzochid_range_lock *range_lock =
	most_recent_covering_range_lock (lock_table, key, node_id);
      
      if (range_lock != NULL)
	return lock_failure (range_lock->timestamp, ret_timestamp);
    }

  /* At this point, there are no impediments to creating an entirely new, 
     possibly exclusive lock on the target key. */
  
  { gzochid_node_locks *node_locks =
      ensure_node_registration (lock_table, node_id);
    
    gzochid_lock *lock = lock_new (key, node_id, for_write);
    gzochid_locks *locks = malloc (sizeof (gzochid_locks));
    
    locks->locks = g_list_prepend (NULL, lock);
    locks->most_recent_timestamp = lock->timestamp;      
    g_sequence_insert_sorted
      (lock_table->locks, locks, compare_lock_lists_data, NULL);

    node_locks->locks = g_list_prepend (node_locks->locks, lock);

    /* Add the new lock to the proper place in the timestamp sequence. */
    
    lock->timestamp_iter = g_sequence_insert_sorted
      (lock_table->locks_by_timestamp, lock, compare_timestamp_data,
       NULL);
    
    return TRUE;
  }
}

/* An `itree_search_func' implementation to locate range locks that overlap a 
   specified key interval. Use with `range_lock_search_context'. */

static gboolean
find_overlapping_range_lock (gpointer from, gpointer to, gpointer data,
			     gpointer user_data)
{
  gzochid_range_lock *lock = data;
  range_lock_search_context *search_context = user_data;

  if (lock->node_id != search_context->excluded_node_id)
    {
      search_context->range_lock = lock;
      return TRUE;
    }
  else return FALSE;
}

gboolean
gzochid_lock_range_check_and_set (gzochid_lock_table *lock_table, guint node_id,
				  GBytes *from, GBytes *to,
				  struct timeval *ret_timestamp)
{
  range_lock_search_context search_context;

  search_context.excluded_node_id = node_id;
  search_context.range_lock = NULL;

  /* Search for conflicting range locks. */
  
  itree_search_interval
    (lock_table->range_locks, from, to, find_overlapping_range_lock,
     &search_context);

  if (search_context.range_lock != NULL)

    /* Range locks are exclusive, so if our search finds any results, indicate a
       failure. */
    
    return lock_failure (search_context.range_lock->timestamp, ret_timestamp);

  else 
    {
      gzochid_node_locks *node_locks = NULL;
      gzochid_range_lock *range_lock = NULL;
      GSequenceIter *point_iter = find_locks_by_key (lock_table, from);

      /* Even if there are no conflicting range locks, there may be point locks
	 that are locked for write that fall within the covered interval. */
      
      if (point_iter != NULL)
	while (!g_sequence_iter_is_end (point_iter))
	  {
	    gzochid_locks *locks = g_sequence_get (point_iter);
	    gzochid_lock *lock = locks->locks->data;

	    /* It's only necessary to check the first locker in the lock list;
	       if the key is locked for write, there'll only be a single 
	       locker. */
	    
	    if (lock->for_write)
	      return lock_failure (lock->timestamp, ret_timestamp);
	    
	    point_iter = g_sequence_iter_next (point_iter);
	  }
      
      node_locks = ensure_node_registration (lock_table, node_id);
      range_lock = range_lock_new (from, to, node_id);

      itree_insert (lock_table->range_locks, from, to, range_lock);
      node_locks->range_locks = g_list_prepend
	(node_locks->range_locks, range_lock);

      /* Add the new lock to the proper place in the timestamp sequence. */

      range_lock->timestamp_iter = g_sequence_insert_sorted
	(lock_table->range_locks_by_timestamp, range_lock,
	 compare_timestamp_data, NULL);
      
      return TRUE;
    }
}

/* Releases the specified point lock, removing it from the specified lock table.
   Both read and write locks are released completely by this function. */

static void
remove_lock (gzochid_lock_table *lock_table, gzochid_lock *lock)
{
  GSequenceIter *iter = find_locks_by_key (lock_table, lock->key);
  gzochid_node_locks *node_locks = g_hash_table_lookup
    (lock_table->nodes_to_locks, &lock->node_id);

  gzochid_locks *locks = NULL;
  GList *lock_link = NULL;

  if (iter == NULL)
    return;

  locks = g_sequence_get (iter);  
  lock_link = g_list_find (locks->locks, lock);  

  /* Remove the lock from the lock list for its key in the sequence. */

  locks->locks = g_list_delete_link (locks->locks, lock_link);

  if (locks->locks == NULL)

    /* If this causes the lock list to go empty, remove the lock list from the
       sequence. */
    
    g_sequence_remove (iter);

  /* Remove the lock from the timestamp-ordered lock sequence. */

  g_sequence_remove (lock->timestamp_iter);

  /* Remove the lock from the node's lock list. */
  
  lock_link = g_list_find (node_locks->locks, lock);
  node_locks->locks = g_list_delete_link (node_locks->locks, lock_link);
  
  if (node_locks->locks == NULL && node_locks->range_locks == NULL)
    {
      /* If this causes the node's lock list to go empty, remove the node's
	 entry from the node-to-lock mapping table. */
      
      free (node_locks);
      g_hash_table_remove (lock_table->nodes_to_locks, &lock->node_id);
    }
  
  lock_free (lock);
}

void
gzochid_lock_release (gzochid_lock_table *lock_table, guint node_id,
		      GBytes *key)
{
  GSequenceIter *iter = find_locks_by_key (lock_table, key);  
  
  if (iter != NULL)
    {
      gzochid_locks *locks = g_sequence_get (iter);
      GList *lock_ptr = g_list_find_custom
	(locks->locks, &node_id, find_lock_with_node_id);

      if (lock_ptr != NULL)
	remove_lock (lock_table, lock_ptr->data);
    }
}

/* Encapsulates contextual data used during the interval tree search for the 
   range lock with a matching specified owner node id, lower and upper interval
   bound. */

struct _range_lock_match_context
{
  guint node_id; /* The node id to match. */

  gpointer from; /* The lower bound to match. */
  gpointer to; /* The upper bound to match. */
  
  gzochid_range_lock *range_lock; /* Pointer to store the candidate result. */
};

typedef struct _range_lock_match_context range_lock_match_context;

/* 
   An `itree_search_func' implementation to locate range locks with a specified
   owner. Aborts the traversal on the first overlapping range lock owned by the
   node. 

   Use with `range_lock_match_context'. 
*/

static gboolean
find_range_lock_with_owner (gpointer from, gpointer to, gpointer data,
			    gpointer user_data)
{
  gzochid_range_lock *lock = data;
  range_lock_match_context *match_context = user_data;  
  
  if (bytes_compare_null_first (match_context->from, from) == 0
      && bytes_compare_null_last (match_context->to, to) == 0
      && lock->node_id == match_context->node_id)
    {
      match_context->range_lock = lock;
      return TRUE;
    }
  else return FALSE;
}

/* Releases the specified range lock, removing it from the specified lock 
   table. */

static void
remove_range_lock (gzochid_lock_table *lock_table,
		   gzochid_range_lock *range_lock)
{
  gzochid_node_locks *node_locks = g_hash_table_lookup
    (lock_table->nodes_to_locks, &range_lock->node_id);

  GList *lock_link = g_list_find (node_locks->range_locks, range_lock);

  /* Remove the range lock from the interval tree. */
  
  itree_remove (lock_table->range_locks, range_lock->from, range_lock->to);

  /* Remove the range lock from the timestamp-ordered range lock sequence. */

  g_sequence_remove (range_lock->timestamp_iter);
  
  /* Remove the range lock from the node's lock list. */
  
  node_locks->range_locks = g_list_delete_link
    (node_locks->range_locks, lock_link);

  if (node_locks->locks == NULL && node_locks->range_locks == NULL)
    {
      /* If this causes the node's lock list to go empty, remove the node's
	 entry from the node-to-lock mapping table. */
      
      free (node_locks);
      g_hash_table_remove (lock_table->nodes_to_locks, &range_lock->node_id);
    }
  
  range_lock_free (range_lock);  
}

void
gzochid_lock_release_range (gzochid_lock_table *lock_table, guint node_id,
			    GBytes *from, GBytes *to)
{
  range_lock_match_context match_context;

  match_context.node_id = node_id;
  match_context.from = from;
  match_context.to = to;
  match_context.range_lock = NULL;
  
  itree_search
    (lock_table->range_locks, from, find_range_lock_with_owner, &match_context);

  if (match_context.range_lock != NULL)  
    remove_range_lock (lock_table, match_context.range_lock);
}

/* Adapts `remove_lock' as a `GFunc' for use by `g_list_foreach' in 
   `gzochid_lock_release_all'. */

static void
release_lock (gpointer data, gpointer user_data)
{
  gzochid_lock *lock = data;
  gzochid_lock_table *lock_table = user_data;

  remove_lock (lock_table, lock);
}

/* Adapts `remove_range_lock' as a `GFunc' for use by `g_list_foreach' in 
   `gzochid_lock_release_all'. */

static void
release_range_lock (gpointer data, gpointer user_data)
{
  gzochid_range_lock *range_lock = data;
  gzochid_lock_table *lock_table = user_data;

  remove_range_lock (lock_table, range_lock);
}

void
gzochid_lock_release_all (gzochid_lock_table *lock_table, guint node_id)
{
  gzochid_node_locks *node_locks = g_hash_table_lookup
    (lock_table->nodes_to_locks, &node_id);

  if (node_locks == NULL)
    return;

  g_list_foreach (node_locks->locks, release_lock, lock_table);

  if (g_hash_table_contains (lock_table->nodes_to_locks, &node_id))  
    g_list_foreach (node_locks->range_locks, release_range_lock, lock_table);
}
