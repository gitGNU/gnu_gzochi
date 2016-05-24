/* itree.c: Interval tree implementation for gzochid
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

#include "itree.h"

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

struct _gzochid_itree
{
  itree_node *root; /* The root node. */
  GCompareFunc lower_comparator; /* The interval bound comparison function. */
  GCompareFunc upper_comparator; /* The interval bound comparison function. */
};

typedef struct _gzochid_itree gzochid_itree;

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

gzochid_itree *
gzochid_itree_new (GCompareFunc lower_comparator, GCompareFunc upper_comparator)
{
  gzochid_itree *t = malloc (sizeof (gzochid_itree));

  t->root = NULL;
  t->lower_comparator = lower_comparator;
  t->upper_comparator = upper_comparator;
  
  return t;
}

void
gzochid_itree_free (gzochid_itree *itree)
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
	    src = g_list_prepend (src, node->left);

	  if (node->right != NULL)
	    src = g_list_prepend (src, node->right);
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
itree_location (gzochid_itree *itree, gpointer lower, gpointer upper)
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
update_max_upper (gzochid_itree *itree, itree_node *node)
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
itree_rotate (gzochid_itree *itree, itree_node *node, gboolean left)
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
itree_retrace_insert (gzochid_itree *itree, itree_node *node)
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

void
gzochid_itree_insert (gzochid_itree *itree, gpointer lower, gpointer upper,
		      gpointer data)
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
itree_retrace_remove (gzochid_itree *itree, itree_node *node)
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

void
gzochid_itree_remove (gzochid_itree *itree, gpointer lower, gpointer upper)
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

void
gzochid_itree_search_interval (gzochid_itree *itree, gpointer from, gpointer to,
			       gzochid_itree_search_func search_func,
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
      
      if (itree->upper_comparator (from, node->max_upper) > 0
	  && itree->upper_comparator (to, node->max_upper) > 0)

	/* If the interval is entirely outside of the max upper bound in this
	   sub-tree, there's no point in expanding it any further. */
	
	continue;

      if (node->right != NULL)
	stack = g_list_prepend (stack, node->right);
      if (node->left != NULL)
	stack = g_list_prepend (stack, node->left);
    }
}

void
gzochid_itree_search (gzochid_itree *itree, gpointer point,
		      gzochid_itree_search_func search_func, gpointer user_data)
{
  GList *stack = NULL;

  if (itree->root == NULL)
    return;
  
  stack = g_list_prepend (NULL, itree->root);

  while (stack != NULL)
    {
      itree_node *node = stack->data;

      stack = g_list_delete_link (stack, stack);

      if (itree->lower_comparator (point, node->lower) >= 0
	  && itree->upper_comparator (point, node->upper) <= 0)
	if (search_func (node->lower, node->upper, node->data, user_data))
	  {
	    g_list_free (stack);
	    return;
	  }
      
      if (itree->upper_comparator (point, node->max_upper) > 0)

	/* If the point falls beyond the max upper bound in this sub-tree, 
	   there's no point in expanding it any further. */
		
	continue;

      if (node->right != NULL)
	stack = g_list_prepend (stack, node->right);
      if (node->left != NULL)
	stack = g_list_prepend (stack, node->left);
    }
}
