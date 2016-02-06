/* resolver.c: Minimal dependency injection framework for gzochid components
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
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>

#include "resolver.h"

GQuark
gzochid_resolution_error_quark (void)
{
  return g_quark_from_static_string ("gzochid-resolution-error-quark");
}

struct _GzochidResolutionContext
{
  GInitiallyUnowned parent_instance;

  gboolean disposing;
  GHashTable *instances; /* The instance cache. */
};

G_DEFINE_TYPE (GzochidResolutionContext, gzochid_resolution_context,
	       G_TYPE_INITIALLY_UNOWNED);

static void
unref_value (gpointer key, gpointer value, gpointer userdata)
{
  g_object_unref (value);
}

void
gzochid_resolution_context_dispose (GObject *object)
{
  GzochidResolutionContext *self = GZOCHID_RESOLUTION_CONTEXT (object);

  if (!self->disposing)
    {
      GType context_type = GZOCHID_TYPE_RESOLUTION_CONTEXT;
      
      self->disposing = TRUE;
      g_hash_table_remove (self->instances, &context_type);
      g_hash_table_foreach (self->instances, unref_value, NULL);
    }
}

static void
gzochid_resolution_context_finalize (GObject *object)
{
  g_hash_table_destroy (GZOCHID_RESOLUTION_CONTEXT (object)->instances);
}

static void
gzochid_resolution_context_class_init (GzochidResolutionContextClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  /*  object_class->dispose = gzochid_resolution_context_dispose; */
  object_class->finalize = gzochid_resolution_context_finalize;
}

static void
gzochid_resolution_context_init (GzochidResolutionContext *self)
{
  GType *resolution_context_type = malloc (sizeof (GType));

  *resolution_context_type = GZOCHID_TYPE_RESOLUTION_CONTEXT;

  self->disposing = FALSE;
  self->instances = g_hash_table_new_full (g_int_hash, g_int_equal, free, NULL);

  /* Add the resolution context to its own instance cache. */
  
  g_hash_table_insert (self->instances, resolution_context_type, self);
}

/* Represents a maximally-injectable constructor for a GObject-derived type: The
   set of writable, construct-time GObject-derived parameters with everything
   other parameter stripped out. */

struct _constructor
{
  GObjectClass *klass;

  guint n_parameters;
  GParamSpec *parameters;
};

typedef struct _constructor constructor;

static constructor *
injectable_constructor_for_type (GType type, GError **err)
{
  guint i = 0;
  guint n_properties = 0;
  guint n_injectable_properties = 0;
  GObjectClass *klass = g_type_class_ref (type);
  GParamSpec **properties = g_object_class_list_properties
    (klass, &n_properties);

  constructor *ctor = NULL;

  /* Make one pass through the list to identify the eligible parameters and make
     sure that there are no uninjectable parameters... */
  
  for (; i < n_properties; i++)
    {
      GParamSpec *pspec = properties[i];

      if (g_type_is_a (pspec->value_type, G_TYPE_OBJECT)
	  && pspec->flags & G_PARAM_CONSTRUCT)
	n_injectable_properties++;
      else if (pspec->flags & G_PARAM_CONSTRUCT_ONLY)
	{
	  g_set_error
	    (err, GZOCHID_RESOLUTION_ERROR,
	     GZOCHID_RESOLUTION_ERROR_INCOMPATIBLE_CONSTRUCTOR,
	     "Type '%s' requires non-injectable constructor argument '%s'.",
	     g_type_name (type), pspec->name);
	     
	  free (properties);
	  g_type_class_unref (klass);
	  
	  return NULL;
	}
    }

  ctor = calloc (1, sizeof (constructor));
  ctor->klass = klass;

  /* ...then make another pass (if necessary) to collate them into a constructor
     invocation that'll get pushed onto the stack. */
  
  if (n_injectable_properties > 0)
    {
      ctor->n_parameters = n_injectable_properties;
      ctor->parameters = malloc (sizeof (GParamSpec) * ctor->n_parameters);

      guint j = 0;

      for (i = 0; i < n_properties; i++)
	{
	  GParamSpec *pspec = properties[i];
	  
	  if (g_type_is_a (pspec->value_type, G_TYPE_OBJECT)
	      && pspec->flags & G_PARAM_CONSTRUCT)
	    ctor->parameters[j++] = *properties[i];
	  else free (properties[i]);
	}
    }
  
  free (properties);
  return ctor;
}

static void
free_constructor (constructor *ctor)
{
  g_type_class_unref (ctor->klass);

  if (ctor->n_parameters > 0)
    free (ctor->parameters);
  
  free (ctor);
}

static gboolean
free_node_constructor (GNode *node, gpointer data)
{
  free_constructor (node->data);
  return FALSE;
}

/* A representation of the state required during the construction of the
   dependency tree. */

struct _tree_state
{
  GNode *node; /* The "current" node in the tree. */

  /* The remaining types that need constructors to be added as children of the 
     current node. */

  GList *types; 
};

typedef struct _tree_state tree_state;

static tree_state *
create_tree_state (GNode *node, GList *types)
{
  tree_state *state = malloc (sizeof (tree_state));

  state->node = node;
  state->types = types;

  return state;
}

static void
free_tree_state (gpointer data)
{
  tree_state *state = data;
  
  g_list_free_full (state->types, free);
  free (state);
}

static gboolean
has_ancestor_with_type (GNode *node, GType type)
{
  while (node != NULL)
    {
      constructor *ctor = node->data;
	
      if (G_OBJECT_CLASS_TYPE (ctor->klass) == type)
	return TRUE;

      node = node->parent;
    }

  return FALSE;
}

static GList *
extract_dependencies (constructor *ctor)
{
  guint i = 0;
  GList *dependencies = NULL;

  for (; i < ctor->n_parameters; i++)
    {
      GType *type = malloc (sizeof (GType));
      *type = ctor->parameters[i].value_type;
      dependencies = g_list_prepend (dependencies, type);
    }
  
  return g_list_reverse (dependencies);
}

static tree_state *
build_dependency_tree_inner (GList *stack, GError **err)
{
  GError *local_err = NULL;
  tree_state *state = NULL;
  GType *type = NULL;
  
  assert (stack != NULL);

  state = stack->data;

  if (state->types == NULL)
    return NULL;

  /* Pop the next sibling type off the list of constructor dependencies. */
  
  type = state->types->data;
  state->types = g_list_delete_link (state->types, state->types);  

  /* Circular dependencies are detected by walking up the tree to make sure that
     a type never appears as its own ancestor reachable on the same "branch" of
     the dependency tree. */
  
  if (has_ancestor_with_type (state->node, *type))
    {
      g_set_error
	(err, GZOCHID_RESOLUTION_ERROR,
	 GZOCHID_RESOLUTION_ERROR_CIRCULAR_DEPENDENCY,
	 "Circular dependency on type '%s'.", g_type_name (*type));

      free (type);
      
      return NULL;
    }
  else
    {
      constructor *ctor = injectable_constructor_for_type (*type, &local_err);

      free (type);
      
      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);
	  return NULL;
	}
      else
	{
	  GNode *new_node = g_node_new (ctor);
	  g_node_insert (state->node, -1, new_node);

	  if (ctor->n_parameters > 0)
	    return create_tree_state
	      (new_node, extract_dependencies (ctor));
	  else return create_tree_state (new_node, NULL);
	}
    }
}

/* Build a dependency tree in a roughly tail-recursive way. The process goes 
   like this: For each type to be instantiated, create a representation of the 
   constructor to be used to instantiate it and attach it to a new node in the 
   dependency tree; if it has any eligible parameters, push that node and a list
   of those parameters onto the stack. Peek at the top of the stack: If the
   element has a non-empty list of types to be constructed, remove the first
   type from that list and start from the beginning; otherwise, pop the
   stack. */

static GNode *
build_dependency_tree (GType root_type, GError **err)
{
  GError *local_err = NULL;
  constructor *ctor = injectable_constructor_for_type (root_type, &local_err);

  GNode *root = NULL;
  tree_state *state = NULL;
  GList *stack = NULL;

  if (local_err != NULL)
    {
      g_propagate_error (err, local_err);
      return NULL;
    }

  root = g_node_new (ctor);

  state = create_tree_state (root, extract_dependencies (ctor));
  stack = g_list_append (NULL, state);
  
  while (stack != NULL)
    {
      GError *local_err = NULL;
      tree_state *old_state = stack->data, *new_state = NULL;

      if (old_state->types == NULL)
	{
	  free_tree_state (old_state);
	  stack = g_list_delete_link (stack, stack);
	  continue;
	}
      
      new_state = build_dependency_tree_inner (stack, &local_err);

      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);

	  /* If there was an error building the tree, cleanup the intermediate
	     storage. */
	  
	  g_node_traverse
	    (root, G_POST_ORDER, G_TRAVERSE_ALL, -1, free_node_constructor,
	     NULL);
	  g_node_destroy (root);
	  
	  g_list_free_full (stack, free_tree_state);

	  return NULL;
	}

      if (new_state != NULL)
	stack = g_list_prepend (stack, new_state);
    }

  return root;
}

static gboolean
construct_instance (GNode *node, gpointer data)
{
  constructor *ctor = node->data;
  GzochidResolutionContext *context = data;
  GType type = G_TYPE_FROM_CLASS (ctor->klass);
  
  if (!g_hash_table_contains (context->instances, &type))
    {
      GObject *inst = NULL;
      GType *type_key = malloc (sizeof (GType));
      
      *type_key = type;

      if (ctor->n_parameters > 0)
	{
	  guint i = 0;
	  GParameter *params = malloc
	    (sizeof (GParameter) * ctor->n_parameters);

	  for (; i < ctor->n_parameters; i++)
	    {
	      GParamSpec pspec = ctor->parameters[i];
	      GType ptype = pspec.value_type;
	      GValue value = G_VALUE_INIT;
	      
	      params[i].name = pspec.name;
	      params[i].value = value;

	      g_value_init (&params[i].value, ptype);
	      
	      assert (g_hash_table_contains (context->instances, &ptype));

	      g_value_take_object
		(&params[i].value,
		 g_object_ref_sink
		 (g_hash_table_lookup (context->instances, &ptype)));
	    }

	  inst = g_object_newv (type, ctor->n_parameters, params);	
	  free (params);
	}
      else inst = g_object_new (type, NULL);

      g_object_force_floating (inst);
      g_hash_table_insert (context->instances, type_key, inst);
    }
  free_constructor (ctor);

  return FALSE;
}

gpointer
gzochid_resolver_require_full (GzochidResolutionContext *context,
			       const GType type, GError **err)
{
  GError *local_err = NULL;
  
  assert (context != NULL);

  if (!g_hash_table_contains (context->instances, &type))
    {
      GNode *dependency_tree = build_dependency_tree (type, &local_err);

      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);
	  return NULL;
	}
      
      g_node_traverse
	(dependency_tree, G_POST_ORDER, G_TRAVERSE_ALL, -1, construct_instance,
	 context);
      g_node_destroy (dependency_tree);
    }

  return g_object_ref_sink (g_hash_table_lookup (context->instances, &type));
}

gpointer
gzochid_resolver_require (GType type, GError **err)
{
  GzochidResolutionContext *context = g_object_new
    (GZOCHID_TYPE_RESOLUTION_CONTEXT, NULL);
  GObject *obj = gzochid_resolver_require_full (context, type, err);

  if (g_object_is_floating (context))
    g_object_unref (context);

  return obj;
}
