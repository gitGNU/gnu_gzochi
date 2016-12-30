/* nodemap-mem.c: In-memory nodemap implementation for gzochi-metad
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
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "nodemap.h"
#include "nodemap-mem.h"

/*
  The functions and data structures defined below form an implementation of
  `gzochi_metad_nodemap' in which the session-to-server mappings are held in
  memory. As such, it may be used in gzochi server cluster configurations in 
  which there is only a single meta server.
  
  This implementation is not thread-safe; only a single thread should interact
  with the map at a time.
*/

/* Qualifies a session id with the name of the application to which it 
   belongs. */

struct _nodemap_qualified_key
{
  char *app; /* The application name. */
  guint64 session_id; /* The session id. */
};

typedef struct _nodemap_qualified_key nodemap_qualified_key;

/* Create and return a new qualified key with the specified application name and
   session id. The memory used by this object should be freed via 
   `nodemap_qualified_key_free' when no longer in use. */

static nodemap_qualified_key *
nodemap_qualified_key_new (const char *app, guint64 session_id)
{
  nodemap_qualified_key *key = malloc (sizeof (nodemap_qualified_key));

  key->app = strdup (app);
  key->session_id = session_id;
  
  return key;
}

/* Frees the specified qualified key. */

static void
nodemap_qualified_key_free (nodemap_qualified_key *qualified_key)
{
  free (qualified_key->app);
  free (qualified_key);
}

/* A `GHashFunc' implementation for `nodemap_qualified_key'. */

static guint
nodemap_qualified_key_hash (gconstpointer v)
{
  const nodemap_qualified_key *key = v;
  return g_str_hash (key->app) * 31 + g_int64_hash (&key->session_id);
}

/* A `GEqualFunc' implementation for `nodemap_qualified_key'. */

static gboolean
nodemap_qualified_key_equal (gconstpointer v1, gconstpointer v2)
{
  const nodemap_qualified_key *key1 = v1;
  const nodemap_qualified_key *key2 = v2;

  return strcmp (key1->app, key2->app) == 0 &&
    key1->session_id == key2->session_id;
}

/* Memory-backed nodemap struct definition. */

struct _gzochi_metad_nodemap_mem
{
  gzochi_metad_nodemap base; /* The base nodemap type. */

  /* Map of `nodemap_qualified_key *' -> `int *'. */

  GHashTable *session_to_node;

  /* Map of `int *' to a `GSequence' of `nodemap_qualified_key *'. */
  
  GHashTable *node_to_sessions; 
};

typedef struct _gzochi_metad_nodemap_mem gzochi_metad_nodemap_mem;

/* Returns a heap-allocated pointer to a four-byte buffer containing the 
   specified int value. The buffer should be freed via `free' when no longer in
   use. */

static inline int *
copy_int (const int v)
{
  int *int_copy = malloc (sizeof (int));
  *int_copy = v;
  return int_copy;
}

/* A `GCompareDataFunc' implementation for `nodemap_qualified_key'. */

static gint
nodemap_qualified_key_compare_data_func (gconstpointer a, gconstpointer b,
					 gpointer user_data)
{
  const nodemap_qualified_key *key_a = (const nodemap_qualified_key *) a;
  const nodemap_qualified_key *key_b = (const nodemap_qualified_key *) b;

  int app_compare = strcmp (key_a->app, key_b->app);
  
  return app_compare == 0 ? key_a->session_id - key_b->session_id : app_compare;
}

/* The `map_session' implementation for the `gzochi_metad_nodemap_mem' 
   functional interface. */

static void
map_session (gzochi_metad_nodemap *nodemap, char *app, guint64 session_id,
	     int node_id, GError **err)
{
  nodemap_qualified_key qualified_key = { app, session_id };
  gzochi_metad_nodemap_mem *nodemap_mem = (gzochi_metad_nodemap_mem *) nodemap;
  
  if (g_hash_table_contains (nodemap_mem->session_to_node, &qualified_key))

    g_set_error
      (err, GZOCHI_METAD_NODEMAP_ERROR,
       GZOCHI_METAD_NODEMAP_ERROR_ALREADY_MAPPED,
       "Mapping for session already exists.");
  else
    {
      GSequence *sessions = NULL;

      /* Is there a session sequence already mapped to the target node id? */
      
      if (g_hash_table_contains (nodemap_mem->node_to_sessions, &node_id))
	sessions = g_hash_table_lookup
	  (nodemap_mem->node_to_sessions, &node_id);
      else
	{
	  sessions = g_sequence_new
	    ((GDestroyNotify) nodemap_qualified_key_free);
	  g_hash_table_insert
	    (nodemap_mem->node_to_sessions, copy_int (node_id), sessions);
	}
      
      g_sequence_insert_sorted
	(sessions, nodemap_qualified_key_new (app, session_id),
	 nodemap_qualified_key_compare_data_func, NULL);
      
      g_hash_table_insert
	(nodemap_mem->session_to_node,
	 nodemap_qualified_key_new (app, session_id), copy_int (node_id));
    }
}

/* The `unmap_session' implementation for the `gzochi_metad_nodemap_mem' 
   functional interface. */

static void
unmap_session (gzochi_metad_nodemap *nodemap, char *app, guint64 session_id,
	       GError **err)
{
  nodemap_qualified_key qualified_key = { app, session_id };
  gzochi_metad_nodemap_mem *nodemap_mem = (gzochi_metad_nodemap_mem *) nodemap;

  if (g_hash_table_contains (nodemap_mem->session_to_node, &qualified_key))
    {
      int *node_id = g_hash_table_lookup
	(nodemap_mem->session_to_node, &qualified_key);
      GSequence *sessions = g_hash_table_lookup
	(nodemap_mem->node_to_sessions, node_id);
      
      g_sequence_remove
	(g_sequence_lookup (sessions, &qualified_key,
			    nodemap_qualified_key_compare_data_func, NULL));

      /* If the session sequence is now empty, remove it from the map. */
      
      if (g_sequence_get_begin_iter (sessions) ==
	  g_sequence_get_end_iter (sessions))
	g_hash_table_remove (nodemap_mem->node_to_sessions, node_id);
	
      g_hash_table_remove (nodemap_mem->session_to_node, &qualified_key);
    }
  else g_set_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		    GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED,
		    "No mapping for session exists.");
}

/* The `lookup_session' implementation for the `gzochi_metad_nodemap_mem' 
   functional interface. */

static int
lookup_session (gzochi_metad_nodemap *nodemap, char *app, guint64 session_id,
		GError **err)
{
  int ret = 0;
  nodemap_qualified_key qualified_key = { app, session_id };
  gzochi_metad_nodemap_mem *nodemap_mem = (gzochi_metad_nodemap_mem *) nodemap;

  if (g_hash_table_contains (nodemap_mem->session_to_node, &qualified_key))
    {
      int *ret_ptr = g_hash_table_lookup
	(nodemap_mem->session_to_node, &qualified_key);
      ret = *ret_ptr;
    }
  else g_set_error (err, GZOCHI_METAD_NODEMAP_ERROR,
		    GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED,
		    "No mapping for session exists.");
  
  return ret;
}

/* A `GFunc' implementation to remove a session-to-node mapping when a session 
   is removed from a node's session sequence. */

static void
unmap_session_func (gpointer data, gpointer user_data)
{
  gzochi_metad_nodemap_mem *nodemap_mem = user_data;
  g_hash_table_remove (nodemap_mem->session_to_node, data);
}

/* The `unmap_all' implementation for the `gzochi_metad_nodemap_mem' 
   functional interface. */

void
unmap_all (gzochi_metad_nodemap *nodemap, int node_id)
{
  gzochi_metad_nodemap_mem *nodemap_mem = (gzochi_metad_nodemap_mem *) nodemap;
  
  if (g_hash_table_contains (nodemap_mem->node_to_sessions, &node_id))
    {
      GSequence *sessions = g_hash_table_lookup
	(nodemap_mem->node_to_sessions, &node_id);
      g_sequence_foreach (sessions, unmap_session_func, nodemap);      
      g_hash_table_remove (nodemap_mem->node_to_sessions, &node_id);
    }
}

static gzochi_metad_nodemap_iface mem_iface =
  { map_session, unmap_session, lookup_session, unmap_all };

gzochi_metad_nodemap *
gzochi_metad_nodemap_mem_new ()
{
  gzochi_metad_nodemap_mem *nodemap =
    malloc (sizeof (gzochi_metad_nodemap_mem));

  nodemap->base.iface = &mem_iface;
  
  nodemap->session_to_node = g_hash_table_new_full
    (nodemap_qualified_key_hash, nodemap_qualified_key_equal,
     (GDestroyNotify) nodemap_qualified_key_free, free);
  nodemap->node_to_sessions = g_hash_table_new_full
    (g_int_hash, g_int_equal, free, (GDestroyNotify) g_sequence_free);

  return (gzochi_metad_nodemap *) nodemap;
}

void
gzochi_metad_nodemap_mem_free (gzochi_metad_nodemap *nodemap)
{
  gzochi_metad_nodemap_mem *nodemap_mem = (gzochi_metad_nodemap_mem *) nodemap;
  
  g_hash_table_destroy (nodemap_mem->session_to_node);
  g_hash_table_destroy (nodemap_mem->node_to_sessions);

  free (nodemap);
}

GQuark
gzochi_metad_nodemap_error_quark ()
{
  return g_quark_from_static_string ("gzochi-metad-nodemap-error-quark");
}

