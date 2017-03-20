/* dataserver.c: Data server for gzochi-metad
 * Copyright (C) 2017 Julian Graham
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
#include <string.h>

#include "config.h"
#include "dataserver-protocol.h"
#include "dataserver.h"
#include "gzochid-storage.h"
#include "httpd.h"
#include "lock.h"
#include "log.h"
#include "oids-storage.h"
#include "oids.h"
#include "resolver.h"
#include "socket.h"
#include "storage-mem.h"
#include "storage.h"
#include "util.h"

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif /* G_LOG_DOMAIN */
#define G_LOG_DOMAIN "gzochi-metad.dataserver"

#ifndef GZOCHID_STORAGE_ENGINE_DIR
#define GZOCHID_STORAGE_ENGINE_DIR "./storage"
#endif /* GZOCHID_STORAGE_ENGINE_DIR */

#define LOCK_ACCESS(for_write) (for_write ? "r/w" : "read")

/* A store with an associated lock table. */

struct _gzochi_metad_dataserver_lockable_store
{
  gzochid_storage_store *store; /* The persistent store. */
  gzochid_lock_table *locks; /* The lock table. */
};

typedef struct _gzochi_metad_dataserver_lockable_store
gzochi_metad_dataserver_lockable_store;

/* The dataserver-side representation of the storage for a gzochi game 
   application, somewhat analogous to a `gzochid_application_context' 
   structure. */

struct _gzochi_metad_dataserver_application_store
{
  char *name; /* The name of the application. */
  gzochid_storage_context *storage_context; /* The storage context. */

  gzochi_metad_dataserver_lockable_store *oids; /* The lockable object store. */

  /* The lockable binding store. */
  
  gzochi_metad_dataserver_lockable_store *names;
  gzochid_storage_store *meta; /* The metadata store. */

  /* The oid allocation strategy. */
  
  gzochid_oid_allocation_strategy *oid_strategy; 
};

typedef struct _gzochi_metad_dataserver_application_store
gzochi_metad_dataserver_application_store;

/* Boilerplate setup for the data server object. */

/* The data server object. */

struct _GzochiMetadDataServer
{
  GObject parent_instance; /* The parent struct, for casting. */

  GzochidConfiguration *configuration; /* The global configuration object. */

  /* The data client configuration table; extracted from the global 
     configuration object and cached for convenience. */

  GHashTable *data_configuration;

  /* The global resolution context; used for getting a (temporary) handle to
     the meta server's embedded HTTP server (if available) to extract the base
     URL to send to the client as part of the login response. */
  
  GzochidResolutionContext *resolution_context;
  
  /* The socket server for the data server. */
  
  GzochidSocketServer *socket_server; 

  gzochid_storage_engine *storage_engine; /* The storage engine. */

  /* Mapping application name to `gzochi_metad_dataserver_application_store'. */

  GHashTable *application_stores; 
};

#define STORAGE_INTERFACE(server) server->storage_engine->interface

G_DEFINE_TYPE (GzochiMetadDataServer, gzochi_metad_data_server, G_TYPE_OBJECT);

enum gzochi_metad_data_server_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_RESOLUTION_CONTEXT,
    PROP_SOCKET_SERVER,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochi_metad_data_server_constructed (GObject *object)
{
  GzochiMetadDataServer *data_server = GZOCHI_METAD_DATA_SERVER (object);

  data_server->data_configuration = gzochid_configuration_extract_group
    (data_server->configuration, "data");  
}

static void
gzochi_metad_data_server_set_property (GObject *object, guint property_id,
				       const GValue *value, GParamSpec *pspec)
{
  GzochiMetadDataServer *self = GZOCHI_METAD_DATA_SERVER (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;

    case PROP_RESOLUTION_CONTEXT:
      self->resolution_context = g_object_ref_sink (g_value_get_object (value));
      break;
      
    case PROP_SOCKET_SERVER:
      self->socket_server = g_object_ref (g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

/* A `GHRFunc' implementation that cleans up 
   `gzochi_metad_dataserver_application_store' instances managed by the data 
   server as part of the data server shutdown process. */

static gboolean
close_application_store (gpointer key, gpointer value, gpointer user_data)
{
  GzochiMetadDataServer *server = user_data;
  gzochi_metad_dataserver_application_store *store = value;
  
  STORAGE_INTERFACE (server)->close_store (store->oids->store);
  gzochid_lock_table_free (store->oids->locks);
  free (store->oids);
  
  STORAGE_INTERFACE (server)->close_store (store->names->store);
  gzochid_lock_table_free (store->names->locks);
  free (store->names);

  STORAGE_INTERFACE (server)->close_store (store->meta);

  STORAGE_INTERFACE (server)->close_context (store->storage_context);

  gzochid_oid_allocation_strategy_free (store->oid_strategy);
  
  return TRUE;
}

static void
gzochi_metad_data_server_finalize (GObject *gobject)
{
  GzochiMetadDataServer *server = GZOCHI_METAD_DATA_SERVER (gobject);
  
  g_hash_table_destroy (server->data_configuration);

  g_hash_table_foreach_remove
    (server->application_stores, close_application_store, server);
  g_hash_table_destroy (server->application_stores);

  if (server->storage_engine != NULL)
    {
      if (server->storage_engine->handle == NULL)

	/* A `NULL' handle indicates a "synthetic" storage engine (i.e., the
	   memory engine). */
	
	free (server->storage_engine);
    }

  G_OBJECT_CLASS (gzochi_metad_data_server_parent_class)->finalize (gobject);
}

static void
gzochi_metad_data_server_dispose (GObject *gobject)
{
  GzochiMetadDataServer *server = GZOCHI_METAD_DATA_SERVER (gobject);

  g_object_unref (server->configuration);
  g_object_unref (server->resolution_context);
  g_object_unref (server->socket_server);

  G_OBJECT_CLASS (gzochi_metad_data_server_parent_class)->dispose (gobject);
}

static void
gzochi_metad_data_server_class_init (GzochiMetadDataServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->constructed = gzochi_metad_data_server_constructed;
  object_class->dispose = gzochi_metad_data_server_dispose;
  object_class->finalize = gzochi_metad_data_server_finalize;
  object_class->set_property = gzochi_metad_data_server_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  obj_properties[PROP_RESOLUTION_CONTEXT] = g_param_spec_object
    ("resolution-context", "resolution-context",
     "The global resolution context", GZOCHID_TYPE_RESOLUTION_CONTEXT,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);
  
  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "socket-server", "The global socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_data_server_init (GzochiMetadDataServer *self)
{
  self->application_stores = g_hash_table_new_full
    (g_str_hash, g_str_equal, (GDestroyNotify) free, (GDestroyNotify) free);
}

void
gzochi_metad_dataserver_start (GzochiMetadDataServer *self)
{
  if (g_hash_table_contains (self->data_configuration, "storage.engine"))
    {
      const char *dir = NULL;
      const char *env = getenv ("GZOCHID_STORAGE_ENGINE_DIR");
      
      if (env != NULL)
        dir = env;
      else 
        {
          const char *conf_dir = g_hash_table_lookup
	    (self->data_configuration, "storage.engine.dir");
          dir = conf_dir == NULL ? GZOCHID_STORAGE_ENGINE_DIR : conf_dir;
        }

      self->storage_engine = gzochid_storage_load_engine 
        (dir, g_hash_table_lookup (self->data_configuration, "storage.engine"));
    }
  else g_message 
         ("No durable storage engine configured; memory engine will be used.");
  
  if (self->storage_engine == NULL)
    {
      g_message ("\
Using in-memory storage for application data. THIS CONFIGURATION IS NOT SAFE \
FOR PRODUCTION USE.");

      self->storage_engine = calloc (1, sizeof (gzochid_storage_engine));
      self->storage_engine->interface = &gzochid_storage_engine_interface_mem;
    }
}

/* Switch on the specified store name to return either the oids store or the
   named binding store, or set an error if the name was not "oids" or 
   "names." */

static gzochi_metad_dataserver_lockable_store *
get_lockable_store (gzochi_metad_dataserver_application_store *store,
		    const char *name, GError **err)
{
  if (strcmp (name, "oids") == 0)
    return store->oids;
  else if (strcmp (name, "names") == 0)
    return store->names;
  else
    {
      g_set_error
	(err, GZOCHI_METAD_DATASERVER_ERROR,
	 GZOCHI_METAD_DATASERVER_ERROR_STORE_NAME, "Invalid store name '%s'.",
	 name);

      return NULL;
    }
}

/* 
  Ensures that an application store for the specified application name is open
  and managed by the specified data store, opening / creating one if 
  necessary.
  
  This function returns a pointer to the new or existing application store; 
  this poitner is owned by the data server and should not be freed. 
*/

static gzochi_metad_dataserver_application_store *
ensure_open_application_store (const GzochiMetadDataServer *server,
			       const char *app)
{
  if (g_hash_table_contains (server->application_stores, app))
    return g_hash_table_lookup (server->application_stores, app);
  else
    {
      gzochi_metad_dataserver_application_store *store =
	malloc (sizeof (gzochi_metad_dataserver_application_store));
      gzochid_storage_engine_interface *iface = STORAGE_INTERFACE (server);

      g_message ("Initializing application storage for '%s'.", app);
      
      store->storage_context = iface->initialize ((char *) app);

      store->oids = malloc (sizeof (gzochi_metad_dataserver_lockable_store));
      store->oids->store = iface->open
	(store->storage_context, "oids", GZOCHID_STORAGE_CREATE);
      store->oids->locks = gzochid_lock_table_new ("oids"); 

      store->names = malloc (sizeof (gzochi_metad_dataserver_lockable_store));
      store->names->store = iface->open
	(store->storage_context, "names", GZOCHID_STORAGE_CREATE);
      store->names->locks = gzochid_lock_table_new ("names"); 

      store->meta = iface->open
	(store->storage_context, "meta", GZOCHID_STORAGE_CREATE);

      store->oid_strategy = gzochid_storage_oid_strategy_new
	(iface, store->storage_context, store->meta);
      
      g_hash_table_insert (server->application_stores, strdup (app), store);
      return store;
    }
}

gzochid_data_reserve_oids_response *
gzochi_metad_dataserver_reserve_oids (GzochiMetadDataServer *server,
				      guint node_id, const char *app)
{
  gzochid_data_oids_block oids_block;
  gzochid_data_reserve_oids_response *response = NULL;
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);

  gzochid_trace ("Node %d requested oid block for %s.", node_id, app);
  
  assert (gzochid_oids_reserve_block
	  (app_store->oid_strategy, &oids_block, NULL));

  gzochid_trace ("Reserved block { %" G_GUINT64_FORMAT ", %d } for node %d/%s.",
		 oids_block.block_start, oids_block.block_size, node_id, app);
  
  response = gzochid_data_reserve_oids_response_new (app, &oids_block);
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_value (GzochiMetadDataServer *server,
				       guint node_id, const char *app,
				       const char *store_name, GBytes *key,
				       gboolean for_write, GError **err)
{
  gzochid_data_response *response = NULL;
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);

  GError *local_err = NULL;
  gzochi_metad_dataserver_lockable_store *store = get_lockable_store
    (app_store, store_name, &local_err);
  
  struct timeval most_recent_lock;

  if (store == NULL)
    {
      g_propagate_error (err, local_err);
      return NULL;
    }

  if (gzochid_log_level_visible (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
    GZOCHID_WITH_FORMATTED_BYTES
      (key, buf, 33, gzochid_trace
       ("Node %d requested %s lock on key %s/%s/%s.", node_id,
	LOCK_ACCESS (for_write), app, store_name, buf));
  
  if (gzochid_lock_check_and_set
      (store->locks, node_id, key, for_write, &most_recent_lock))
    {
      size_t data_len = 0;
      gzochid_storage_transaction *transaction = STORAGE_INTERFACE (server)
	->transaction_begin (app_store->storage_context);
      char *data = STORAGE_INTERFACE (server)->transaction_get
	(transaction, store->store, (char *) g_bytes_get_data (key, NULL),
	 g_bytes_get_size (key), &data_len);

      if (data != NULL)
	{
	  GBytes *data_bytes = g_bytes_new_with_free_func
	    (data, data_len, (GDestroyNotify) free, data);
	  response = gzochid_data_response_new
	    (app, store_name, TRUE, data_bytes);

	  /* Turn ownership of the data over to the response object. */

	  g_bytes_unref (data_bytes);
	}
      else response = gzochid_data_response_new (app, store_name, TRUE, NULL);

      STORAGE_INTERFACE (server)->transaction_rollback (transaction);

      if (gzochid_log_level_visible (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
	GZOCHID_WITH_FORMATTED_BYTES
	  (key, buf, 33, gzochid_trace
	   ("Granted %s lock on key %s/%s/%s to node %d.",
	    LOCK_ACCESS (for_write), app, store_name, buf, node_id));
      
      return response;
    }
  else
    {
      if (gzochid_log_level_visible (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG))
	GZOCHID_WITH_FORMATTED_BYTES
	  (key, buf, 33, g_debug ("Denied %s lock on key %s/%s/%s to node %d.",
				  LOCK_ACCESS (for_write), app, store_name, buf,
				  node_id));
      
      return gzochid_data_response_new (app, store_name, FALSE, NULL);
    }
}

gzochid_data_response *
gzochi_metad_dataserver_request_next_key (GzochiMetadDataServer *server,
					  guint node_id, const char *app,
					  const char *store_name, GBytes *key,
					  GError **err)
{
  gzochid_data_response *response = NULL;
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);
  GBytes *to_key = NULL;
  
  GError *local_err = NULL;
  gzochi_metad_dataserver_lockable_store *store = get_lockable_store
    (app_store, store_name, &local_err);

  struct timeval most_recent_lock;

  size_t data_len = 0;
  gzochid_storage_transaction *transaction = NULL;
  char *data = NULL;

  if (store == NULL)
    {
      g_propagate_error (err, local_err);
      return NULL;
    }
  
  transaction = STORAGE_INTERFACE (server)->transaction_begin
    (app_store->storage_context);
  
  if (key == NULL)
    {
      gzochid_trace ("Node %d requested range lock on %s/%s keyspace.", node_id,
		     app, store_name);
      data = STORAGE_INTERFACE (server)->transaction_first_key
	(transaction, store->store, &data_len);
    }
  else
    {
      if (gzochid_log_level_visible (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
	GZOCHID_WITH_FORMATTED_BYTES
	  (key, buf, 33, gzochid_trace
	   ("Node %d requested range lock from key %s/%s/%s.", node_id, app,
	    store_name, buf));
	   
      data = STORAGE_INTERFACE (server)->transaction_next_key
	(transaction, store->store, (char *) g_bytes_get_data (key, NULL),
	 g_bytes_get_size (key), &data_len);
    }
  
  assert (!transaction->rollback);
  
  if (data != NULL)
    to_key = g_bytes_new_with_free_func
      (data, data_len, (GDestroyNotify) free, data);

  STORAGE_INTERFACE (server)->transaction_rollback (transaction);

  if (!gzochid_lock_range_check_and_set
      (store->locks, node_id, key, to_key, &most_recent_lock))
    {
      if (key == NULL)
	g_debug ("Denied range lock on %s/%s keyspace to node %d.", app,
		 store_name, node_id);
      else if (gzochid_log_level_visible (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG))
	GZOCHID_WITH_FORMATTED_BYTES
	  (key, buf, 33, g_debug
	   ("Denied range lock starting at %s/%s/%s to node %d.", app,
	    store_name, buf, node_id));
      
      response = gzochid_data_response_new (app, store_name, FALSE, NULL);
    }
  else
    {
      if (key == NULL)
	gzochid_trace ("Granted range lock on %s/%s keyspace to node %d.", app,
		       store_name, node_id);
      else if (gzochid_log_level_visible
	       (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
	GZOCHID_WITH_FORMATTED_BYTES
	  (key, buf, 33, gzochid_trace
	   ("Granted range lock starting at %s/%s/%s to node %d.", app,
	    store_name, buf, node_id));
      
      response = gzochid_data_response_new (app, store_name, TRUE, to_key);
    }

  if (to_key != NULL)
    g_bytes_unref (to_key);
  
  return response;
}

void
gzochi_metad_dataserver_process_changeset (GzochiMetadDataServer *server,
					   guint node_id,
					   gzochid_data_changeset *changeset,
					   GError **err)
{
  gint i = 0;
  gboolean needs_rollback = FALSE;
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, changeset->app);
  gzochid_storage_transaction *transaction =
    STORAGE_INTERFACE (server)->transaction_begin (app_store->storage_context);

  gzochid_trace ("Processing %d changes from %d/%s.", changeset->changes->len,
		 node_id, changeset->app);
  
  for (; i < changeset->changes->len; i++)
    {
      gzochid_data_change change = g_array_index
	(changeset->changes, gzochid_data_change, i);
      GError *local_err = NULL;
      gzochi_metad_dataserver_lockable_store *store = get_lockable_store
	(app_store, change.store, &local_err);

      if (store == NULL)
	{
	  g_propagate_error (err, local_err);
	  needs_rollback = TRUE;
	  break;
	}

      if (!gzochid_lock_check (store->locks, node_id, change.key, TRUE))
	{
	  g_set_error
	    (err, GZOCHI_METAD_DATASERVER_ERROR,
	     GZOCHI_METAD_DATASERVER_ERROR_LOCK_CONFLICT,
	     "Attempted to commit change to oid '%s' without write lock.",
	     (char *) g_bytes_get_data (change.key, NULL));

	  needs_rollback = TRUE;
	  break;
	}

	if (change.delete)
	  STORAGE_INTERFACE (server)->transaction_delete
	    (transaction, store->store,
	     (char *) g_bytes_get_data (change.key, NULL),
	     g_bytes_get_size (change.key));

	else STORAGE_INTERFACE (server)->transaction_put
	       (transaction, store->store,
		(char *) g_bytes_get_data (change.key, NULL),
		g_bytes_get_size (change.key),
		(char *) g_bytes_get_data (change.data, NULL),
		g_bytes_get_size (change.data));

	if (transaction->rollback)
	  {
	    g_set_error
	      (err, GZOCHI_METAD_DATASERVER_ERROR,
	       GZOCHI_METAD_DATASERVER_ERROR_LOCK_CONFLICT,
	       "Transaction failure writing oid '%s'.",
	       (char *) g_bytes_get_data (change.key, NULL));

	    needs_rollback = TRUE;
	    break;
	  }
    }
    
  if (needs_rollback)
    {
      g_debug ("Changes from %d/%s rolled back during processing.", node_id,
	       changeset->app);
      STORAGE_INTERFACE (server)->transaction_rollback (transaction);
    }
  else
    {
      STORAGE_INTERFACE (server)->transaction_prepare (transaction);

      if (transaction->rollback)
	{
	  g_set_error
	    (err, GZOCHI_METAD_DATASERVER_ERROR,
	     GZOCHI_METAD_DATASERVER_ERROR_FAILED,
	     "Transaction failed to prepare.");
	  g_debug ("Changes from %d/%s rolled back during prepare.", node_id,
		   changeset->app);
	  STORAGE_INTERFACE (server)->transaction_rollback (transaction);
	}
      else
	{
	  gzochid_trace ("Committed changes from %d/%s.", node_id,
			 changeset->app);
	  STORAGE_INTERFACE (server)->transaction_commit (transaction);
	}
    }
}

void
gzochi_metad_dataserver_release_key (GzochiMetadDataServer *server,
				     guint node_id, const char *app,
				     const char *store_name, GBytes *key)
{
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);

  GError *local_err = NULL;
  gzochi_metad_dataserver_lockable_store *store = get_lockable_store
    (app_store, store_name, &local_err);

  if (local_err != NULL)
    {
      g_warning
	("Failed to release key for application '%s': %s", app,
	 local_err->message);
      g_error_free (local_err);
      return;
    }

  if (gzochid_log_level_visible (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
    GZOCHID_WITH_FORMATTED_BYTES
      (key, buf, 33, gzochid_trace ("Node id %d releasing key %s/%s/%s.",
				    node_id, app, store_name, buf));
  
  gzochid_lock_release (store->locks, node_id, key);
}

void
gzochi_metad_dataserver_release_range (GzochiMetadDataServer *server,
				       guint node_id, const char *app,
				       const char *store_name,
				       GBytes *first_key, GBytes *last_key)
{
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);
  
  GError *local_err = NULL;
  gzochi_metad_dataserver_lockable_store *store = get_lockable_store
    (app_store, store_name, &local_err);

  if (local_err != NULL)
    {
      g_warning
	("Failed to release key range for application '%s': %s", app,
	 local_err->message);
      g_error_free (local_err);
      return;
    }

  if (first_key == NULL)
    gzochid_trace ("Node %d releasing range lock on %s/%s keyspace.", node_id,
		   app, store_name);
  else if (gzochid_log_level_visible (G_LOG_DOMAIN, GZOCHID_LOG_LEVEL_TRACE))
    GZOCHID_WITH_FORMATTED_BYTES
      (first_key, buf, 33, gzochid_trace 
       ("Node id %d releasing range lock from %s/%s/%s.", node_id, app,
	store_name, buf));
  
  gzochid_lock_release_range (store->locks, node_id, first_key, last_key);
}

void
gzochi_metad_dataserver_release_all (GzochiMetadDataServer *server,
				     guint node_id)
{
  GHashTableIter iter;
  gpointer value = NULL;

  g_hash_table_iter_init (&iter, server->application_stores);

  while (g_hash_table_iter_next (&iter, NULL, &value))
    {
      gzochi_metad_dataserver_application_store *store = value;

      gzochid_lock_release_all (store->oids->locks, node_id);
      gzochid_lock_release_all (store->names->locks, node_id);
    }
}

GQuark
gzochi_metad_dataserver_error_quark ()
{
  return g_quark_from_static_string ("gzochi-metad-dataserver-error-quark");
}
