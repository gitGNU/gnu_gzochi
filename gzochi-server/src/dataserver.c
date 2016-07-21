/* dataserver.c: Data server for gzochi-metad
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
#include <string.h>

#include "config.h"
#include "dataserver.h"
#include "dataserver-protocol.h"
#include "gzochid-storage.h"
#include "lock.h"
#include "oids.h"
#include "oids-storage.h"
#include "socket.h"
#include "storage.h"
#include "storage-mem.h"

#ifndef GZOCHID_STORAGE_ENGINE_DIR
#define GZOCHID_STORAGE_ENGINE_DIR "./storage"
#endif /* GZOCHID_STORAGE_ENGINE_DIR */

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
  GObject parent_instance;

  GHashTable *configuration; /* The dataserver configuration table. */

  /* The socket server for the data server. */
  
  GzochidSocketServer *socket_server; 

  gzochid_storage_engine *storage_engine; /* The storage engine. */

  /* Mapping application name to `gzochi_metad_dataserver_application_store'. */

  GHashTable *application_stores; 

  gzochid_server_socket *server_socket; /* The dataserver's server socket. */

  /* The port on which the server listens. This may be zero to indicate the 
     server should listen on any available port, and will be set to the port 
     assigned by the operating system. */

  guint port; 
};

#define STORAGE_INTERFACE(server) server->storage_engine->interface

G_DEFINE_TYPE (GzochiMetadDataServer, gzochi_metad_data_server, G_TYPE_OBJECT);

enum gzochi_metad_data_server_properties
  {
    PROP_CONFIGURATION = 1,
    PROP_SOCKET_SERVER,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochi_metad_data_server_set_property (GObject *object, guint property_id,
				       const GValue *value, GParamSpec *pspec)
{
  GzochiMetadDataServer *self = GZOCHI_METAD_DATA_SERVER (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = gzochid_configuration_extract_group
	(GZOCHID_CONFIGURATION (g_value_get_object (value)), "data");

      self->port = gzochid_config_to_int
	(g_hash_table_lookup (self->configuration, "server.port"), 9001);

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
  
  g_hash_table_destroy (server->configuration);

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
}

static void
gzochi_metad_data_server_dispose (GObject *gobject)
{
  GzochiMetadDataServer *server = GZOCHI_METAD_DATA_SERVER (gobject);

  g_object_unref (server->socket_server);
}

static void
gzochi_metad_data_server_class_init (GzochiMetadDataServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochi_metad_data_server_dispose;
  object_class->finalize = gzochi_metad_data_server_finalize;
  object_class->set_property = gzochi_metad_data_server_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "gzochi meta server configuration",
     "Set the meta server configuration", GZOCHID_TYPE_CONFIGURATION,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);
  
  obj_properties[PROP_SOCKET_SERVER] = g_param_spec_object
    ("socket-server", "Socket server", "Set the socket server",
     GZOCHID_TYPE_SOCKET_SERVER,
     G_PARAM_WRITABLE | G_PARAM_CONSTRUCT | G_PARAM_PRIVATE);

  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochi_metad_data_server_init (GzochiMetadDataServer *self)
{
  self->server_socket = gzochid_server_socket_new
    ("Data server", gzochi_metad_dataserver_server_protocol, self);
  self->application_stores = g_hash_table_new_full
    (g_str_hash, g_str_equal, (GDestroyNotify) free, (GDestroyNotify) free);
  self->port = 0;
}

void
gzochi_metad_dataserver_start (GzochiMetadDataServer *self)
{
  if (g_hash_table_contains (self->configuration, "storage.engine"))
    {
      const char *dir = NULL;
      const char *env = getenv ("GZOCHID_STORAGE_ENGINE_DIR");
      
      if (env != NULL)
        dir = env;
      else 
        {
          const char *conf_dir = g_hash_table_lookup
	    (self->configuration, "storage.engine.dir");
          dir = conf_dir == NULL ? GZOCHID_STORAGE_ENGINE_DIR : conf_dir;
        }

      self->storage_engine = gzochid_storage_load_engine 
        (dir, g_hash_table_lookup (self->configuration, "storage.engine"));
    }
  else g_info 
         ("No durable storage engine configured; memory engine will be used.");
  
  if (self->storage_engine == NULL)
    {
      g_info ("\
Using in-memory storage for application data. THIS CONFIGURATION IS NOT SAFE \
FOR PRODUCTION USE.");

      self->storage_engine = calloc (1, sizeof (gzochid_storage_engine));
      self->storage_engine->interface = &gzochid_storage_engine_interface_mem;
    }
  
  gzochid_server_socket_listen
    (self->socket_server, self->server_socket, self->port);
}

/* Switch on the specified store name to return either the oids store or the
   named binding store, or set an error if the name was not "oids" or 
   "names." */

static gzochi_metad_dataserver_lockable_store *
get_lockable_store (gzochi_metad_dataserver_application_store *store,
		    char *name, GError **err)
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
				      guint node_id, char *app)
{
  gzochid_data_oids_block oids_block;
  gzochid_data_reserve_oids_response *response = NULL;
  gzochi_metad_dataserver_application_store *app_store =
    ensure_open_application_store (server, app);

  assert (gzochid_oids_reserve_block
	  (app_store->oid_strategy, &oids_block, NULL));

  response = gzochid_data_reserve_oids_response_new (app, &oids_block);
  return response;
}

gzochid_data_response *
gzochi_metad_dataserver_request_value (GzochiMetadDataServer *server,
				       guint node_id, char *app,
				       char *store_name, GBytes *key,
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

      return response;
    }
  else return gzochid_data_response_new (app, store_name, FALSE, NULL);
}

gzochid_data_response *
gzochi_metad_dataserver_request_next_key (GzochiMetadDataServer *server,
					  guint node_id, char *app,
					  char *store_name, GBytes *key,
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
    data = STORAGE_INTERFACE (server)->transaction_first_key
      (transaction, store->store, &data_len);
  else data = STORAGE_INTERFACE (server)->transaction_next_key
	 (transaction, store->store, (char *) g_bytes_get_data (key, NULL),
	  g_bytes_get_size (key), &data_len);
  
  assert (!transaction->rollback);
  
  if (data != NULL)
    to_key = g_bytes_new_with_free_func
      (data, data_len, (GDestroyNotify) free, data);

  STORAGE_INTERFACE (server)->transaction_rollback (transaction);

  if (!gzochid_lock_range_check_and_set
      (store->locks, node_id, key, to_key, &most_recent_lock))
    response = gzochid_data_response_new (app, store_name, FALSE, NULL);
  else response = gzochid_data_response_new (app, store_name, TRUE, to_key);

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

      if (!gzochid_lock_check_and_set
	  (store->locks, node_id, change.key, TRUE, NULL))
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
    STORAGE_INTERFACE (server)->transaction_rollback (transaction);
  else
    {
      STORAGE_INTERFACE (server)->transaction_prepare (transaction);

      if (transaction->rollback)
	STORAGE_INTERFACE (server)->transaction_rollback (transaction);
      else STORAGE_INTERFACE (server)->transaction_commit (transaction);
    }
}

void
gzochi_metad_dataserver_release_key (GzochiMetadDataServer *server,
				     guint node_id, char *app, char *store_name,
				     GBytes *key)
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
    }
  
  gzochid_lock_release (store->locks, node_id, key);  
}

void
gzochi_metad_dataserver_release_range (GzochiMetadDataServer *server,
				       guint node_id, char *app,
				       char *store_name, GBytes *first_key,
				       GBytes *last_key)
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
    }

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
