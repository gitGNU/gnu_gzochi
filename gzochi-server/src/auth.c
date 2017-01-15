/* auth.c: Authorization management routines for gzochid
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

#include <glib.h>
#include <glib-object.h>
#include <gmodule.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "app.h"
#include "auth_int.h"
#include "config.h"
#include "game.h"
#include "gzochid-auth.h"
#include "util.h"

#define PLUGIN_INFO_FUNCTION "gzochid_auth_init_plugin"

/* The authentication plugin registry encapsulates the module-loading bootstrap
   process for the gzochid authentication system, and provides a plugin lookup
   API for the application bootstrap process. */

struct _GzochidAuthPluginRegistry
{
  GObject parent_instance;

  GzochidConfiguration *configuration; /* The global configuration object. */
  
  GHashTable *auth_plugins; /* Mapping of `char *' to `gzochid_auth_plugin'. */
};

/* Boilerplate setup for the data client object. */

G_DEFINE_TYPE (GzochidAuthPluginRegistry, gzochid_auth_plugin_registry,
	       G_TYPE_OBJECT);

enum gzochid_auth_plugin_registry_properties
  {
    PROP_CONFIGURATION = 1,
    N_PROPERTIES
  };

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL };

static void
gzochid_auth_plugin_registry_set_property (GObject *object, guint property_id,
					   const GValue *value,
					   GParamSpec *pspec)
{
  GzochidAuthPluginRegistry *self = GZOCHID_AUTH_PLUGIN_REGISTRY (object);

  switch (property_id)
    {
    case PROP_CONFIGURATION:
      self->configuration = g_object_ref (g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_auth_plugin_registry_dispose (GObject *object)
{
  GzochidAuthPluginRegistry *registry = GZOCHID_AUTH_PLUGIN_REGISTRY (object);

  g_object_unref (registry->configuration);
}

static void
gzochid_auth_plugin_registry_finalize (GObject *object)
{
  GzochidAuthPluginRegistry *registry = GZOCHID_AUTH_PLUGIN_REGISTRY (object);

  g_hash_table_destroy (registry->auth_plugins);
}

#define GZOCHID_AUTH_IDENTITY_CACHE_DEFAULT_MAX_SIZE 1024
static void
gzochid_auth_plugin_registry_class_init (GzochidAuthPluginRegistryClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gzochid_auth_plugin_registry_dispose;
  object_class->finalize = gzochid_auth_plugin_registry_finalize;
  object_class->set_property = gzochid_auth_plugin_registry_set_property;

  obj_properties[PROP_CONFIGURATION] = g_param_spec_object
    ("configuration", "config", "The global configuration object",
     GZOCHID_TYPE_CONFIGURATION, G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);
  
  g_object_class_install_properties
    (object_class, N_PROPERTIES, obj_properties);
}

static void
gzochid_auth_plugin_registry_init (GzochidAuthPluginRegistry *self)
{
  self->auth_plugins = g_hash_table_new_full
    (g_str_hash, g_str_equal, NULL, (GDestroyNotify) free);
}

struct _gzochid_auth_identity
{
  char *name;
  guint ref_count;

  /* Indicates whether or not this identity is reserved. (I.e., is this the 
     system identity?) */
  
  gboolean reserved; 
};

static gzochid_auth_identity system_identity = { "[SYSTEM]", 1, TRUE };

GQuark
gzochid_auth_plugin_error_quark (void)
{
  return g_quark_from_static_string ("gzochid-auth-plugin-error-quark");
}

gzochid_auth_identity *
gzochid_auth_identity_new (char *name)
{
  gzochid_auth_identity *identity = calloc (1, sizeof (gzochid_auth_identity));

  identity->name = strdup (name);
  identity->ref_count = 1;

  return identity;
}

char *
gzochid_auth_identity_name (gzochid_auth_identity *identity)
{
  return identity->name;
}

void
gzochid_auth_identity_free (gzochid_auth_identity *identity)
{
}

gzochid_auth_identity *
gzochid_auth_function_pass_thru (unsigned char *cred, short cred_len,
				 gpointer auth_data, GError **error)
{
  char *name = NULL;
  gzochid_auth_identity *identity = NULL;
  
  if (cred_len <= 0)
    return NULL;

  name = strndup ((char *) cred, cred_len);
  identity = gzochid_auth_identity_new (name);
  free (name);

  return identity;
}

gzochid_auth_plugin *
gzochid_auth_plugin_registry_lookup (GzochidAuthPluginRegistry *registry,
				     const char *name)
{
  return g_hash_table_lookup (registry->auth_plugins, name);
}

void 
gzochid_auth_identity_serializer 
(gzochid_application_context *context, void *ptr, GByteArray *out, GError **err)
{
  gzochid_auth_identity *identity = ptr;

  g_byte_array_append
    (out, (unsigned char *) identity->name, strlen (identity->name) + 1);
}

void *
gzochid_auth_identity_deserializer
(gzochid_application_context *context, GByteArray *in, GError **err)
{
  char *name = strndup ((char *) in->data, in->len);
  gzochid_auth_identity *identity =
    gzochid_auth_identity_from_name (context->identity_cache, name);

  g_byte_array_remove_range (in, 0, strlen (name) + 1);

  free (name);

  return identity;
}

void
gzochid_auth_identity_finalizer (gzochid_application_context *context,
				 void *ptr)
{
  gzochid_auth_identity_unref (ptr);
}

struct _gzochid_auth_identity_cache
{
  gzochid_lru_cache *cache;
};

static gpointer
identity_new_func (gpointer key, gpointer *key_copy)
{
  *key_copy = strdup (key);
  return gzochid_auth_identity_new (key);
}

gzochid_auth_identity *
gzochid_auth_system_identity ()
{
  return &system_identity;
}

gzochid_auth_identity_cache *
gzochid_auth_identity_cache_new ()
{
  gzochid_auth_identity_cache *cache;

  cache = malloc (sizeof (gzochid_auth_identity_cache));
  cache->cache = gzochid_lru_cache_new_full
    (g_str_hash, g_str_equal, identity_new_func,
     GZOCHID_AUTH_IDENTITY_CACHE_DEFAULT_MAX_SIZE, (GDestroyNotify) free,
     (GDestroyNotify) gzochid_auth_identity_unref);
  
  return cache;
}

void
gzochid_auth_identity_cache_destroy (gzochid_auth_identity_cache *cache)
{
  gzochid_lru_cache_destroy (cache->cache);
  free (cache);
}

gzochid_auth_identity *
gzochid_auth_identity_from_name (gzochid_auth_identity_cache *cache, char *name)
{
  return gzochid_auth_identity_ref
    (gzochid_lru_cache_lookup (cache->cache, name));
}

gzochid_auth_identity *
gzochid_auth_identity_ref (gzochid_auth_identity *identity)
{
  if (!identity->reserved)  
    g_atomic_int_inc (&identity->ref_count);

  return identity;
}

void
gzochid_auth_identity_unref (gzochid_auth_identity *identity)
{
  if (identity->reserved)
    return;
  
  if (g_atomic_int_dec_and_test (&identity->ref_count)) {
    free (identity->name);
    free (identity);
  }
}

static gchar *
remove_extension (const gchar *filename)
{
  char *dot = rindex (filename, '.');

  if (dot == NULL)
    return g_strdup (filename);
  else return g_strndup (filename, dot - filename);
}

static void
probe_auth_plugin (gpointer data, gpointer user_data)
{
  const gchar *path = (const gchar *) data;
  gzochid_game_context *context = (gzochid_game_context *) user_data;
  GModule *plugin_handle = 
    g_module_open (path, G_MODULE_BIND_LAZY | G_MODULE_BIND_LOCAL);
  int (*plugin_info) (gzochid_auth_plugin *);

  if (plugin_handle == NULL)
    g_warning
      ("Failed to load auth plugin at '%s': %s", path, g_module_error ());
  else if (!g_module_symbol 
      (plugin_handle, PLUGIN_INFO_FUNCTION, (gpointer *) &plugin_info))
    {
      g_warning ("Missing plugin info at '%s': %s", path, g_module_error ());
      g_module_close (plugin_handle);
    }
  else 
    {
      gzochid_auth_plugin *plugin = malloc (sizeof (gzochid_auth_plugin));
      if (plugin_info (plugin) != 0)
	{
	  g_warning ("Failed to introspect plugin at '%s'.", path);

	  free (plugin);
	  g_module_close (plugin_handle);
	}
      else 
	{
	  g_message ("Loaded auth plugin '%s'", plugin->info->name);
	  g_hash_table_insert 
	    (context->auth_plugins, plugin->info->name, plugin);
	  plugin->handle = plugin_handle;
	}
    }
}

static void
probe_auth_plugins (gzochid_game_context *context, const char *search_path)
{
  GDir *plugin_dir = g_dir_open (search_path, 0, NULL);
  const gchar *file = NULL;

  if (plugin_dir != NULL)
    {
      GSequence *plugin_queue = g_sequence_new (free);
      while ((file = g_dir_read_name (plugin_dir)) != NULL)
	{
	  char *stripped_file = NULL;
	  gchar *path = g_build_filename (search_path, file, NULL);

	  /* Make a reasonable attempt to ignore non-regular files, like
	     directories. This test is obviously not synchronous, which means
	     that it could be "fooled" by a file that is removed or replaced
	     with some other entity; the consequence is warning downstream. */

	  if (!g_file_test (path, G_FILE_TEST_IS_REGULAR))
	    {
	      g_free (path);
	      continue;
	    }

	  g_free (path);
	  stripped_file = remove_extension (file);
	  path = g_build_filename (search_path, stripped_file, NULL);

	  free (stripped_file);

	  if (g_sequence_lookup 
	      (plugin_queue, path, gzochid_util_string_data_compare, NULL) 
	      == NULL)
	    g_sequence_insert_sorted 
	      (plugin_queue, path, gzochid_util_string_data_compare, NULL);
	  else g_free (path);
	}
      
      g_sequence_foreach (plugin_queue, probe_auth_plugin, context);
      g_sequence_free (plugin_queue);

      g_dir_close (plugin_dir);
    }
}

void
gzochid_auth_init (gzochid_game_context *context)
{
  if (g_module_supported ())
    probe_auth_plugins (context, context->auth_plugin_dir);
  else g_message ("Plugins not supported; skipping auth plugin probe.");
}
