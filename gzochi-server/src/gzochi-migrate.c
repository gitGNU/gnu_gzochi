/* gzochid-migrate.c: Utility for migrating game data schema
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
#include <errno.h>
#include <getopt.h>
#include <glib.h>
#include <libguile.h>
#include <libintl.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "app.h"
#include "config.h"
#include "data.h"
#include "descriptor.h"
#include "game.h"
#include "guile.h"
#include "gzochid-storage.h"
#include "reloc.h"
#include "scheme.h"
#include "scheme-task.h"
#include "toollib.h"
#include "tx.h"
#include "util.h"

#define _(String) gettext (String)

#define Q(x) #x
#define QUOTE(x) Q(x)

#ifndef GZOCHID_CONF_LOCATION
#define GZOCHID_CONF_LOCATION "/etc/gzochid.conf"
#endif /* GZOCHID_CONF_LOCATION */

#define GAME_DESCRIPTOR_XML "game.xml"

#define SERVER_FS_APPS_DEFAULT "/var/gzochid/deploy"
#define SERVER_FS_DATA_DEFAULT "/var/gzochid/data"

static SCM 
scm_gzochi_visit_object = SCM_BOOL_F;
static SCM 
scm_push_type_registry_x = SCM_BOOL_F;
static SCM 
scm_pop_type_registry_x = SCM_BOOL_F;

/* TODO: Remove temporary, fake definition of `GZOCHID_TYPE_ROOT_CONTEXT' as
   soon as the root context is decoupled from the game server. */

int
gzochid_root_context_get_type ()
{
  return g_object_get_type ();
}

struct migration 
{
  gzochid_application_context *context;
  struct migration_descriptor *descriptor;
  gboolean dry_run;
  gboolean explicit_rollback;
  gboolean quiet;

  GQueue *pending_oids;
  char *visited_oids_path;
  gzochid_storage_store *visited_oids;

  SCM callback;
  SCM input_registry;
  SCM output_registry;
  gboolean pushed_registry;

  struct timeval start_time;
  struct timeval end_time;
  guint64 num_visited;
  guint64 num_transformed;
  guint64 num_removed;
};

struct migration_descriptor
{
  char *target;
  GList *load_paths;

  char *input_registry_name;
  char *input_registry_module;

  char *output_registry_name;
  char *output_registry_module;

  char *callback_name;
  char *callback_module;
};

/*
  Helper functions and parser callbacks for GLib's XML subset parser.
 */

static const gchar *
attribute_value (const gchar **attribute_names, const gchar **attribute_values,
		 const gchar *attribute_name)
{
  int i = 0;
  while (attribute_names[i] != NULL)
    {
      if (strcmp (attribute_names[i], attribute_name) == 0)
        return attribute_values[i];
      i++;
    }

  return NULL;
}

static void
start_element (GMarkupParseContext *context, const gchar *element_name,
	       const gchar **attribute_names, const gchar **attribute_values,
	       gpointer user_data, GError **error)
{
  struct migration_descriptor *md = user_data;
  const GSList *stack = g_markup_parse_context_get_element_stack (context);
  char *parent = stack == NULL || stack->next == NULL 
    ? NULL : stack->next->data;

  if (strcmp (element_name, "migration") == 0)
    {
      const gchar *target_attr = 
	attribute_value (attribute_names, attribute_values, "target");

      if (target_attr == NULL || strlen (target_attr) == 0)
	g_set_error 
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	   "migration@target is required");
      else md->target = strdup (target_attr);
    }
  else if (strcmp (element_name, "load-paths") == 0)
    {
      if (parent == NULL || strcmp (parent, "migration") != 0)
	g_set_error
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT,
	   "invalid load-paths location");
    }
  else if (strcmp (element_name, "load-path") == 0)
    {
      if (parent == NULL || strcmp (parent, "load-paths") != 0)
	g_set_error
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT,
	   "invalid load-path location");
    }
  else if (strcmp (element_name, "input-registry") == 0)
    {
      if (parent == NULL || strcmp (parent, "migration") != 0)
	g_set_error
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT,
	   "invalid input-registry location");
      else
	{
	  const gchar *module_attr = 
	    attribute_value (attribute_names, attribute_values, "module");
	  const gchar *name_attr = 
	    attribute_value (attribute_names, attribute_values, "name");

	  if (module_attr == NULL || strlen (module_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "input-registry@module is required");
	  else if (name_attr == NULL || strlen (name_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "input-registry@name is required");
	  else
	    {
	      md->input_registry_module = strdup (module_attr);
	      md->input_registry_name = strdup (name_attr);
	    }
	}
    }
  else if (strcmp (element_name, "output-registry") == 0)
    {
      if (parent == NULL || strcmp (parent, "migration") != 0)
	g_set_error
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT,
	   "invalid output-registry location");
      else
	{
	  const gchar *module_attr = 
	    attribute_value (attribute_names, attribute_values, "module");
	  const gchar *name_attr = 
	    attribute_value (attribute_names, attribute_values, "name");

	  if (module_attr == NULL || strlen (module_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "output-registry@module is required");
	  else if (name_attr == NULL || strlen (name_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "output-registry@name is required");
	  else
	    {
	      md->output_registry_module = strdup (module_attr);
	      md->output_registry_name = strdup (name_attr);
	    }
	}
    }
  else if (strcmp (element_name, "callback") == 0)
    {
      if (parent == NULL || strcmp (parent, "migration") != 0)
	g_set_error
	  (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT,
	   "invalid callback location");
      else
	{
	  const gchar *module_attr = 
	    attribute_value (attribute_names, attribute_values, "module");
	  const gchar *procedure_attr = 
	    attribute_value (attribute_names, attribute_values, "procedure");

	  if (module_attr == NULL || strlen (module_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "callback@module is required");
	  else if (procedure_attr == NULL || strlen (procedure_attr) == 0)
	    g_set_error
	      (error, G_MARKUP_ERROR, G_MARKUP_ERROR_MISSING_ATTRIBUTE,
	       "callback@procedure is required");
	  else
	    {
	      md->callback_module = strdup (module_attr);
	      md->callback_name = strdup (procedure_attr);
	    }
	}
    }
  else g_set_error 
	 (error, G_MARKUP_ERROR, G_MARKUP_ERROR_UNKNOWN_ELEMENT,
	  "unknown element %s", element_name);
}

static void
text (GMarkupParseContext *context, const gchar *text, gsize text_len,
      gpointer user_data, GError **error)
{
  struct migration_descriptor *md = user_data;
  const GSList *stack = g_markup_parse_context_get_element_stack (context);
  char *parent = stack == NULL ? NULL : stack->data;
 
  gchar *text_clone = g_strdup (text);
  text_clone = g_strstrip (text_clone);

  if (strlen (text_clone) > 0)
    {
      if (parent != NULL && strcmp (parent, "load-path") == 0)
	md->load_paths = g_list_append 
	  (md->load_paths, strndup (text, text_len));
      else g_set_error 
	     (error, G_MARKUP_ERROR, G_MARKUP_ERROR_INVALID_CONTENT, 
	      "unexpected text content");
    }

  free (text_clone);
}

static void
error (GMarkupParseContext *context, GError *err, gpointer user_data)
{
  g_critical ("Failed to parse migration descriptor: %s", err->message);
  exit (EXIT_FAILURE);
}

static GMarkupParser 
migration_parser = { start_element, NULL, text, NULL, error };

static struct migration_descriptor *
parse_descriptor (FILE *descriptor)
{
  struct migration_descriptor *md = 
    calloc (1, sizeof (struct migration_descriptor));

  GMarkupParseContext *context = g_markup_parse_context_new
    (&migration_parser, 0, md, NULL);

  char buf[1024];
  int len = 0;
  
  while ((len = fread (buf, sizeof (char), 1024, descriptor)) > 0)
    g_markup_parse_context_parse (context, buf, len, NULL);
  g_markup_parse_context_end_parse (context, NULL);

  return md;
}

static SCM
enqueue_oids (SCM migration_ptr, SCM oids)
{
  struct migration *m = scm_to_pointer (migration_ptr);

  while (oids != SCM_EOL)
    {
      guint64 *oid_ptr = malloc (sizeof (guint64));

      *oid_ptr = scm_to_uint64 (SCM_CAR (oids));
      g_queue_push_tail (m->pending_oids, oid_ptr);
      oids = SCM_CDR (oids);
    }

  return SCM_UNSPECIFIED;
}

static void
initialize_scheme_bindings ()
{
  SCM gpd = scm_c_resolve_module ("gzochi private data");
  SCM gpdm = scm_c_resolve_module ("gzochi private data migration");
  SCM scm_enqueue_oids = scm_c_make_gsubr 
    ("enqueue-oids!", 2, 0, 0, enqueue_oids);

  gzochid_guile_init ();
  gzochid_scheme_initialize_bindings ();

  scm_push_type_registry_x = 
    scm_variable_ref (scm_c_module_lookup (gpd, "gzochi:push-type-registry!"));
  scm_pop_type_registry_x =
    scm_variable_ref (scm_c_module_lookup (gpd, "gzochi:pop-type-registry!"));
  scm_gzochi_visit_object = 
    scm_variable_ref (scm_c_module_lookup (gpdm, "gzochi:visit-object"));

  scm_variable_set_x 
    (scm_c_module_lookup (gpdm, "enqueue-oids!"), scm_enqueue_oids);

  scm_gc_protect_object (scm_push_type_registry_x);
  scm_gc_protect_object (scm_pop_type_registry_x);
  scm_gc_protect_object (scm_gzochi_visit_object);
  scm_gc_protect_object (scm_enqueue_oids);
}

static gzochid_storage_store *
create_store (gzochid_storage_engine_interface *iface, 
	      gzochid_storage_context *context, char *path)
{
  gzochid_storage_store *store = iface->open 
    (context, path, GZOCHID_STORAGE_CREATE | GZOCHID_STORAGE_EXCL);

  if (store == NULL)
    {
      g_critical ("Failed to open store in %s", path);
      exit (EXIT_FAILURE);
    }
  else return store;
}

static gzochid_storage_store *
create_oid_scratch_storage (gzochid_storage_engine_interface *iface, 
			    char *scratch_dir)
{
  gchar *filename = g_strconcat (scratch_dir, "/oids", NULL);
  gzochid_storage_context *context = iface->initialize (scratch_dir);
  gzochid_storage_store *store = create_store (iface, context, filename);

  g_free (filename);
  return store;
}

static void
cleanup_oids_scratch_storage (gzochid_storage_engine_interface *iface, 
			      gzochid_storage_store *s, char *scratch_dir)
{
  gzochid_storage_context *context = s->context;

  iface->close_store (s);
  iface->destroy_store (context, scratch_dir);
}

static SCM 
resolve_or_die (char *module_name, char *symbol)
{
  SCM module = scm_c_resolve_module (module_name);
  SCM variable = scm_module_variable (module, scm_from_locale_symbol (symbol));

  if (variable == SCM_BOOL_F)
    {
      g_critical ("Failed to resolve (@ %s (%s)).", symbol, module_name);
      exit (EXIT_FAILURE);
    }
  else return scm_variable_ref (variable);
}

static void
append_load_paths (GList *load_paths)
{
  GList *load_path_ptr = load_paths;
  while (load_path_ptr != NULL)
    {
      gzochid_guile_add_to_load_path (load_path_ptr->data);
      load_path_ptr = load_path_ptr->next;
    }
}

static struct migration *
create_migration (gzochid_application_context *context, 
		  struct migration_descriptor *md, gboolean dry_run, 
		  gboolean quiet)
{ 
  struct migration *m = malloc (sizeof (struct migration));
  char *scratch_dir_template = strdup ("gzochid-migrate-XXXXXXX");
  char *scratch_dir = g_dir_make_tmp (scratch_dir_template, NULL);
  
  if (scratch_dir == NULL)
    {
      g_critical ("Unable to create scratch storage: %s", strerror (errno));
      exit (EXIT_FAILURE);
      return NULL; /* Never reached. */
    }

  assert (md->callback_name != NULL && strlen (md->callback_name) > 0);
  assert (md->callback_module != NULL && strlen (md->callback_module) > 0);

  append_load_paths (context->load_paths);
  append_load_paths (md->load_paths);

  m->context = context;
  m->descriptor = md;
  m->dry_run = dry_run;
  m->explicit_rollback = FALSE;
  m->quiet = quiet;
  m->pending_oids = g_queue_new ();
  
  m->visited_oids = create_oid_scratch_storage 
    (context->storage_engine_interface, scratch_dir);
  m->visited_oids_path = scratch_dir;

  m->callback = resolve_or_die (md->callback_module, md->callback_name);

  if (md->input_registry_name != NULL)
    {
      assert (md->input_registry_module != NULL);
      m->input_registry = resolve_or_die 
	(md->input_registry_module, md->input_registry_name);
    }
  else m->input_registry = SCM_BOOL_F;

  if (md->output_registry_name != NULL)
    {
      assert (md->output_registry_module != NULL);
      m->output_registry = resolve_or_die
	(md->output_registry_module, md->output_registry_name);
    }
  else m->output_registry = SCM_BOOL_F;

  m->pushed_registry = FALSE;

  scm_gc_protect_object (m->input_registry);
  scm_gc_protect_object (m->output_registry);
  scm_gc_protect_object (m->callback);

  return m;
}

static void 
cleanup_application_context (gzochid_application_context *context)
{
  context->storage_engine_interface->close_store (context->meta);
  context->storage_engine_interface->close_store (context->names);
  context->storage_engine_interface->close_store (context->oids);

  gzochid_application_context_free (context);
}

static void
cleanup_migration (struct migration *m)
{
  cleanup_oids_scratch_storage 
    (m->context->storage_engine_interface, m->visited_oids,
     m->visited_oids_path);
  cleanup_application_context (m->context);

  free (m->visited_oids_path);

  g_queue_free (m->pending_oids);

  if (m->input_registry != SCM_BOOL_F)
    scm_gc_unprotect_object (m->input_registry);
  if (m->output_registry != SCM_BOOL_F)
    scm_gc_unprotect_object (m->output_registry);
  scm_gc_unprotect_object (m->callback);

  free (m);
}

static gzochid_storage_store *
open_store (gzochid_application_context *context, char *path, char *db)
{
  gchar *filename = g_strconcat (path, "/", db, NULL);
  gzochid_storage_store *store = gzochid_tool_open_store 
    (context->storage_engine_interface, context->storage_context, filename);

  g_free (filename);
  return store;
}

static gzochid_application_context *
create_application_context (char *path, char *app)
{
  GHashTable *config = NULL; 
  FILE *descriptor_file = NULL;

  const char *env = getenv ("GZOCHID_CONF_LOCATION");
  const char *gzochid_conf = NULL;

  gzochid_game_context *parent = gzochid_game_context_new (NULL);
  gzochid_application_context *context = gzochid_application_context_new ();
  char *work_dir = NULL, *data_dir = NULL, *apps_dir = NULL,
    *descriptor_path = NULL, *storage_engine = NULL;

  if (path != NULL)
    gzochid_conf = path;
  else if (env != NULL)
    gzochid_conf = env;
  else gzochid_conf = QUOTE (GZOCHID_CONF_LOCATION);

  config = gzochid_tool_load_game_config (gzochid_conf);

  if (g_hash_table_contains (config, "server.fs.data"))
    work_dir = strdup (g_hash_table_lookup (config, "server.fs.data"));
  else work_dir = strdup (SERVER_FS_DATA_DEFAULT);

  data_dir = g_strconcat (work_dir, "/", app, NULL);  

  if (g_hash_table_contains (config, "server.fs.apps"))    
    apps_dir = strdup (g_hash_table_lookup (config, "server.fs.apps"));
  else apps_dir = strdup (SERVER_FS_APPS_DEFAULT);

  descriptor_path = g_build_filename (apps_dir, app, GAME_DESCRIPTOR_XML, NULL);
  
  if (g_path_is_absolute (apps_dir))
    descriptor_path = g_build_filename
      (apps_dir, app, GAME_DESCRIPTOR_XML, NULL);
  else
    {
      gchar *basedir = g_path_get_dirname (gzochid_conf);
      descriptor_path = g_build_filename
	(basedir, apps_dir, app, GAME_DESCRIPTOR_XML, NULL);
      g_free (basedir);
    }
  
  descriptor_file = fopen (descriptor_path, "r");
  if (descriptor_file == NULL)
    {
      g_critical
	("Failed to open application descriptor in %s", descriptor_path);
      exit (EXIT_FAILURE);
    }
  
  context->descriptor =
    gzochid_config_parse_application_descriptor (descriptor_file);

  fclose (descriptor_file);

  if (context->descriptor == NULL)
    {
      g_critical
	("Failed to parse application descriptor in %s", descriptor_path);
      exit (EXIT_FAILURE);
    }

  context->deployment_root = g_path_get_dirname (descriptor_path);
  context->load_paths = g_list_prepend
    (g_list_copy (context->descriptor->load_paths),
     strdup (context->deployment_root));

  if (g_hash_table_contains (config, "storage.engine"))
    storage_engine = strdup (g_hash_table_lookup (config, "storage.engine"));
  else
    {
      g_critical ("storage.engine is required.");
      exit (EXIT_FAILURE);
    }

  parent->storage_engine =
    gzochid_tool_probe_storage_engine (config, storage_engine);

  context->storage_engine_interface = parent->storage_engine->interface;
  context->storage_context = context->storage_engine_interface
    ->initialize (data_dir);

  if (context->storage_context == NULL)
    {
      fprintf (stderr, "Failed to initialize store in %s", data_dir);
      exit (EXIT_FAILURE);
    }

  context->meta = open_store (context, data_dir, "meta");
  context->names = open_store (context, data_dir, "names");
  context->oids = open_store (context, data_dir, "oids");

  free (work_dir);
  free (data_dir);
  free (apps_dir);
  free (descriptor_path);
  free (storage_engine);

  return context;
}

static SCM
invoke_callback (struct migration *m, gzochid_data_managed_reference *obj_ref)
{
  SCM migration_ptr = scm_from_pointer (m, NULL);
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  SCM obj = gzochid_scm_location_resolve (m->context, obj_ref->obj);
  SCM ret = SCM_BOOL_F;

  gpointer args[4];

  args[0] = scm_gzochi_visit_object;
  args[1] = scm_list_3 (migration_ptr, obj, m->callback);
  args[2] = exception_var;
  args[3] = &ret;

  gzochid_scheme_application_worker (m->context, NULL, args);

  scm_remember_upto_here_1 (migration_ptr);
  scm_remember_upto_here_1 (exception_var);
  scm_remember_upto_here_1 (obj);

  return ret;
}

/*
  A no-op transaction participant, so that the migration process can join and
  possibly roll back the transaction (if performing a dry run).
 */

static int
prepare (gpointer user_data) { return TRUE; }

static void
commit (gpointer user_data) { }

static void
rollback (gpointer user_data) { }

static gzochid_transaction_participant 
migration_participant = { "migration", prepare, commit, rollback };


static void
migrate_object_tx (gpointer data)
{
  gpointer *args = data;
  struct migration *m = args[0];
  guint64 *oid = args[1];

  GError *err = NULL;
  gzochid_data_managed_reference *obj_ref = NULL;

  m->explicit_rollback = FALSE;
  if (m->pushed_registry)
    {
      scm_call_0 (scm_pop_type_registry_x);
      m->pushed_registry = FALSE;
    }
  if (m->input_registry != SCM_BOOL_F)
    {
      scm_call_1 (scm_push_type_registry_x, m->input_registry);
      m->pushed_registry = TRUE;
    }

  obj_ref = gzochid_data_create_reference_to_oid 
    (m->context, &gzochid_scm_location_aware_serialization, *oid);
  gzochid_data_dereference (obj_ref, &err);

  m->num_visited++;

  if (err != NULL)
    {
      if (g_error_matches 
	  (err, GZOCHID_DATA_ERROR, GZOCHID_DATA_ERROR_NOT_FOUND))
	g_warning ("No data found for oid %" G_GUINT64_FORMAT ".", *oid);
      else 
	{
	  g_critical 
	    ("Failed to deserialize data for oid %" G_GUINT64_FORMAT ": %s",
	     *oid, err->message);
	  exit (EXIT_FAILURE);
	}
    }
  else
    {
      SCM ret = invoke_callback (m, obj_ref);

      if (ret == SCM_BOOL_F)
	{
	  gzochid_data_remove_object (obj_ref, &err);
	  if (err != NULL)
	    {
	      g_critical
		("Failed to remove data for oid %" G_GUINT64_FORMAT, *oid);
	      exit (EXIT_FAILURE);
	    }
	  
	  m->num_removed++;
	}
      else if (ret != SCM_UNSPECIFIED)
	{
	  /* gzochid_scm_location_get does not protect objects (though it does
	     unprotect them during finalization) so do it here explicitly. */

	  scm_gc_protect_object (ret);

	  obj_ref->obj = gzochid_scm_location_get (m->context, ret);
	  obj_ref->state = GZOCHID_MANAGED_REFERENCE_STATE_MODIFIED;

	  m->num_transformed++;
	}
    }

  if (m->pushed_registry)
    {
      scm_call_0 (scm_pop_type_registry_x);
      m->pushed_registry = FALSE;
    }
  if (m->output_registry != SCM_BOOL_F)
    {
      scm_call_1 (scm_push_type_registry_x, m->output_registry);
      m->pushed_registry = TRUE;
    }

  if (m->dry_run && !m->explicit_rollback)
    {
      gzochid_transaction_join (&migration_participant, NULL);
      gzochid_transaction_mark_for_rollback (&migration_participant, TRUE);
      m->explicit_rollback = TRUE;
    }
}

static void 
migrate_object (struct migration *m, guint64 oid)
{
  gpointer args[2];

  args[0] = m;
  args[1] = &oid;

  gzochid_storage_transaction *tx = m->context->storage_engine_interface
    ->transaction_begin (m->context->storage_context);
  guint64 encoded_oid = gzochid_util_encode_oid (oid);
  char *val = m->context->storage_engine_interface->transaction_get
    (tx, m->visited_oids, (char *) &encoded_oid, sizeof (guint64), NULL);
      
  if (val == NULL)
    {
      m->context->storage_engine_interface->transaction_put
	(tx, m->visited_oids, (char *) &encoded_oid, sizeof (guint64), "", 1);
      if (gzochid_transaction_execute (migrate_object_tx, args) 
	  != GZOCHID_TRANSACTION_SUCCESS && !m->explicit_rollback)
	{
	  g_critical ("Migration transaction failed.");
	  exit (EXIT_FAILURE);
	}
    }
  else free (val);

  m->context->storage_engine_interface->transaction_prepare (tx);
  m->context->storage_engine_interface->transaction_commit (tx);
}

struct datum
{
  char *data;
  size_t data_len;
};

static struct datum
next_key (struct migration *m, struct datum last_key)
{
  struct datum key = { NULL, 0 };
  gzochid_storage_transaction *tx = m->context->storage_engine_interface
    ->transaction_begin (m->context->storage_context);
  
  if (last_key.data == NULL)
    key.data = m->context->storage_engine_interface
      ->transaction_next_key (tx, m->context->names, "o.", 2, &key.data_len);
  else key.data = m->context->storage_engine_interface
	 ->transaction_next_key (tx, m->context->names, last_key.data,
				 last_key.data_len, &key.data_len);

  m->context->storage_engine_interface->transaction_rollback (tx);
  
  if (key.data != NULL && strncmp ("o.", key.data, 2) != 0)
    {
      free (key.data);
      memset (&key, 0, sizeof (struct datum));
    }
  
  return key;
}

static void
run_migration (struct migration *m)
{
  struct datum key = { NULL, 0 }, last_key = { NULL, 0 };

  while (TRUE)
    {
      guint64 *oid = NULL;

      if (! g_queue_is_empty (m->pending_oids))
	oid = g_queue_pop_head (m->pending_oids);
      else
	{
	  last_key = key;
	  key = next_key (m, last_key);
	  
	  free (last_key.data);

	  if (key.data == NULL)
	    break;
	  else
	    {
	      gzochid_storage_transaction *tx =
		m->context->storage_engine_interface->transaction_begin
		(m->context->storage_context);

	      size_t oid_len = 0;
	      char *oid_bytes = NULL;
	      guint64 encoded_oid = 0;
	      
	      oid_bytes = m->context->storage_engine_interface->transaction_get 
		(tx, m->context->names, key.data, key.data_len, &oid_len);

	      assert (oid_len == sizeof (guint64));

	      memcpy (&encoded_oid, oid_bytes, sizeof (guint64));

	      oid = malloc (sizeof (guint64));
	      *oid = gzochid_util_decode_oid (encoded_oid);
	      
	      m->context->storage_engine_interface->transaction_rollback (tx);
	    }
	}
      
      migrate_object (m, *oid);
      free (oid);
    }
}

static void
report_stats (struct migration *m)
{
  struct timeval ret;

  timersub (&m->end_time, &m->start_time, &ret);
  fprintf (stderr, "Migration complete in %lu ms.\n", 
	   ret.tv_sec * 1000 + ret.tv_usec / 1000);

  fprintf (stderr, "Objects visited: %" G_GUINT64_FORMAT "\n", m->num_visited);
  fprintf (stderr, "Objects transformed: %" G_GUINT64_FORMAT "\n",
	   m->num_transformed);
  fprintf (stderr, "Objects removed: %" G_GUINT64_FORMAT "\n", m->num_removed);
}

static struct migration_descriptor *
create_migration_descriptor (const char *path)
{
  FILE *f = fopen (path, "r");
  struct migration_descriptor *md = NULL;

  if (f == NULL)
    {
      g_critical 
	("Failed to open migration descriptor %s: %s", path, strerror (errno));
      exit (EXIT_FAILURE);
    }
  
  md = parse_descriptor (f);
  fclose (f);

  return md;
}

static void
migrate 
(char *gzochid_conf_path, const char *path, gboolean dry_run, gboolean quiet)
{
  struct migration *m = NULL;
  struct migration_descriptor *md = create_migration_descriptor (path);

  m = create_migration 
    (create_application_context (gzochid_conf_path, md->target), md, dry_run, 
     quiet);

  gettimeofday (&m->start_time,  NULL);
  run_migration (m);
  gettimeofday (&m->end_time,  NULL);
  
  if (!m->quiet)
    report_stats (m);

  cleanup_migration (m);
}

/*
  A dummy GLib log handler to use for suppressing log messages when the 
  migrator is in "quiet" mode.
*/
static void 
null_log_handler 
(const gchar *domain, GLogLevelFlags level, const gchar *msg, gpointer data)
{
}

static const struct option longopts[] =
  {
    { "config", required_argument, NULL, 'c' },
    { "help", no_argument, NULL, 'h' },
    { "version", no_argument, NULL, 'v' },
    { "dry-run", no_argument, NULL, 'n' },
    { "quiet", no_argument, NULL, 'q' },
    { NULL, 0, NULL, 0 }
  };

static void
print_version (void)
{
  fprintf (stderr, "gzochi-migrate (gzochi) %s\n", VERSION);

  fputs ("", stderr);
  fprintf (stderr, _("\
Copyright (C) %s Julian Graham\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"),
          "2015");
}

static void
print_help (const char *program_name)
{
  fprintf (stderr, _("\
Usage: %s [-c <CONF>] [-n] [-q] <MIGRATION_DESCRIPTOR>\n\
       %s [-h | -v]\n"), program_name, program_name);
  
  fputs ("", stderr);
  fputs (_("\
  -c, --config        full path to gzochid.conf\n\
  -n, --dry-run       run the migration without committing changes\n\
  -q, --quiet         run without logging messages or stats\n\
  -h, --help          display this help and exit\n\
  -v, --version       display version information and exit\n"), stderr);

  fputs ("", stderr);
  fprintf (stderr, _("\
Report bugs to: %s\n"), PACKAGE_BUGREPORT);
#ifdef PACKAGE_PACKAGER_BUG_REPORTS
  fprintf (stderr, _("Report %s bugs to: %s\n"), PACKAGE_PACKAGER,
          PACKAGE_PACKAGER_BUG_REPORTS);
#endif /* PACKAGE_PACKAGER_BUG_REPORTS */

#ifdef PACKAGE_URL
  fprintf (stderr, _("%s home page: <%s>\n"), PACKAGE_NAME, PACKAGE_URL);
#else
  fprintf (stderr, _("%s home page: <http://www.nongnu.org/%s/>\n"),
	   PACKAGE_NAME, PACKAGE);
#endif /* PACKAGE_URL */
}

static void
inner_main (void *data, int argc, char *argv[])
{
  int optc = 0;
  const char *program_name = argv[0];
  char *gzochid_conf_path = NULL;
  gboolean dry_run = FALSE;
  gboolean quiet = FALSE;
  
  setlocale (LC_ALL, "");
  
  while ((optc = getopt_long (argc, argv, "+c:nqhv", longopts, NULL)) != -1)
    switch (optc)
      {
      case 'c':
	gzochid_conf_path = strdup (optarg);
	break;
      case 'n': 
	dry_run = TRUE;
	break;
      case 'q':
	quiet = TRUE;
	g_log_set_handler 
	  (NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, 
	   null_log_handler, NULL);
	break;

      case 'v':
	print_version ();
	exit (EXIT_SUCCESS);
	break;
      case 'h':
	print_help (program_name);
	exit (EXIT_SUCCESS);
	break;
      }

  if (optind != argc - 1)
    {
      print_help (program_name);
      exit (EXIT_FAILURE);
    }
  else 
    {
      initialize_scheme_bindings ();
      migrate (gzochid_conf_path, argv[optind], dry_run, quiet);
    }
}

/*
  Bootstrap Guile and call the "real" main.
 */
int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
