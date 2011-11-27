/* guile.c: GNU Guile interface routines for gzochid
 * Copyright (C) 2011 Julian Graham
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
#include <libguile.h>
#include <stdlib.h>

#include "guile.h"
#include "threads.h"

typedef struct _gzochid_guile_thread_work 
{
  gzochid_thread_worker worker;
  gpointer data;
  gpointer user_data;
} gzochid_guile_thread_work;

static GStaticMutex resolve_module_mutex = G_STATIC_MUTEX_INIT;
static GHashTable *module_cache;

static void *guile_unwrapper (gpointer data)
{
  gzochid_guile_thread_work *work = (gzochid_guile_thread_work *) data;
  work->worker (work->data, work->user_data);
  free (work);

  return NULL;
}

static void guile_dispatch (gpointer data, gpointer user_data)
{
  gzochid_guile_thread_work *work = (gzochid_guile_thread_work *) data;
  work->user_data = user_data;
  scm_with_guile (guile_unwrapper, work);
}

static SCM resolve_module 
(gzochid_application_context *context, char *module_name)
{
  SCM module = SCM_BOOL_F;
  SCM load_path = SCM_EOL, new_load_path = SCM_EOL, backup_load_path = SCM_EOL;
  GList *path_ptr = context->descriptor->load_paths;

  load_path = scm_module_variable 
    (scm_c_resolve_module ("guile"), scm_from_locale_symbol ("%load-path"));
  backup_load_path = scm_variable_ref (load_path);
  new_load_path = backup_load_path;

  while (path_ptr != NULL)
    {
      new_load_path = scm_cons 
	(scm_from_locale_string ((char *) path_ptr->data), new_load_path);      
      path_ptr = path_ptr->prev;
    }
  
  scm_variable_set_x (load_path, new_load_path);
  module = scm_c_resolve_module (module_name);
  scm_gc_protect_object (module);
  scm_variable_set_x (load_path, backup_load_path);

  return module;
}

SCM gzochid_guile_resolve_module 
(gzochid_application_context *context, char *module_name)
{
  SCM module = SCM_BOOL_F;
  GHashTable *application_module_cache;

  g_static_mutex_lock (&resolve_module_mutex);

  if (module_cache == NULL)
    module_cache = g_hash_table_new (g_direct_hash, g_direct_equal);

  application_module_cache = g_hash_table_lookup (module_cache, context);
  if (application_module_cache == NULL)
    {
      application_module_cache = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (module_cache, context, application_module_cache);
    }

  module = g_hash_table_lookup (application_module_cache, module_name);
  if (module == NULL)
    {
      module = resolve_module (context, module_name);
      g_hash_table_insert (application_module_cache, module_name, module);
    }

  g_static_mutex_unlock (&resolve_module_mutex);

  return module;
}

void gzochid_guile_run (gzochid_thread_worker worker, gpointer data)
{
  gzochid_guile_thread_work *work = 
    calloc (1, sizeof (gzochid_guile_thread_work));
  
  work->worker = worker;
  work->data = data;
  
  guile_dispatch (work, NULL);
}

void gzochid_guile_thread_pool_push
(GThreadPool *pool, gzochid_thread_worker worker, gpointer data, GError **error)
{ 
  gzochid_guile_thread_work *work = calloc 
    (1, sizeof (gzochid_guile_thread_work));

  work->worker = worker;
  work->data = data;

  gzochid_thread_pool_push (pool, guile_dispatch, work, error);
}
