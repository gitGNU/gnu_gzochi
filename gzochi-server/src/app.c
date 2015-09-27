/* app.c: Application context routines for gzochid
 * Copyright (C) 2015 Julian Graham
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
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "context.h"
#include "event.h"
#include "gzochid-auth.h"

static GPrivate thread_application_context_key;
static GPrivate thread_identity_key;

gzochid_application_context *
gzochid_application_context_new (void)
{
  gzochid_application_context *context = calloc 
    (1, sizeof (gzochid_application_context));

  context->oids_to_clients = g_hash_table_new (g_str_hash, g_str_equal);
  context->clients_to_oids = g_hash_table_new (g_direct_hash, g_direct_equal);

  g_mutex_init (&context->free_oids_lock);
  g_mutex_init (&context->client_mapping_lock);

  context->event_source = gzochid_application_event_source_new ();
  context->stats = calloc (1, sizeof (gzochid_application_stats));
  return context;
}

void 
gzochid_application_context_free (gzochid_application_context *app_context)
{
  gzochid_context *context = (gzochid_context *) app_context;
  gzochid_context_free (context);

  g_hash_table_destroy (app_context->oids_to_clients);
  g_hash_table_destroy (app_context->clients_to_oids);
  
  g_list_free (app_context->free_oid_blocks);

  g_mutex_clear (&app_context->free_oids_lock);
  g_mutex_clear (&app_context->client_mapping_lock);

  gzochid_application_event_source_free (app_context->event_source);
  free (app_context->stats);

  free (context);
}

void *
gzochid_with_application_context (gzochid_application_context *context, 
				  gzochid_auth_identity *identity,
				  void *(*worker) (gpointer), gpointer data)
{
  gpointer ret = NULL;
  gboolean private_needs_context = 
    g_private_get (&thread_application_context_key) == NULL;
  SCM application_root_fluid = 
    scm_variable_ref
    (scm_c_module_lookup 
     (scm_c_resolve_module("gzochi app"), "%gzochi:application-root"));

  if (private_needs_context)
    {
      g_private_set (&thread_application_context_key, context);
      g_private_set (&thread_identity_key, identity);

      assert (context->deployment_root != NULL);
      
      scm_fluid_set_x
	(application_root_fluid, 
	 scm_from_locale_string (context->deployment_root));
    }
  
  ret = worker (data);

  if (private_needs_context)
    {
      g_private_set (&thread_application_context_key, NULL);
      g_private_set (&thread_identity_key, NULL);

      scm_fluid_set_x (application_root_fluid, SCM_UNSPECIFIED);
    }

  return ret;
}

gzochid_application_context *
gzochid_get_current_application_context (void)
{
  return (gzochid_application_context *)
    g_private_get (&thread_application_context_key);
}

gzochid_auth_identity *
gzochid_get_current_identity (void)
{
  return (gzochid_auth_identity *) g_private_get (&thread_identity_key);
}
