/* app.h: Prototypes and declarations for app.c
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

#ifndef GZOCHID_APP_H
#define GZOCHID_APP_H

#include <glib.h>

#include "auth_int.h"
#include "callback.h"
#include "context.h"
#include "event.h"
#include "gzochid-auth.h"
#include "gzochid-storage.h"
#include "oids.h"
#include "stats.h"
#include "tx.h"

enum gzochid_application_state 
  {
    GZOCHID_APPLICATION_STATE_INITIALIZING,
    GZOCHID_APPLICATION_STATE_RUNNING,
    GZOCHID_APPLICATION_STATE_PAUSED,
    GZOCHID_APPLICATION_STATE_STOPPED
  };

struct _gzochid_application_descriptor
{
  char *name;
  char *description;

  GList *load_paths; /* Descriptor-specified module load paths. */
 
  gzochid_application_callback *initialized;
  gzochid_application_callback *logged_in;
  gzochid_application_callback *ready;

  char *auth_type;
  GHashTable *auth_properties;

  GHashTable *properties;
};

typedef struct _gzochid_application_descriptor gzochid_application_descriptor;

struct _gzochid_application_context
{
  gzochid_context base;

  /* The directory containing the application descriptor. Used to resolve
     relative load paths. */

  char *deployment_root;
  gzochid_application_descriptor *descriptor;  

  GList *load_paths; /* Complete set of module load paths. */

  gzochid_auth_identity *(*authenticator) 
    (unsigned char *, short, gpointer, GError **);
  gpointer auth_data;

  gzochid_auth_identity_cache *identity_cache;
  
  GList *free_oid_blocks;
  GMutex free_oids_lock;

  gzochid_storage_context *storage_context;
  gzochid_storage_store *meta;
  gzochid_storage_store *oids;
  gzochid_storage_store *names;

  gzochid_oid_allocation_strategy *oid_strategy;
  
  GHashTable *oids_to_clients;
  GHashTable *clients_to_oids;
  GMutex client_mapping_lock;
  
  gzochid_application_event_source *event_source;
  gzochid_application_stats *stats;
};

typedef struct _gzochid_application_context gzochid_application_context;

/* A helper macro to simplify access to the storage interface of the storage 
   engine loaded by the game manager that owns the specified application 
   context.

   Needless to say, this macro assumes that the application context is attached
   to a properly configured game manager. 
*/

#define APP_STORAGE_INTERFACE(app_context) \
  ((gzochid_game_context *) \
   ((gzochid_context *) (app_context))->parent) \
  ->storage_engine->interface

gzochid_application_context *gzochid_application_context_new (void);

void gzochid_application_context_free (gzochid_application_context *);
void gzochid_application_context_init (gzochid_application_context *, 
				       gzochid_context *, 
				       gzochid_application_descriptor *);

void *gzochid_with_application_context (gzochid_application_context *, 
					gzochid_auth_identity *,
					void *(*) (gpointer), 
					gpointer);

gzochid_application_context *gzochid_get_current_application_context (void);
gzochid_auth_identity *gzochid_get_current_identity (void);

#endif /* GZOCHID_APP_H */
