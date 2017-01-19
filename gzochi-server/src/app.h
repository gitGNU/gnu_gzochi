/* app.h: Prototypes and declarations for app.c
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

#ifndef GZOCHID_APP_H
#define GZOCHID_APP_H

#include <glib.h>

#include "auth_int.h"
#include "context.h"
#include "descriptor.h"
#include "event.h"
#include "gzochid-auth.h"
#include "gzochid-storage.h"
#include "metaclient.h"
#include "oids.h"
#include "schedule.h"
#include "stats.h"
#include "tx.h"

enum gzochid_application_state 
  {
    GZOCHID_APPLICATION_STATE_INITIALIZING,
    GZOCHID_APPLICATION_STATE_RUNNING,
    GZOCHID_APPLICATION_STATE_PAUSED,
    GZOCHID_APPLICATION_STATE_STOPPED
  };

struct _gzochid_application_context
{
  gzochid_context base;

  /* The directory containing the application descriptor. Used to resolve
     relative load paths. */

  char *deployment_root;
  GzochidApplicationDescriptor *descriptor;

  GList *load_paths; /* Complete set of module load paths. */

  gzochid_auth_identity *(*authenticator) 
    (unsigned char *, short, gpointer, GError **);
  gpointer auth_data;

  gzochid_auth_identity_cache *identity_cache;
  
  GList *free_oid_blocks;
  GMutex free_oids_lock;

  gzochid_storage_engine_interface *storage_engine_interface;
  
  gzochid_storage_context *storage_context;
  gzochid_storage_store *meta;
  gzochid_storage_store *oids;
  gzochid_storage_store *names;

  gzochid_oid_allocation_strategy *oid_strategy;

  gzochid_task_queue *task_queue;
  struct timeval tx_timeout;
  
  GHashTable *oids_to_clients;
  GHashTable *clients_to_oids;
  GMutex client_mapping_lock;

  /* A mapping of channel oid strings to `GSequences' of session oid strings.*/

  GHashTable *channel_oids_to_local_session_oids; 

  GMutex channel_mapping_lock; /* Protects the channel oid mapping. */
  
  gzochid_event_source *event_source;
  gzochid_application_stats *stats;
};

typedef struct _gzochid_application_context gzochid_application_context;

gzochid_application_context *gzochid_application_context_new (void);

void gzochid_application_context_init (gzochid_application_context *,
				       gzochid_context *,
				       GzochidApplicationDescriptor *,
				       GzochidMetaClientContainer *,
				       GzochidAuthPluginRegistry *,
				       gzochid_storage_engine_interface *,
				       const char *, gzochid_task_queue *,
				       struct timeval);

void gzochid_application_context_free (gzochid_application_context *);

void *gzochid_with_application_context (gzochid_application_context *, 
					gzochid_auth_identity *,
					void *(*) (gpointer), 
					gpointer);

gzochid_application_context *gzochid_get_current_application_context (void);
gzochid_auth_identity *gzochid_get_current_identity (void);

#endif /* GZOCHID_APP_H */
