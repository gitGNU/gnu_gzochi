/* app.h: Prototypes and declarations for app.c
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

#ifndef GZOCHID_APP_H
#define GZOCHID_APP_H

#include <glib.h>
#include <gmp.h>
#include <libguile.h>

#include "auth.h"
#include "context.h"
#include "io.h"
#include "storage.h"

enum gzochid_application_state 
  {
    GZOCHID_APPLICATION_STATE_INITIALIZING,
    GZOCHID_APPLICATION_STATE_RUNNING,
    GZOCHID_APPLICATION_STATE_PAUSED,
    GZOCHID_APPLICATION_STATE_STOPPED
  };

typedef struct _gzochid_application_callback
{
  GList *module;
  char *procedure;
  mpz_t scm_oid;
} gzochid_application_callback;

typedef struct _gzochid_application_descriptor
{
  char *name;
  char *description;

  GList *load_paths;
 
  gzochid_application_callback *initialized;
  gzochid_application_callback *logged_in;

  GHashTable *properties;
} gzochid_application_descriptor;

typedef struct _gzochid_application_context
{
  gzochid_context base;

  gzochid_application_descriptor *descriptor;
  gzochid_auth_identity *(*authenticator)
    (struct _gzochid_application_context *, unsigned char *, short);

  GList *free_oid_blocks;
  GMutex *free_oids_lock;

  gzochid_storage_store *meta;
  gzochid_storage_store *oids;
  gzochid_storage_store *names;

  GHashTable *oids_to_clients;
  GHashTable *clients_to_oids;
  GMutex *client_mapping_lock;
} gzochid_application_context;

typedef void (*gzochid_application_worker) 
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
typedef struct _gzochid_application_work_unit
{
  GMutex *lock;
  GCond *cond;
  gzochid_application_worker worker;
  gpointer data;
} gzochid_application_work_unit;

gzochid_io_serialization gzochid_application_callback_serialization;

gzochid_application_callback *gzochid_application_callback_new 
(char *, GList *, mpz_t);
void gzochid_application_callback_free (gzochid_application_callback *);

void gzochid_application_schedule_work_unit 
(gzochid_application_context *, gzochid_auth_identity *,
 gzochid_application_work_unit *);
void gzochid_application_work_unit_worker (gpointer, gpointer);
gzochid_application_work_unit *gzochid_application_work_unit_new 
(gzochid_application_worker, gpointer);
void gzochid_application_work_unit_free (gzochid_application_work_unit *);

gzochid_application_descriptor *gzochid_application_parse_descriptor (char *);

gzochid_application_context *gzochid_application_context_new (void);
void gzochid_application_context_free (gzochid_application_context *);
void gzochid_application_context_init 
(gzochid_application_context *, gzochid_context *, 
 gzochid_application_descriptor *);

struct _gzochid_protocol_client;

void gzochid_application_client_logged_in
(gzochid_application_context *, struct _gzochid_protocol_client *);
void gzochid_application_client_disconnected
(gzochid_application_context *, struct _gzochid_protocol_client *);
void gzochid_application_session_received_message 
(gzochid_application_context *, struct _gzochid_protocol_client *, 
 unsigned char *, short);
void gzochid_application_channel_message_received
(gzochid_application_context *, struct _gzochid_protocol_client *, 
 char *, unsigned char *, short);

#endif /* GZOCHID_APP_H */
