/* data.h: Prototypes and declarations for data.c
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

#ifndef GZOCHID_DATA_H
#define GZOCHID_DATA_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "auth.h"
#include "io.h"
#include "storage.h"

enum gzochid_data_managed_reference_state
  {
    GZOCHID_MANAGED_REFERENCE_STATE_NEW,
    GZOCHID_MANAGED_REFERENCE_STATE_EMPTY,
    GZOCHID_MANAGED_REFERENCE_STATE_NOT_MODIFIED,
    GZOCHID_MANAGED_REFERENCE_STATE_MAYBE_MODIFIED,
    GZOCHID_MANAGED_REFERENCE_STATE_MODIFIED,
    GZOCHID_MANAGED_REFERENCE_STATE_FLUSHED,
    GZOCHID_MANAGED_REFERENCE_STATE_REMOVED_EMPTY,
    GZOCHID_MANAGED_REFERENCE_STATE_REMOVED_FETCHED
  };

typedef struct _gzochid_data_worker_serialization
{
  void (*serializer) 
  (gzochid_application_context *, gzochid_application_worker, GString *);
  
  gzochid_application_worker (*deserializer) 
  (gzochid_application_context *, GString *);
} gzochid_data_worker_serialization;

typedef struct _gzochid_data_managed_reference
{
  gzochid_application_context *context;
  enum gzochid_data_managed_reference_state state;
  gzochid_io_serialization *serialization;

  mpz_t oid;
  void *obj;
} gzochid_data_managed_reference;

typedef struct _gzochid_data_oid_block
{
  mpz_t first;
  mpz_t next;
  mpz_t last;  
} gzochid_data_oid_block;

typedef struct _gzochid_data_transaction_context
{
  gzochid_application_context *context;

  gzochid_data_oid_block *free_oids;

  gzochid_storage_transaction *oids_transaction;
  gzochid_storage_transaction *names_transaction;

  GHashTable *oids_to_references;
  GHashTable *ptrs_to_references;

} gzochid_data_transaction_context;

typedef struct _gzochid_data_managed_reference_holder
{
  gzochid_application_context *context;
  gzochid_data_managed_reference *reference;
  gzochid_io_serialization *serialization;
  void *data;
} gzochid_data_managed_reference_holder;

void *gzochid_data_get_binding 
(gzochid_application_context *, char *, gzochid_io_serialization *);
void gzochid_data_set_binding
(gzochid_application_context *, char *, gzochid_io_serialization *, void *);
void gzochid_data_remove_binding (gzochid_application_context *, char *);
gzochid_data_managed_reference *gzochid_data_create_reference
(gzochid_application_context *, gzochid_io_serialization *, void *);
gzochid_data_managed_reference *gzochid_data_create_reference_sync
(gzochid_application_context *, gzochid_auth_identity *, 
 gzochid_io_serialization *, void *);
gzochid_data_managed_reference *gzochid_data_create_reference_to_oid
(gzochid_application_context *, gzochid_io_serialization *, mpz_t);
void gzochid_data_dereference (gzochid_data_managed_reference *reference);

void gzochid_data_mark (gzochid_application_context *, void *);

#endif /* GZOCHID_DATA_H */
