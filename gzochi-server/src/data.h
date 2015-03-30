/* data.h: Prototypes and declarations for data.c
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

#ifndef GZOCHID_DATA_H
#define GZOCHID_DATA_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "gzochid-auth.h"
#include "gzochid-storage.h"
#include "io.h"

#ifdef __GNUC__
#define WARN_UNUSED_RESULT __attribute__ ((warn_unused_result))
#else
#define WARN_UNUSED_RESULT
#endif /* __GNUC__ */

#define GZOCHID_DATA_ERROR gzochid_data_error_quark ()

GQuark gzochid_data_error_quark (void);

typedef enum
  {
    GZOCHID_DATA_ERROR_NOT_FOUND,
    GZOCHID_DATA_ERROR_TRANSACTION,
    GZOCHID_DATA_ERROR_FAILED
  }
  GzochidDataError;

struct _gzochid_oid_holder
{
  mpz_t oid;
};

typedef struct _gzochid_oid_holder gzochid_oid_holder;

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

struct _gzochid_data_managed_reference
{
  gzochid_application_context *context;
  enum gzochid_data_managed_reference_state state;
  gzochid_io_serialization *serialization;

  mpz_t oid;
  void *obj;
};

typedef struct _gzochid_data_managed_reference gzochid_data_managed_reference;

struct _gzochid_data_oid_block
{
  mpz_t first;
  mpz_t next;
  mpz_t last;  
};

typedef struct _gzochid_data_oid_block gzochid_data_oid_block;

struct _gzochid_data_transaction_context
{
  gzochid_application_context *context;

  gzochid_data_oid_block *free_oids;
  GList *used_oid_blocks;

  gzochid_storage_transaction *transaction;

  GHashTable *oids_to_references;
  GHashTable *ptrs_to_references;

};

typedef struct _gzochid_data_transaction_context
gzochid_data_transaction_context;

struct _gzochid_data_managed_reference_holder
{
  gzochid_application_context *context;
  gzochid_data_managed_reference *reference;
  gzochid_io_serialization *serialization;
  void *data;
};

typedef struct _gzochid_data_managed_reference_holder
gzochid_data_managed_reference_holder;

gzochid_oid_holder *gzochid_oid_holder_new (void);
void gzochid_oid_holder_free (gzochid_oid_holder *);

void *gzochid_data_get_binding 
(gzochid_application_context *, char *, gzochid_io_serialization *, GError **);

void gzochid_data_set_binding
(gzochid_application_context *, char *, gzochid_io_serialization *, void *,
 GError **);

void gzochid_data_set_binding_to_oid
(gzochid_application_context *, char *, mpz_t, GError **);

void gzochid_data_remove_binding 
(gzochid_application_context *, char *, GError **);

gboolean gzochid_data_binding_exists 
(gzochid_application_context *, char *, GError **);
char *gzochid_data_next_binding_oid 
(gzochid_application_context *, char *, mpz_t, GError **);

gzochid_data_managed_reference *gzochid_data_create_reference
(gzochid_application_context *, gzochid_io_serialization *, void *);
gzochid_data_managed_reference *gzochid_data_create_reference_to_oid
(gzochid_application_context *, gzochid_io_serialization *, mpz_t);

void gzochid_data_dereference 
(gzochid_data_managed_reference *reference, GError **);
void gzochid_data_remove_object (gzochid_data_managed_reference *, GError **);
void gzochid_data_mark 
(gzochid_application_context *, gzochid_io_serialization *, void *, GError **);

#endif /* GZOCHID_DATA_H */
