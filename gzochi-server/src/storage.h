/* storage.h: Prototypes and declarations for storage.c
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

#ifndef GZOCHID_STORAGE_H
#define GZOCHID_STORAGE_H

#include <gdbm.h>
#include <glib.h>
#include <gmp.h>

enum gzochid_storage_operation_type
  {
    GZOCHID_STORAGE_OPERATION_PUT,
    GZOCHID_STORAGE_OPERATION_DELETE
  };

enum gzochid_storage_lock_type
  {
    GZOCHID_STORAGE_LOCK_READ,
    GZOCHID_STORAGE_LOCK_WRITE
  };

typedef struct _gzochid_storage_store
{
  GMutex *mutex;

  GMutex *lock_table_mutex;
  GHashTable *lock_table;

  GDBM_FILE database;
} gzochid_storage_store;

typedef struct _gzochid_storage_transaction
{
  gzochid_storage_store *store;
  GHashTable *cache;
  GList *operations;
  gboolean rollback;
} gzochid_storage_transaction;

typedef struct _gzochid_storage_data_lock
{
  enum gzochid_storage_lock_type type;
  gzochid_storage_transaction *transaction;
} gzochid_storage_data_lock;

typedef struct _gzochid_storage_operation
{
  char *key;
  int key_len;
  enum gzochid_storage_operation_type type;
} gzochid_storage_operation;

typedef struct _gzochid_storage_operation_put
{
  gzochid_storage_operation base;
  char *value;
  int value_len;
} gzochid_storage_operation_put;

gzochid_storage_store *gzochid_storage_open (char *);
void gzochid_storage_close (gzochid_storage_store *);
void gzochid_storage_lock (gzochid_storage_store *);
void gzochid_storage_unlock (gzochid_storage_store *);

char *gzochid_storage_get (gzochid_storage_store *, char *, int, int *);
void gzochid_storage_put (gzochid_storage_store *, char *, int, char *, int);
void gzochid_storage_delete (gzochid_storage_store *, char *, int);

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_store *);
void gzochid_storage_transaction_commit (gzochid_storage_transaction *);
void gzochid_storage_transaction_rollback (gzochid_storage_transaction *);
void gzochid_storage_transaction_check (gzochid_storage_transaction *);
char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *, char *, int, int *);
void gzochid_storage_transaction_put 
(gzochid_storage_transaction *, char *, int, char *, int);
void gzochid_storage_transaction_delete 
(gzochid_storage_transaction *, char *, int);

#endif /* GZOCHID_STORAGE_H */
