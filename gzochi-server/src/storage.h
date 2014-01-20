/* storage.h: Prototypes and declarations for storage.c
 * Copyright (C) 2014 Julian Graham
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
#include <sys/time.h>

#define GZOCHID_STORAGE_EFAILURE -1
#define GZOCHID_STORAGE_ENOTFOUND -2
#define GZOCHID_STORAGE_ETXFAILURE -3

typedef struct _gzochid_storage_context
{
  gpointer environment;
} gzochid_storage_context;

typedef struct _gzochid_storage_store
{
  GMutex mutex;
  gzochid_storage_context *context;
  gpointer database;
} gzochid_storage_store;

typedef struct _gzochid_storage_transaction
{
  gzochid_storage_context *context;
  gpointer txn;
  gboolean rollback;
  gboolean should_retry;
} gzochid_storage_transaction;

gzochid_storage_context *gzochid_storage_initialize (char *);
gzochid_storage_store *gzochid_storage_open 
(gzochid_storage_context *, char *);
void gzochid_storage_close (gzochid_storage_store *);
void gzochid_storage_lock (gzochid_storage_store *);
void gzochid_storage_unlock (gzochid_storage_store *);

char *gzochid_storage_get (gzochid_storage_store *, char *, size_t, size_t *);
void gzochid_storage_put 
(gzochid_storage_store *, char *, size_t, char *, size_t);
int gzochid_storage_delete (gzochid_storage_store *, char *, size_t);
char *gzochid_storage_first_key (gzochid_storage_store *, size_t *);
char *gzochid_storage_next_key 
(gzochid_storage_store *, char *, size_t, size_t *);

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_context *);
gzochid_storage_transaction *gzochid_storage_transaction_begin_timed
(gzochid_storage_context *, struct timeval);
void gzochid_storage_transaction_commit (gzochid_storage_transaction *);
void gzochid_storage_transaction_rollback (gzochid_storage_transaction *);
void gzochid_storage_transaction_prepare (gzochid_storage_transaction *);
char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
 size_t *);
char *gzochid_storage_transaction_get_for_update 
(gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
 size_t *);
void gzochid_storage_transaction_put 
(gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
 char *, size_t);
int gzochid_storage_transaction_delete 
(gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t);
char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *, gzochid_storage_store *, size_t *);
char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
 size_t *);

#endif /* GZOCHID_STORAGE_H */
