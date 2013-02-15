/* storage.h: Prototypes and declarations for storage.c
 * Copyright (C) 2013 Julian Graham
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

typedef struct _gzochid_storage_store
{
  GMutex *mutex;
  gpointer database;
} gzochid_storage_store;

typedef struct _gzochid_storage_transaction
{
  gzochid_storage_store *store;
  gpointer txn;
  gboolean rollback;
} gzochid_storage_transaction;

gzochid_storage_store *gzochid_storage_open (char *);
void gzochid_storage_close (gzochid_storage_store *);
void gzochid_storage_lock (gzochid_storage_store *);
void gzochid_storage_unlock (gzochid_storage_store *);

char *gzochid_storage_get (gzochid_storage_store *, char *, size_t, size_t *);
void gzochid_storage_put 
(gzochid_storage_store *, char *, size_t, char *, size_t);
void gzochid_storage_delete (gzochid_storage_store *, char *, size_t);
char *gzochid_storage_first_key (gzochid_storage_store *, size_t *);
char *gzochid_storage_next_key 
(gzochid_storage_store *, char *, size_t, size_t *);

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_store *);
gzochid_storage_transaction *gzochid_storage_transaction_begin_timed
(gzochid_storage_store *, struct timeval);
void gzochid_storage_transaction_commit (gzochid_storage_transaction *);
void gzochid_storage_transaction_rollback (gzochid_storage_transaction *);
void gzochid_storage_transaction_prepare (gzochid_storage_transaction *);
char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *, char *, size_t, size_t *);
char *gzochid_storage_transaction_get_for_update 
(gzochid_storage_transaction *, char *, size_t, size_t *);
void gzochid_storage_transaction_put 
(gzochid_storage_transaction *, char *, size_t, char *, size_t);
void gzochid_storage_transaction_delete 
(gzochid_storage_transaction *, char *, size_t);
char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *, size_t *);
char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *, char *, size_t, size_t *);

#endif /* GZOCHID_STORAGE_H */
