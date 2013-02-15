/* storage-gdbm.c: Database storage routines for gzochid (GDBM)
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

#include <assert.h>
#include <gdbm.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "storage.h"

#define DEFAULT_BLOCK_SIZE 512

enum gzochid_storage_lock_type
  {
    GZOCHID_STORAGE_LOCK_READ,
    GZOCHID_STORAGE_LOCK_WRITE
  };

enum gzochid_storage_operation_type
  {
    GZOCHID_STORAGE_OPERATION_PUT,
    GZOCHID_STORAGE_OPERATION_DELETE
  };

typedef struct _gdbm_context 
{
  GDBM_FILE dbf;
  
  GMutex *lock_table_mutex;
  GHashTable *lock_table;
} gdbm_context;

typedef struct _gdbm_transaction_context
{
  GHashTable *cache;
  GList *operations;
} gdbm_transaction_context;

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

typedef struct _extended_datum
{
  datum base;
  gboolean null;
} extended_datum;

guint g_datum_hash (gconstpointer v)
{
  int idx;
  datum *d = (datum *) v;
  guint32 h = 5381;

  for (idx = 0; idx < d->dsize; idx++)
    h = (h << 5) + h + d->dptr[idx];

  return h;
}

gboolean g_datum_equal (gconstpointer v1, gconstpointer v2)
{
  const datum *d1 = v1;
  const datum *d2 = v2;

  return d1->dsize == d2->dsize && memcmp (d1->dptr, d2->dptr, d1->dsize) == 0;
}

gzochid_storage_store *gzochid_storage_open (char *basename)
{
  int basename_len = strlen (basename);
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));
  gdbm_context *context = malloc (sizeof (gdbm_context));

  char *filename = malloc (sizeof (char) * (basename_len + 4));
  
  filename = strncpy (filename, basename, basename_len + 1);
  filename = strncat (filename, ".db", 3);

  context->lock_table_mutex = g_mutex_new ();
  context->lock_table = g_hash_table_new (g_datum_hash, g_datum_equal);
  context->dbf = gdbm_open 
    (filename, DEFAULT_BLOCK_SIZE, GDBM_WRCREAT, 420, NULL);

  store->mutex = g_mutex_new ();
  store->database = context;

  free (filename);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  gdbm_context *context = (gdbm_context *) store->database;
  
  g_mutex_free (context->lock_table_mutex);
  g_hash_table_destroy (context->lock_table);
  gdbm_close (context->dbf);

  g_mutex_free (store->mutex);
  free (store->database);
  free (store);
}

void gzochid_storage_lock (gzochid_storage_store *store)
{
  g_mutex_lock (store->mutex);
}

void gzochid_storage_unlock (gzochid_storage_store *store)
{
  g_mutex_unlock (store->mutex);
}

char *gzochid_storage_get 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  gdbm_context *context = (gdbm_context *) store->database;
  datum dkey, dvalue;

  dkey.dptr = key;
  dkey.dsize = key_len;

  dvalue = gdbm_fetch (context->dbf, dkey);

  if (len != NULL)
    *len = dvalue.dsize;

  return dvalue.dptr;
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  gdbm_context *context = (gdbm_context *) store->database;
  datum dkey, ddata;

  dkey.dptr = key;
  dkey.dsize = key_len;

  ddata.dptr = data;
  ddata.dsize = data_len;

  assert (gdbm_store (context->dbf, dkey, ddata, GDBM_REPLACE) == 0);
  gdbm_sync (context->dbf);
}

void gzochid_storage_delete 
(gzochid_storage_store *store, char *key, size_t key_len)
{
  gdbm_context *context = (gdbm_context *) store->database;
  datum dkey;

  dkey.dptr = key;
  dkey.dsize = key_len;

  assert (gdbm_delete (context->dbf, dkey) == 0);
}

char *gzochid_storage_first_key (gzochid_storage_store *store, size_t *len)
{
  char z = '\0';
  return gzochid_storage_next_key (store, &z, 1, len);
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  gdbm_context *context = (gdbm_context *) store->database;
  datum next, best_match = { NULL, 0 };

  next = gdbm_firstkey (context->dbf);
  while (next.dptr != NULL)
    {
      if (strncmp (key, next.dptr, MIN (key_len, next.dsize)) < 0)
	{
	  if (best_match.dptr == NULL)
	    best_match = next;
	  else if (strncmp (next.dptr, best_match.dptr, 
			    MIN (next.dsize, best_match.dsize)) < 0)
	    {
	      free (best_match.dptr);
	      best_match = next;
	    }
	}

      next = gdbm_nextkey (context->dbf, next);
    }
  
  if (best_match.dptr == NULL)
    return NULL;

  if (len != NULL)
    *len = best_match.dsize;

  return best_match.dptr;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_store *store)
{
  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  gdbm_transaction_context *txn = 
    calloc (sizeof (gdbm_transaction_context), 1);

  txn->cache = g_hash_table_new (g_datum_hash, g_datum_equal);

  transaction->store = store;
  transaction->txn = txn;

  return transaction;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin_timed
(gzochid_storage_store *store, struct timeval timeout)
{
  return gzochid_storage_transaction_begin (store);
}

static void cleanup_transaction (gzochid_storage_transaction *tx)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;

  g_hash_table_destroy (txn->cache);
  g_list_free_full (txn->operations, free);

  free (txn);
  free (tx);
}

static void commit_operation (gpointer data, gpointer user_data)
{
  datum key, value;
  gzochid_storage_operation *op = (gzochid_storage_operation *) data;
  gzochid_storage_operation_put *put = NULL;
  gzochid_storage_transaction *tx = (gzochid_storage_transaction *) user_data;
  gdbm_context *context = (gdbm_context *) tx->store->database;

  key.dptr = op->key;
  key.dsize = op->key_len;

  switch (op->type)
    {
    case GZOCHID_STORAGE_OPERATION_PUT:

      put = (gzochid_storage_operation_put *) op;
      value.dptr = put->value;
      value.dsize = put->value_len;
      assert (gdbm_store (context->dbf, key, value, GDBM_REPLACE) == 0);
      
      break;		  
    case GZOCHID_STORAGE_OPERATION_DELETE: 
      gdbm_delete (context->dbf, key);
    default: break;
    }
}

void gzochid_storage_transaction_commit (gzochid_storage_transaction *tx)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;
  gdbm_context *context = (gdbm_context *) tx->store->database;

  g_list_foreach (txn->operations, commit_operation, tx);
  gdbm_sync (context->dbf);
  cleanup_transaction (tx);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  cleanup_transaction (tx);
}

void gzochid_storage_transaction_prepare (gzochid_storage_transaction *tx)
{
}

static void set_read_lock (gzochid_storage_transaction *tx, datum *key)
{
  gdbm_context *context = (gdbm_context *) tx->store->database;

  g_mutex_lock (context->lock_table_mutex);
  g_mutex_unlock (context->lock_table_mutex);
}

static void set_write_lock (gzochid_storage_transaction *tx, datum *key)
{
  gdbm_context *context = (gdbm_context *) tx->store->database;

  g_mutex_lock (context->lock_table_mutex);
  g_mutex_unlock (context->lock_table_mutex);
}

static datum *make_key (char *key, size_t key_len)
{
  datum *d = malloc (sizeof (datum));

  d->dptr = malloc (sizeof (char) * key_len);
  memcpy (d->dptr, key, key_len);
  d->dsize = key_len;

  return d;
}

static extended_datum *make_extended_key (char *key, size_t key_len)
{
  extended_datum *ed = malloc (sizeof (extended_datum));
  datum *d = (datum *) ed;

  if (key_len > 0)
    {
      d->dptr = malloc (sizeof (char) * key_len);
      memcpy (d->dptr, key, key_len);
      d->dsize = key_len;
      ed->null = FALSE;
    }
  else
    {
      d->dptr = NULL;
      d->dsize = 0;
      ed->null = TRUE;
    }

  return ed;
}

static gzochid_storage_operation *make_delete (datum *key)
{
  gzochid_storage_operation *delete = 
    malloc (sizeof (gzochid_storage_operation));

  delete->key = key->dptr;
  delete->key_len = key->dsize;
  delete->type = GZOCHID_STORAGE_OPERATION_DELETE;

  return delete;
}

static gzochid_storage_operation *make_put (datum *key, datum *value)
{
  gzochid_storage_operation_put *put = 
    malloc (sizeof (gzochid_storage_operation_put));
  gzochid_storage_operation *op = (gzochid_storage_operation *) put;

  op->key = key->dptr;
  op->key_len = key->dsize;
  
  put->value = value->dptr;
  put->value_len = value->dsize;
  
  op->type = GZOCHID_STORAGE_OPERATION_PUT;

  return op;
}

static char *transaction_get_internal
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len, 
 gboolean write_lock)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;
  gdbm_context *context = (gdbm_context *) tx->store->database;

  datum *value = NULL;
  datum *k = make_key (key, key_len);
  char *ret = NULL;

  if (write_lock)
    set_write_lock (tx, k);
  else set_read_lock (tx, k);

  value = g_hash_table_lookup (txn->cache, k);

  if (value == NULL)
    { 
      datum db_value = gdbm_fetch (context->dbf, *k);

      if (db_value.dptr != NULL)
	{ 
	  value = (datum *) make_extended_key (db_value.dptr, db_value.dsize);
	  g_hash_table_insert (txn->cache, k, value);
	  free (db_value.dptr);
	}
      else g_hash_table_insert (txn->cache, k, make_extended_key (NULL, 0));
    }
  else
    {
      extended_datum *evalue = (extended_datum *) value;

      free (k);
      if (evalue->null)
	return NULL;
    }
  
  if (value == NULL)
    return NULL;

  if (len != NULL)
    *len = value->dsize;

  ret = malloc (sizeof (char) * value->dsize);
  ret = memcpy (ret, value->dptr, value->dsize);

  return ret;
}

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len)
{
  return transaction_get_internal (tx, key, key_len, len, FALSE);
}

char *gzochid_storage_transaction_get_for_update
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len)
{
  return transaction_get_internal (tx, key, key_len, len, TRUE);
}

void gzochid_storage_transaction_put
(gzochid_storage_transaction *tx, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;
  datum *k = make_key (key, key_len);
  datum *v = (datum *) make_extended_key (data, data_len);
  
  set_write_lock (tx, k);
  g_hash_table_insert (txn->cache, k, v);  
  txn->operations = g_list_append (txn->operations, make_put (k, v));
}

void gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, char *key, size_t key_len)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;
  datum *k = make_key (key, key_len);
  datum *v = (datum *) make_extended_key (NULL, 0);
  
  set_write_lock (tx, k);
  g_hash_table_insert (txn->cache, k, v);
  txn->operations = g_list_append (txn->operations, make_delete (k));
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, size_t *len)
{
  char z = '\0';
  return gzochid_storage_transaction_next_key (tx, &z, 1, len);
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len)
{
  gdbm_transaction_context *txn = (gdbm_transaction_context *) tx->txn;
  gdbm_context *context = (gdbm_context *) tx->store->database;
  datum next, best_match = { NULL, 0 };

  next = gdbm_firstkey (context->dbf);
  while (next.dptr != NULL)
    {      
      extended_datum *value = g_hash_table_lookup (txn->cache, &next);
      if (value != NULL && value->null)
	{
	  datum next_next = gdbm_nextkey (context->dbf, next);
	  free (next.dptr);
	  next = next_next;
	  continue;
	}

      if (strncmp (key, next.dptr, MIN (key_len, next.dsize)) < 0)
	{
	  if (best_match.dptr == NULL)
	    best_match = next;
	  else if (strncmp (next.dptr, best_match.dptr, 
			    MIN (next.dsize, best_match.dsize)) < 0)
	    {
	      free (best_match.dptr);
	      best_match = next;
	    }
	}

      next = gdbm_nextkey (context->dbf, next);
    }
  
  if (best_match.dptr == NULL)
    return NULL;

  set_read_lock (tx, make_key (best_match.dptr, best_match.dsize));

  if (len != NULL)
    *len = best_match.dsize;

  return best_match.dptr;
}
