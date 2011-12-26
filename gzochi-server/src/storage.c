/* storage.c: Database storage routines for gzochid
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

#include <assert.h>
#include <gdbm.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "storage.h"

#define DEFAULT_BLOCK_SIZE 512

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

gzochid_storage_store *gzochid_storage_open (char *filename)
{
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));

  store->mutex = g_mutex_new ();
  store->lock_table_mutex = g_mutex_new ();
  store->lock_table = g_hash_table_new (g_datum_hash, g_datum_equal);
  store->database = gdbm_open 
    (filename, DEFAULT_BLOCK_SIZE, GDBM_WRCREAT, 420, NULL);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  g_mutex_free (store->mutex);
  g_mutex_free (store->lock_table_mutex);
  g_hash_table_destroy (store->lock_table);
  gdbm_close (store->database);
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
(gzochid_storage_store *store, char *key, int key_len, int *len)
{
  datum dkey, dvalue;

  dkey.dptr = key;
  dkey.dsize = key_len;

  dvalue = gdbm_fetch (store->database, dkey);

  if (len != NULL)
    *len = dvalue.dsize;

  return dvalue.dptr;
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, int key_len, char *data, int data_len)
{
  datum dkey, ddata;

  dkey.dptr = key;
  dkey.dsize = key_len;

  ddata.dptr = data;
  ddata.dsize = data_len;

  assert (gdbm_store (store->database, dkey, ddata, GDBM_REPLACE) == 0);
  gdbm_sync (store->database);
}

void gzochid_storage_delete 
(gzochid_storage_store *store, char *key, int key_len)
{
  datum dkey;

  dkey.dptr = key;
  dkey.dsize = key_len;

  assert (gdbm_delete (store->database, dkey) == 0);
}

char *gzochid_storage_first_key (gzochid_storage_store *store, int *len)
{
  datum dkey = gdbm_firstkey (store->database);
  
  if (len != NULL)
    *len = dkey.dsize;

  return dkey.dptr;
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, int key_len, int *len)
{
  datum next, best_match = { NULL, 0 };

  next = gdbm_firstkey (store->database);
  while (next.dptr != NULL)
    {
      if (strncmp (key, next.dptr, MIN (key_len, next.dsize)) < 0)
	{
	  if (best_match.dptr == NULL)
	    best_match = next;
	  else if (strncmp (next.dptr, best_match.dptr, 
			    MIN (next.dsize, best_match.dsize)) > 0)
	    {
	      free (best_match.dptr);
	      best_match = next;
	    }
	}

      next = gdbm_nextkey (store->database, next);
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

  transaction->store = store;
  transaction->cache = g_hash_table_new (g_datum_hash, g_datum_equal);

  return transaction;
}

static void cleanup_transaction (gzochid_storage_transaction *tx)
{
  g_hash_table_destroy (tx->cache);
  g_list_free_full (tx->operations, free);
  free (tx);
}

static void commit_operation (gpointer data, gpointer user_data)
{
  datum key, value;
  gzochid_storage_operation *op = (gzochid_storage_operation *) data;
  gzochid_storage_operation_put *put = NULL;
  gzochid_storage_transaction *tx = (gzochid_storage_transaction *) user_data;

  key.dptr = op->key;
  key.dsize = op->key_len;

  switch (op->type)
    {
    case GZOCHID_STORAGE_OPERATION_PUT:

      put = (gzochid_storage_operation_put *) op;
      value.dptr = put->value;
      value.dsize = put->value_len;
      assert (gdbm_store (tx->store->database, key, value, GDBM_REPLACE) == 0);
      
      break;		  
    case GZOCHID_STORAGE_OPERATION_DELETE: 
      gdbm_delete (tx->store->database, key);
    default: break;
    }
}

void gzochid_storage_transaction_commit (gzochid_storage_transaction *tx)
{
  g_list_foreach (tx->operations, commit_operation, tx);
  gdbm_sync (tx->store->database);
  cleanup_transaction (tx);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  cleanup_transaction (tx);
}

void gzochid_storage_transaction_check (gzochid_storage_transaction *tx)
{
}

static void set_read_lock (gzochid_storage_transaction *tx, datum *key)
{
  g_mutex_lock (tx->store->lock_table_mutex);
  g_mutex_unlock (tx->store->lock_table_mutex);
}

static void set_write_lock (gzochid_storage_transaction *tx, datum *key)
{
  g_mutex_lock (tx->store->lock_table_mutex);
  g_mutex_unlock (tx->store->lock_table_mutex);
}

static datum *make_key (char *key, int key_len)
{
  datum *d = malloc (sizeof (datum));

  d->dptr = malloc (sizeof (char) * key_len);
  memcpy (d->dptr, key, key_len);
  d->dsize = key_len;

  return d;
}

static extended_datum *make_extended_key (char *key, int key_len)
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

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, char *key, int key_len, int *len)
{
  datum *value = NULL;
  datum *k = make_key (key, key_len);
  char *ret = NULL;

  set_read_lock (tx, k);
  value = g_hash_table_lookup (tx->cache, k);

  if (value == NULL)
    { 
      datum db_value = gdbm_fetch (tx->store->database, *k);

      if (db_value.dptr != NULL)
	value = (datum *) make_extended_key (db_value.dptr, db_value.dsize);
      else value = (datum *) make_extended_key (NULL, 0);
	  
      g_hash_table_insert (tx->cache, k, value);
      free (db_value.dptr);
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

void gzochid_storage_transaction_put
(gzochid_storage_transaction *tx, char *key, int key_len, char *data, 
 int data_len)
{
  datum *k = make_key (key, key_len);
  datum *v = (datum *) make_extended_key (data, data_len);
  
  set_write_lock (tx, k);
  g_hash_table_insert (tx->cache, k, v);  
  tx->operations = g_list_append (tx->operations, make_put (k, v));
}

void gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, char *key, int key_len)
{
  datum *k = make_key (key, key_len);
  datum *v = (datum *) make_extended_key (NULL, 0);
  
  set_write_lock (tx, k);
  g_hash_table_insert (tx->cache, k, v);
  tx->operations = g_list_append (tx->operations, make_delete (k));
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, int *len)
{
  datum dkey = gdbm_firstkey (tx->store->database);

  if (dkey.dptr != NULL)
    {
      set_read_lock (tx, make_key (dkey.dptr, dkey.dsize));
      if (len != NULL)
	*len = dkey.dsize;
      return dkey.dptr;
    }
  else return NULL;
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, char *key, int key_len, int *len)
{
  datum next, best_match = { NULL, 0 };

  next = gdbm_firstkey (tx->store->database);
  while (next.dptr != NULL)
    {      
      extended_datum *value = g_hash_table_lookup (tx->cache, &next);
      if (value != NULL && value->null)
	{
	  datum next_next = gdbm_nextkey (tx->store->database, next);
	  free (next.dptr);
	  next = next_next;
	  continue;
	}

      if (strncmp (key, next.dptr, MIN (key_len, next.dsize)) < 0)
	{
	  if (best_match.dptr == NULL)
	    best_match = next;
	  else if (strncmp (next.dptr, best_match.dptr, 
			    MIN (next.dsize, best_match.dsize)) > 0)
	    {
	      free (best_match.dptr);
	      best_match = next;
	    }
	}

      next = gdbm_nextkey (tx->store->database, next);
    }
  
  if (best_match.dptr == NULL)
    return NULL;

  set_read_lock (tx, make_key (best_match.dptr, best_match.dsize));

  if (len != NULL)
    *len = best_match.dsize;

  return best_match.dptr;
}
