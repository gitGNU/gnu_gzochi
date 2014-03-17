/* storage-lmdb.c: Database storage routines for gzochid (Symas Lightning DB)
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

#include <assert.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libgen.h>
#include <lmdb.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "log.h"
#include "storage.h"

static gboolean retryable (int ret)
{
  return ret == MDB_BAD_TXN || ret == MDB_TXN_FULL;
}

typedef struct _gzochid_storage_lmdb_context
{
  gzochid_storage_context base;
  pthread_key_t tx_chain;
} gzochid_storage_lmdb_context;

gzochid_storage_context *gzochid_storage_initialize (char *path)
{
  MDB_env *db_env = NULL;
  gzochid_storage_lmdb_context *lmdb_context = 
    calloc (1, sizeof (gzochid_storage_lmdb_context));
  gzochid_storage_context *context = (gzochid_storage_context *) lmdb_context;

  int ret = 0;

  if (g_file_test (path, G_FILE_TEST_EXISTS))
    {
      if (!g_file_test (path, G_FILE_TEST_IS_DIR))
	{
	  gzochid_err ("%s is not a directory.", path);
	  return NULL;
	}
    }
  else 
    {
      gzochid_notice
	("LMMDB data directory %s does not exist; creating...", path);
      if (g_mkdir (path, 493) != 0)
	{
	  gzochid_err ("Unable to create LMDB data directory %s.", path);
	  return NULL;
	}
    }

  assert (mdb_env_create (&db_env) == 0);
  assert (mdb_env_set_maxdbs (db_env, 3) == 0);
  ret = mdb_env_open (db_env, path, MDB_NOSYNC, 0644);

  if (ret != 0)
    {
      gzochid_err 
	("Unable to open LMDB environment in %s: %s", path, mdb_strerror (ret));
      return NULL;
    }

  context->environment = db_env;
  pthread_key_create (&lmdb_context->tx_chain, NULL);

  return context;
}

gzochid_storage_store *gzochid_storage_open 
(gzochid_storage_context *context, char *path)
{
  MDB_txn *dbopen_tx = NULL;
  MDB_dbi *db = malloc (sizeof (MDB_dbi));
  MDB_env *db_env = (MDB_env *) context->environment;

  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));
  int ret = 0;

  assert (mdb_txn_begin (db_env, NULL, 0, &dbopen_tx) == 0);
  ret = mdb_dbi_open (dbopen_tx, path, MDB_CREATE, db);
  if (ret != 0)
    {
      gzochid_err
	("Unable to open LMDB database in %s: %s", path, mdb_strerror (ret));
      mdb_txn_abort (dbopen_tx);
      return NULL;
    }
  assert (mdb_txn_commit (dbopen_tx) == 0);

  store->database = db;
  store->context = context;
  g_mutex_init (&store->mutex);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  MDB_dbi *db = (MDB_dbi *) store->database;

  g_mutex_clear (&store->mutex);
  mdb_dbi_close ((MDB_env *) store->context->environment, *db);
  free (store);
}

void gzochid_storage_lock (gzochid_storage_store *store)
{
  g_mutex_lock (&store->mutex);
}

void gzochid_storage_unlock (gzochid_storage_store *store)
{
  g_mutex_unlock (&store->mutex);
}

char *gzochid_storage_get 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  char *ret = NULL;
  gzochid_storage_transaction *tx = 
    gzochid_storage_transaction_begin (store->context);
  ret = gzochid_storage_transaction_get (tx, store, key, key_len, len);
  gzochid_storage_transaction_commit (tx);
  
  return ret;
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  gzochid_storage_transaction *tx = 
    gzochid_storage_transaction_begin (store->context);
  gzochid_storage_transaction_put (tx, store, key, key_len, data, data_len);
  gzochid_storage_transaction_commit (tx);
}

int gzochid_storage_delete 
(gzochid_storage_store *store, char *key, size_t key_len)
{
  gzochid_storage_transaction *tx = 
    gzochid_storage_transaction_begin (store->context);
  int ret = gzochid_storage_transaction_delete (tx, store, key, key_len);
  gzochid_storage_transaction_commit (tx);
  return ret;
}

char *gzochid_storage_first_key (gzochid_storage_store *store, size_t *len)
{
  gzochid_storage_transaction *tx = 
    gzochid_storage_transaction_begin (store->context);
  char *ret = gzochid_storage_transaction_first_key (tx, store, len);
  gzochid_storage_transaction_commit (tx);
  return ret;
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  gzochid_storage_transaction *tx = 
    gzochid_storage_transaction_begin (store->context);
  char *ret = gzochid_storage_transaction_next_key 
    (tx, store, key, key_len, len);
  gzochid_storage_transaction_commit (tx);
  return ret;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_context *context)
{
  MDB_env *db_env = (MDB_env *) context->environment;
  gzochid_storage_lmdb_context *lmdb_context = 
    (gzochid_storage_lmdb_context *) context;
  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  GList *parents = (GList *) pthread_getspecific (lmdb_context->tx_chain);
  MDB_txn *parent = parents == NULL ? NULL : (MDB_txn *) parents->data;
  MDB_txn *txn = NULL;

  assert (mdb_txn_begin (db_env, parent, 0, &txn) == 0);

  parents = g_list_prepend (parents, txn);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  transaction->context = context;
  transaction->txn = txn;

  return transaction;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin_timed
(gzochid_storage_context *context, struct timeval timeout)
{
  return gzochid_storage_transaction_begin (context);
}

void gzochid_storage_transaction_commit (gzochid_storage_transaction *tx)
{
  MDB_txn *txn = (MDB_txn *) tx->txn;
  gzochid_storage_lmdb_context *lmdb_context =
    (gzochid_storage_lmdb_context *) tx->context;
  GList *parents = (GList *) pthread_getspecific (lmdb_context->tx_chain);

  assert (mdb_txn_commit (txn) == 0);

  parents = g_list_delete_link (parents, parents);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  free (tx);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  MDB_txn *txn = (MDB_txn *) tx->txn;
  gzochid_storage_lmdb_context *lmdb_context =
    (gzochid_storage_lmdb_context *) tx->context;
  GList *parents = (GList *) pthread_getspecific (lmdb_context->tx_chain);

  mdb_txn_abort (txn);

  parents = g_list_delete_link (parents, parents);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  free (tx);
}

void gzochid_storage_transaction_prepare (gzochid_storage_transaction *tx)
{
}

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  MDB_dbi *db = (MDB_dbi *) store->database;
  MDB_txn *txn = (MDB_txn *) tx->txn;
  MDB_val db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (MDB_val));
  memset (&db_data, 0, sizeof (MDB_val));

  db_key.mv_data = key;
  db_key.mv_size = key_len;

  ret = mdb_get (txn, *db, &db_key, &db_data);

  if (ret == 0)
    {
      char *data = NULL;
      if (db_data.mv_data != NULL)
	{
	  if (len != NULL)
	    *len = db_data.mv_size;
	  data = malloc (sizeof (char) * db_data.mv_size);
	  data = memcpy (data, db_data.mv_data, db_data.mv_size);
	}
      
      return data;
    }
  else 
    {
      if (ret != MDB_NOTFOUND)
	{
	  gzochid_warning 
	    ("Failed to retrieve key %s in transaction: %s", 
	     key, mdb_strerror (ret)); 
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}

char *gzochid_storage_transaction_get_for_update
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  return gzochid_storage_transaction_get (tx, store, key, key_len, len);
}

void gzochid_storage_transaction_put
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, char *data, 
 size_t data_len)
{
  MDB_dbi *db = (MDB_dbi *) store->database;
  MDB_txn *txn = (MDB_txn *) tx->txn;
  MDB_val db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (MDB_val));
  memset (&db_data, 0, sizeof (MDB_val));

  db_key.mv_data = key;
  db_key.mv_size = key_len;

  db_data.mv_data = data;
  db_data.mv_size = data_len;

  ret = mdb_put (txn, *db, &db_key, &db_data, 0);

  if (ret != 0)
    {
      gzochid_warning 
	("Failed to store key %s in transaction: %s", key, mdb_strerror (ret)); 
      tx->rollback = TRUE;
      tx->should_retry = retryable (ret);
    }
}

int gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len)
{
  MDB_dbi *db = (MDB_dbi *) store->database;
  MDB_txn *txn = (MDB_txn *) tx->txn;
  MDB_val db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (MDB_val));
  db_key.mv_data = key;
  db_key.mv_size = key_len;

  ret = mdb_del (txn, *db, &db_key, 0);

  if (ret != 0)
    {
      gzochid_warning
	("Failed to delete key %s in transaction: %s", key, 
	 mdb_strerror (ret)); 
      
      if (ret == MDB_NOTFOUND)
	return GZOCHID_STORAGE_ENOTFOUND;
      else
	{
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	  return GZOCHID_STORAGE_ETXFAILURE;
	}
    }
  else return 0;
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, size_t *len)
{
  MDB_dbi *db = (MDB_dbi *) store->database;
  MDB_txn *txn = (MDB_txn *) tx->txn;
  MDB_cursor *cursor = NULL;
  MDB_val db_key, db_value;
  int ret = 0;

  assert (mdb_cursor_open (txn, *db, &cursor) == 0);

  memset (&db_key, 0, sizeof (MDB_val));
  memset (&db_value, 0, sizeof (MDB_val));

  ret = mdb_cursor_get (cursor, &db_key, &db_value, MDB_FIRST);
  mdb_cursor_close (cursor);

  if (ret != 0)
    {
      gzochid_warning
	("Failed to seek to first key in transaction: %s", mdb_strerror (ret));
      tx->rollback = TRUE;
      tx->should_retry = retryable (ret);

      return NULL;
    }
  else
    {
      char *data = NULL;
      if (db_value.mv_data != NULL)
	{
	  if (len != NULL)
	    *len = db_value.mv_size;
	  data = malloc (sizeof (char) * db_value.mv_size);
	  data = memcpy (data, db_value.mv_data, db_value.mv_size);
	}
      
      return data;
    }
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  MDB_dbi *db = (MDB_dbi *) store->database;
  MDB_txn *txn = (MDB_txn *) tx->txn;
  MDB_cursor *cursor = NULL;
  MDB_val db_key, db_value;
  int ret = 0;

  assert (mdb_cursor_open (txn, *db, &cursor) == 0);

  memset (&db_key, 0, sizeof (MDB_val));
  memset (&db_value, 0, sizeof (MDB_val));

  db_key.mv_data = key;
  db_key.mv_size = key_len;

  ret = mdb_cursor_get (cursor, &db_key, &db_value, MDB_SET_RANGE);

  if (ret == 0 && memcmp (db_key.mv_data, key, key_len) == 0)
    ret = mdb_cursor_get (cursor, &db_key, &db_value, MDB_NEXT);

  mdb_cursor_close (cursor);

  if (ret == 0)
    {
      char *data = NULL;
      if (db_value.mv_data != NULL)
	{
	  if (len != NULL)
	    *len = db_value.mv_size;
	  data = malloc (sizeof (char) * db_value.mv_size);
	  data = memcpy (data, db_value.mv_data, db_value.mv_size);
	}
      
      return data;
    }
  else
    {
      if (ret != MDB_NOTFOUND)
	{
	  gzochid_warning
	    ("Failed to advance cursor in transaction: %s", mdb_strerror (ret));
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}
