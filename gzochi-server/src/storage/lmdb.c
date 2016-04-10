/* lmdb.c: Database storage routines for gzochid (Symas Lightning DB)
 * Copyright (C) 2016 Julian Graham
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

#include "../gzochid-storage.h"

static gboolean 
retryable (int ret)
{
  return ret == MDB_BAD_TXN || ret == MDB_TXN_FULL;
}

struct _gzochid_storage_lmdb_context
{
  gzochid_storage_context base;
  pthread_key_t tx_chain;
};

typedef struct _gzochid_storage_lmdb_context gzochid_storage_lmdb_context;

static gzochid_storage_context *
initialize (char *path)
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
	  g_warning ("%s is not a directory.", path);
	  return NULL;
	}
    }
  else 
    {
      g_info ("LMDB data directory %s does not exist; creating...", path);

      if (g_mkdir (path, 493) != 0)
	{
	  g_warning ("Unable to create LMDB data directory %s.", path);
	  return NULL;
	}
    }

  assert (mdb_env_create (&db_env) == 0);
  assert (mdb_env_set_maxdbs (db_env, 3) == 0);
  ret = mdb_env_open (db_env, path, MDB_NOSYNC, 0644);

  if (ret != 0)
    {
      g_warning 
	("Unable to open LMDB environment in %s: %s", path, mdb_strerror (ret));
      return NULL;
    }

  context->environment = db_env;
  pthread_key_create (&lmdb_context->tx_chain, NULL);

  return context;
}

static void 
close_context (gzochid_storage_context *context)
{
  MDB_env *db_env = context->environment;

  mdb_env_close (db_env);

  free (context);
}

static void
destroy_context (char *path)
{
  gchar *data = g_strconcat (path, "/data.mdb", NULL);
  gchar *lock = g_strconcat (path, "/lock.mdb", NULL);

  assert (g_remove (data) == 0);
  assert (g_remove (lock) == 0);
  assert (g_rmdir (path) == 0);

  g_free (data);
  g_free (lock);
}

static gzochid_storage_store *
open (gzochid_storage_context *context, char *path, unsigned int flags)
{
  MDB_txn *dbopen_tx = NULL;
  MDB_dbi *db = malloc (sizeof (MDB_dbi));
  MDB_env *db_env = context->environment;

  gzochid_storage_store *store = NULL;
  int ret = 0;

  if (flags & GZOCHID_STORAGE_EXCL)
    {
      assert (mdb_txn_begin (db_env, NULL, 0, &dbopen_tx) == 0);
      ret = mdb_dbi_open (dbopen_tx, path, 0, db);
      mdb_txn_abort (dbopen_tx);

      if (ret != MDB_NOTFOUND)
	{
	  free (db);

	  g_warning 
	    ("Unable to open LMDB database in %s: Database exists.", path);
	  return NULL;
	}
    }

  assert (mdb_txn_begin (db_env, NULL, 0, &dbopen_tx) == 0);
  ret = mdb_dbi_open 
    (dbopen_tx, path, flags & GZOCHID_STORAGE_CREATE ? MDB_CREATE : 0, db);
  if (ret != 0)
    {
      g_warning
	("Unable to open LMDB database in %s: %s", path, mdb_strerror (ret));
      mdb_txn_abort (dbopen_tx);
      free (db);
      return NULL;
    }
  else store = calloc (1, sizeof (gzochid_storage_store));
  assert (mdb_txn_commit (dbopen_tx) == 0);

  store->database = db;
  store->context = context;

  return store;
}

static void 
close_store (gzochid_storage_store *store)
{
  MDB_dbi *db = store->database;

  mdb_dbi_close ((MDB_env *) store->context->environment, *db);
  free (store);
}

static void
destroy_store (gzochid_storage_context *context, char *path)
{
  MDB_env *db_env = context->environment;
  MDB_dbi *db = malloc (sizeof (MDB_dbi));
  MDB_txn *txn = NULL;

  assert (mdb_txn_begin (db_env, NULL, 0, &txn) == 0);
  assert (mdb_dbi_open (txn, path, 0, db) == 0);
  assert (mdb_drop (txn, *db, 1) == 0);
  assert (mdb_txn_commit (txn) == 0);

  free (db);
}

static gzochid_storage_transaction *
transaction_begin (gzochid_storage_context *context)
{
  MDB_env *db_env = context->environment;
  gzochid_storage_lmdb_context *lmdb_context = 
    (gzochid_storage_lmdb_context *) context;
  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  GList *parents = pthread_getspecific (lmdb_context->tx_chain);
  MDB_txn *parent = parents == NULL ? NULL : parents->data;
  MDB_txn *txn = NULL;

  assert (mdb_txn_begin (db_env, parent, 0, &txn) == 0);

  parents = g_list_prepend (parents, txn);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  transaction->context = context;
  transaction->txn = txn;

  return transaction;
}

static gzochid_storage_transaction *
transaction_begin_timed (gzochid_storage_context *context, 
			 struct timeval timeout)
{
  return transaction_begin (context);
}

static void 
transaction_commit (gzochid_storage_transaction *tx)
{
  MDB_txn *txn = tx->txn;
  gzochid_storage_lmdb_context *lmdb_context = 
    (gzochid_storage_lmdb_context *) tx->context;
  GList *parents = pthread_getspecific (lmdb_context->tx_chain);

  assert (mdb_txn_commit (txn) == 0);

  parents = g_list_delete_link (parents, parents);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  free (tx);
}

static void 
transaction_rollback (gzochid_storage_transaction *tx)
{
  MDB_txn *txn = tx->txn;
  gzochid_storage_lmdb_context *lmdb_context =
    (gzochid_storage_lmdb_context *) tx->context;
  GList *parents = pthread_getspecific (lmdb_context->tx_chain);

  mdb_txn_abort (txn);

  parents = g_list_delete_link (parents, parents);
  pthread_setspecific (lmdb_context->tx_chain, parents);

  free (tx);
}

static void 
transaction_prepare (gzochid_storage_transaction *tx)
{
}

static char *
transaction_get (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, size_t *len)
{
  MDB_dbi *db = store->database;
  MDB_txn *txn = tx->txn;
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
	  g_warning 
	    ("Failed to retrieve key %s in transaction: %s", 
	     key, mdb_strerror (ret)); 
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}

static void 
transaction_put (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, char *data, size_t data_len)
{
  MDB_dbi *db = store->database;
  MDB_txn *txn = tx->txn;
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
      g_warning 
	("Failed to store key %s in transaction: %s", key, mdb_strerror (ret)); 
      tx->rollback = TRUE;
      tx->should_retry = retryable (ret);
    }
}

static int 
transaction_delete (gzochid_storage_transaction *tx, 
		    gzochid_storage_store *store, char *key, size_t key_len)
{
  MDB_dbi *db = store->database;
  MDB_txn *txn = tx->txn;
  MDB_val db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (MDB_val));
  db_key.mv_data = key;
  db_key.mv_size = key_len;

  ret = mdb_del (txn, *db, &db_key, 0);

  if (ret != 0)
    {
      g_warning
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

static char *
transaction_first_key (gzochid_storage_transaction *tx, 
		       gzochid_storage_store *store, size_t *len)
{
  MDB_dbi *db = store->database;
  MDB_txn *txn = tx->txn;
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
      g_warning
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

static char *
transaction_next_key (gzochid_storage_transaction *tx, 
		      gzochid_storage_store *store, char *key, size_t key_len, 
		      size_t *len)
{
  MDB_dbi *db = store->database;
  MDB_txn *txn = tx->txn;
  MDB_cursor *cursor = NULL;
  MDB_val db_key, db_value;
  int ret = 0;

  assert (mdb_cursor_open (txn, *db, &cursor) == 0);

  memset (&db_key, 0, sizeof (MDB_val));
  memset (&db_value, 0, sizeof (MDB_val));

  db_key.mv_data = key;
  db_key.mv_size = key_len;

  ret = mdb_cursor_get (cursor, &db_key, &db_value, MDB_SET_RANGE);

  if (ret == 0 
      && db_key.mv_size == key_len
      && memcmp (db_key.mv_data, key, key_len) == 0)
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
	  g_warning
	    ("Failed to advance cursor in transaction: %s", mdb_strerror (ret));
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}

static char *
transaction_get_for_update (gzochid_storage_transaction *tx, 
			    gzochid_storage_store *store, char *key, 
			    size_t key_len, size_t *len)
{
  return transaction_get (tx, store, key, key_len, len);
}

static gzochid_storage_engine_interface interface = 
  {
    "lmdb",

    initialize,
    close_context,
    destroy_context,
    open,
    close_store,
    destroy_store,
    
    transaction_begin,
    transaction_begin_timed,
    transaction_commit,
    transaction_rollback,
    transaction_prepare,
    
    transaction_get,
    transaction_get_for_update,
    transaction_put,
    transaction_delete,
    transaction_first_key,
    transaction_next_key
  };
  
GZOCHID_STORAGE_INIT_ENGINE (interface);
