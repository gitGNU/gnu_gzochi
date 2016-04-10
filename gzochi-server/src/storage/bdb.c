/* bdb.c: Database storage routines for gzochid (Berkeley DB)
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
#include <db.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libgen.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "../gzochid-storage.h"

static gboolean 
retryable (int ret)
{
  return ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED;
}

static gzochid_storage_context *
initialize (char *path)
{
  DB_ENV *db_env = NULL;
  u_int32_t env_flags = 0;
  gzochid_storage_context *context = 
    calloc (1, sizeof (gzochid_storage_context));
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
      g_info ("BDB data directory %s does not exist; creating...", path);
      if (g_mkdir (path, 493) != 0)
	{
	  g_warning ("Unable to create BDB data directory %s.", path);
	  return NULL;
	}
    }

  assert (db_env_create (&db_env, 0) == 0);
  assert (db_env->set_lk_detect (db_env, DB_LOCK_YOUNGEST) == 0);

  env_flags = DB_CREATE
    | DB_INIT_LOCK
    | DB_INIT_MPOOL
    | DB_INIT_TXN
    | DB_RECOVER
    | DB_THREAD;

  ret = db_env->open (db_env, path, env_flags, 0);
  if (ret != 0)
    {
      g_warning 
	("Unable to open BDB environment in %s: %s", path, db_strerror (ret));
      return NULL;
    }

  db_env->set_flags (db_env, DB_TXN_WRITE_NOSYNC, TRUE);

  context->environment = db_env;

  return context;
}

static void 
close_context (gzochid_storage_context *context)
{
  DB_ENV *env = context->environment;

  env->close (env, DB_FORCESYNC);

  free (context);
}

void
destroy_context (char *path)
{
  GDir *dir = NULL;
  const gchar *entry = NULL;
  DB_ENV *env = NULL;

  assert (db_env_create (&env, 0) == 0);
  assert (env->remove (env, path, DB_FORCE) == 0);

  dir = g_dir_open (path, 0, NULL);
  while ((entry = g_dir_read_name (dir)) != NULL)
    if (strncmp (entry, "log.", 4) == 0)
      {
	gchar *filename = g_strconcat (path, "/", entry, NULL);
	assert (g_remove (filename) == 0);
	g_free (filename);
      }

  g_dir_close (dir);
  assert (g_rmdir (path) == 0);
}

static gzochid_storage_store *
open (gzochid_storage_context *context, char *path, unsigned int flags)
{
  DB *db = NULL;
  DB_TXN *dbopen_tx = NULL;
  DB_ENV *db_env = context->environment;

  int pathlen = strlen (path);
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));
  u_int32_t db_flags = 0;
  int ret = 0;

  char *filename = malloc (sizeof (char) * (pathlen + 4));

  filename = strncpy (filename, path, pathlen + 1);
  filename = strncat (filename, ".db", 3);
  
  assert (db_create (&db, db_env, 0) == 0);
  db_flags = DB_THREAD;

  if (flags & GZOCHID_STORAGE_CREATE)
    db_flags |= DB_CREATE;
  if (flags & GZOCHID_STORAGE_EXCL)
    db_flags |= DB_EXCL;

  db_env->txn_begin (db_env, NULL, &dbopen_tx, 0);
  ret = db->open (db, dbopen_tx, filename, NULL, DB_BTREE, db_flags, 0);
  if (ret != 0)
    {
      db->close (db, 0);

      g_warning
	("Unable to open BDB database in %s: %s", filename, db_strerror (ret));
      dbopen_tx->abort (dbopen_tx);
      return NULL;
    }
  dbopen_tx->commit (dbopen_tx, 0);

  store->database = db;
  store->context = context;

  free (filename);

  return store;
}

static void 
close_store (gzochid_storage_store *store)
{
  DB *db = store->database;

  db->close (db, 0);
  free (store);
}

static void
destroy_store (gzochid_storage_context *context, char *path)
{
  DB_ENV *env = context->environment;  
  gchar *filename = g_strconcat (path, ".db", NULL);

  env->dbremove (env, NULL, filename, NULL, 0);
  g_free (filename);
}

static gzochid_storage_transaction *
transaction_begin (gzochid_storage_context *context)
{
  DB_ENV *db_env = context->environment;
  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  DB_TXN *txn;

  db_env->txn_begin (db_env, NULL, &txn, 0);

  transaction->context = context;
  transaction->txn = txn;

  return transaction;
}

static gzochid_storage_transaction *
transaction_begin_timed (gzochid_storage_context *context, 
			 struct timeval timeout)
{
  gzochid_storage_transaction *transaction = transaction_begin (context);
  db_timeout_t t = timeout.tv_usec + timeout.tv_sec * 1000000;
  DB_TXN *txn = transaction->txn;

  assert (t > 0);

  assert (txn->set_timeout (txn, t, DB_SET_TXN_TIMEOUT) == 0);
  assert (txn->set_timeout (txn, t, DB_SET_LOCK_TIMEOUT) == 0);

  return transaction;
}

static void 
transaction_commit (gzochid_storage_transaction *tx)
{
  DB_TXN *txn = tx->txn;

  assert (txn->commit (txn, 0) == 0);
  free (tx);
}

static void 
transaction_rollback (gzochid_storage_transaction *tx)
{
  DB_TXN *txn = tx->txn;

  txn->abort (txn);
  free (tx);
}

static void 
transaction_prepare (gzochid_storage_transaction *tx)
{
}

static char *
transaction_get_internal (gzochid_storage_transaction *tx, 
			  gzochid_storage_store *store, char *key, 
			  size_t key_len, size_t *len, gboolean write_lock)
{
  DB *db = store->database;
  DB_TXN *txn = tx->txn;
  DBT db_key, db_data;
  u_int32_t flags = write_lock ? DB_RMW : 0;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.flags = DB_DBT_MALLOC;
  ret = db->get (db, txn, &db_key, &db_data, flags);
  if (ret == 0)
    {
      if (db_data.data != NULL && len != NULL)
	*len = db_data.size;

      return db_data.data;
    }
  else 
    {
      if (ret != DB_NOTFOUND)
	{
	  g_warning 
	    ("Failed to retrieve key %s in transaction: %s", 
	     key, db_strerror (ret)); 
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}

static char *
transaction_get (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, size_t *len)
{
  return transaction_get_internal (tx, store, key, key_len, len, FALSE);
}

static char *
transaction_get_for_update (gzochid_storage_transaction *tx, 
			    gzochid_storage_store *store, char *key, 
			    size_t key_len, size_t *len)
{
  return transaction_get_internal (tx, store, key, key_len, len, TRUE);
}

static void 
transaction_put (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, char *data, size_t data_len)
{
  DB *db = store->database;
  DB_TXN *txn = tx->txn;
  DBT db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.data = data;
  db_data.size = data_len;

  ret = db->put (db, txn, &db_key, &db_data, 0);

  if (ret != 0)
    {
      g_warning 
	("Failed to store key %s in transaction: %s", key, db_strerror (ret)); 
      tx->rollback = TRUE;
      tx->should_retry = retryable (ret);
    }
}

static int
transaction_delete (gzochid_storage_transaction *tx, 
		    gzochid_storage_store *store, char *key, size_t key_len)
{
  DB *db = store->database;
  DB_TXN *txn = tx->txn;
  DBT db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  db_key.data = key;
  db_key.size = key_len;

  ret = db->del (db, txn, &db_key, 0);

  if (ret != 0)
    {
      g_warning
	("Failed to delete key %s in transaction: %s", key, db_strerror (ret)); 
      
      if (ret == DB_NOTFOUND)
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
  DB *db = store->database;
  DB_TXN *txn = tx->txn;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  db->cursor (db, txn, &cursor, 0);
  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.flags = DB_DBT_MALLOC;
  ret = cursor->get (cursor, &db_key, &db_value, DB_FIRST);
  cursor->close (cursor);

  if (ret != 0)
    {
      g_warning
	("Failed to seek to first key in transaction: %s", db_strerror (ret));
      tx->rollback = TRUE;
      tx->should_retry = retryable (ret);

      return NULL;
    }

  if (db_key.data != NULL && len != NULL)
    *len = db_key.size;

  return db_key.data;
}

static char *
transaction_next_key (gzochid_storage_transaction *tx, 
		      gzochid_storage_store *store, char *key, size_t key_len, 
		      size_t *len)
{
  DB *db = store->database;
  DB_TXN *txn = tx->txn;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  db->cursor (db, txn, &cursor, 0);

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;
  db_key.flags = DB_DBT_MALLOC;

  ret = cursor->get (cursor, &db_key, &db_value, DB_SET_RANGE);
  if (ret == 0 
      && db_key.size == key_len 
      && memcmp (db_key.data, key, key_len) == 0)
    {
      free (db_key.data);
      ret = cursor->get (cursor, &db_key, &db_value, DB_NEXT);
    }

  cursor->close (cursor);

  if (ret == 0)
    {
      if (len != NULL)
	*len = db_key.size;
      return db_key.data;
    }
  else 
    {
      if (ret != DB_NOTFOUND)
	{
	  g_warning
	    ("Failed to advance cursor in transaction: %s", db_strerror (ret));
	  tx->rollback = TRUE;
	  tx->should_retry = retryable (ret);
	}
      return NULL;
    }
}

static gzochid_storage_engine_interface interface = 
  {
    "bdb",

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
