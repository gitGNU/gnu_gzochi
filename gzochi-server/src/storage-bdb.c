/* storage-bdb.c: Database storage routines for gzochid (Berkeley DB)
 * Copyright (C) 2012 Julian Graham
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
#include <libgen.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
#include "storage.h"

typedef struct _bdb_context
{
  DB *db;
  DB_ENV *db_env;
} bdb_context;

gzochid_storage_store *gzochid_storage_open (char *path)
{
  DB_TXN *dbopen_tx = NULL;
  int pathlen = strlen (path);
  int pathbaselen = 0;
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));
  bdb_context *context = malloc (sizeof (bdb_context));
  u_int32_t db_flags, env_flags = 0;
  int ret = 0;

  char *pathcopy = strndup (path, pathlen);
  char *pathbase = basename (pathcopy); 
  char *filename = NULL;

  pathbaselen = strlen (pathbase);
  filename = malloc (sizeof (char) * (pathlen + pathbaselen + 5));
  
  filename = strncpy (filename, path, pathlen + 1);
  filename = strncat (filename, "/", 1);
  filename = strncat (filename, pathbase, pathbaselen);
  filename = strncat (filename, ".db", 3);
  
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
	("BDB data directory %s does not exist; creating...", path);
      if (g_mkdir (path, 493) != 0)
	{
	  gzochid_err ("Unable to create BDB data directory %s.", path);
	  return NULL;
	}
    }

  assert (db_env_create (&context->db_env, 0) == 0);
  env_flags = DB_CREATE
    | DB_INIT_LOCK
    | DB_INIT_MPOOL
    | DB_INIT_TXN
    | DB_RECOVER
    | DB_THREAD;

  ret = context->db_env->open (context->db_env, path, env_flags, 0);
  if (ret != 0)
    {
      gzochid_err 
	("Unable to open BDB environment in %s: %s", path, db_strerror (ret));
      return NULL;
    }

  assert (db_create (&context->db, context->db_env, 0) == 0);
  db_flags = DB_CREATE
    | DB_MULTIVERSION
    | DB_THREAD;

  context->db_env->txn_begin (context->db_env, NULL, &dbopen_tx, 0);
  ret = context->db->open 
    (context->db, dbopen_tx, filename, NULL, DB_BTREE, db_flags, 0);
  if (ret != 0)
    {
      gzochid_err
	("Unable to open BDB database in %s: %s", filename, db_strerror (ret));
      dbopen_tx->abort (dbopen_tx);
      return NULL;
    }
  dbopen_tx->commit (dbopen_tx, 0);

  store->database = context;
  store->mutex = g_mutex_new ();

  free (pathcopy);
  free (filename);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  bdb_context *context = store->database;

  g_mutex_free (store->mutex);

  context->db->close (context->db, 0);
  context->db_env->close (context->db_env, 0);
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
  bdb_context *context = (bdb_context *) store->database;
  DBT db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.flags = DB_DBT_MALLOC;
  ret = context->db->get (context->db, NULL, &db_key, &db_data, 0);
  if (ret == 0)
    {
      if (db_data.data != NULL && len != NULL)
	*len = db_data.size;
      return db_data.data;
    }
  else 
    {
      gzochid_err ("Failed to retrieve key %s: %s", key, db_strerror (ret));
      return NULL;
    }
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  bdb_context *context = (bdb_context *) store->database;
  DBT db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.data = data;
  db_data.size = data_len;

  ret = context->db->put (context->db, NULL, &db_key, &db_data, 0);

  if (ret != 0)
    gzochid_err ("Failed to store key %s: %s", key, db_strerror (ret));
}

void gzochid_storage_delete 
(gzochid_storage_store *store, char *key, size_t key_len)
{
  bdb_context *context = (bdb_context *) store->database;
  DBT db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  db_key.data = key;
  db_key.size = key_len;

  ret = context->db->del (context->db, NULL, &db_key, 0);

  if (ret != 0)
    gzochid_err ("Failed to delete key %s: %s", key, db_strerror (ret));
}

char *gzochid_storage_first_key (gzochid_storage_store *store, size_t *len)
{
  bdb_context *context = (bdb_context *) store->database;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  context->db->cursor (context->db, NULL, &cursor, 0);
  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.flags = DB_DBT_MALLOC;
  ret = cursor->get (cursor, &db_key, &db_value, DB_FIRST);
  if (ret != 0)
    {
      gzochid_err ("Failed to seek to first key: %s", db_strerror (ret));
      cursor->close (cursor);
      return NULL;
    }

  cursor->close (cursor);

  if (db_key.data != NULL && len != NULL)
    *len = db_key.size;

  return db_key.data;
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  bdb_context *context = (bdb_context *) store->database;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  context->db->cursor (context->db, NULL, &cursor, 0);

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  ret = cursor->get (cursor, &db_key, &db_value, DB_SET);
  if (ret != 0)
    {
      if (ret != DB_NOTFOUND)
	gzochid_err ("Failed to seek to key %s: %s", key, db_strerror (ret));
      
      cursor->close (cursor);
      return NULL;
    }

  db_key.flags = DB_DBT_MALLOC;
  ret = cursor->get (cursor, &db_key, &db_value, DB_NEXT);
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
	gzochid_err ("Failed to advance cursor: %s", db_strerror (ret));
      return NULL;
    }
}

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_store *store)
{
  bdb_context *context = (bdb_context *) store->database;
  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  DB_TXN *txn;

  context->db_env->txn_begin (context->db_env, NULL, &txn, DB_TXN_NOWAIT);

  transaction->store = store;
  transaction->txn = txn;

  return transaction;
}

void gzochid_storage_transaction_commit (gzochid_storage_transaction *tx)
{
  DB_TXN *txn = (DB_TXN *) tx->txn;

  txn->commit (txn, 0);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  DB_TXN *txn = (DB_TXN *) tx->txn;

  txn->abort (txn);
}

void gzochid_storage_transaction_check (gzochid_storage_transaction *tx)
{
}

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len)
{
  bdb_context *context = (bdb_context *) tx->store->database;
  DB_TXN *txn = (DB_TXN *) tx->txn;
  DBT db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.flags = DB_DBT_MALLOC;
  ret = context->db->get (context->db, txn, &db_key, &db_data, 0);
  if (ret == 0)
    {
      if (db_data.data != NULL && len != NULL)
	*len = db_data.size;

      return db_data.data;
    }
  else 
    {
      gzochid_warning 
	("Failed to retrieve key %s in transaction: %s", 
	 key, db_strerror (ret)); 
      tx->rollback = TRUE;
      return NULL;
    }
}

void gzochid_storage_transaction_put
(gzochid_storage_transaction *tx, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  bdb_context *context = (bdb_context *) tx->store->database;
  DB_TXN *txn = (DB_TXN *) tx->txn;
  DBT db_key, db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_data, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  db_data.data = data;
  db_data.size = data_len;

  ret = context->db->put (context->db, txn, &db_key, &db_data, 0);

  if (ret != 0)
    {
      gzochid_warning 
	("Failed to store key %s in transaction: %s", key, db_strerror (ret)); 
      tx->rollback = TRUE;
    }
}

void gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, char *key, size_t key_len)
{
  bdb_context *context = (bdb_context *) tx->store->database;
  DB_TXN *txn = (DB_TXN *) tx->txn;
  DBT db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (DBT));
  db_key.data = key;
  db_key.size = key_len;

  ret = context->db->del (context->db, txn, &db_key, 0);

  if (ret != 0)
    {
      gzochid_warning
	("Failed to delete key %s in transaction: %s", key, db_strerror (ret)); 
      tx->rollback = TRUE;
    }
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, size_t *len)
{
  bdb_context *context = (bdb_context *) tx->store->database;
  DB_TXN *txn = (DB_TXN *) tx->txn;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  context->db->cursor (context->db, txn, &cursor, 0);
  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.flags = DB_DBT_MALLOC;
  ret = cursor->get (cursor, &db_key, &db_value, DB_FIRST);
  cursor->close (cursor);

  if (ret != 0)
    {
      gzochid_warning
	("Failed to seek to first key in transaction: %s", db_strerror (ret));
      tx->rollback = TRUE;
      return NULL;
    }

  if (db_key.data != NULL && len != NULL)
    *len = db_key.size;

  return db_key.data;
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, char *key, size_t key_len, size_t *len)
{
  bdb_context *context = (bdb_context *) tx->store->database;
  DB_TXN *txn = (DB_TXN *) tx->txn;
  DBC *cursor = NULL;
  DBT db_key, db_value;
  int ret = 0;

  context->db->cursor (context->db, txn, &cursor, 0);

  memset (&db_key, 0, sizeof (DBT));
  memset (&db_value, 0, sizeof (DBT));

  db_key.data = key;
  db_key.size = key_len;

  ret = cursor->get (cursor, &db_key, &db_value, DB_SET);

  if (ret != 0)
    {
      if (ret != DB_NOTFOUND)
	{
	  gzochid_warning
	    ("Failed to seek to key %s in transaction: %s", 
	     key, db_strerror (ret));
	  tx->rollback = TRUE;
	}
      cursor->close (cursor);
      return NULL;
    }

  db_key.flags = DB_DBT_MALLOC;
  ret = cursor->get (cursor, &db_key, &db_value, DB_NEXT);  
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
	  gzochid_warning
	    ("Failed to advance cursor in transaction: %s", db_strerror (ret));
	  tx->rollback = TRUE;
	}
      return NULL;
    }
}
