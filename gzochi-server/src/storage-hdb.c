/* storage-bdb.c: Database storage routines for gzochid (hamsterdb)
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
#include <ham/hamsterdb.h>
#include <libgen.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "log.h"
#include "storage.h"

#define GZOCHID_STORAGE_HDB_META 1
#define GZOCHID_STORAGE_HDB_NAMES 2
#define GZOCHID_STORAGE_HDB_OIDS 3

#define HDB_FILENAME "/gzochid.hdb"

static gzochid_storage_context *report_env_error 
(ham_status_t code, GString *str)
{
  gzochid_err 
    ("Unable to open HDB environment %s: %s", str->str, ham_strerror (code));
  g_string_free (str, TRUE);
  return NULL;
}

gzochid_storage_context *gzochid_storage_initialize (char *path)
{
  ham_env_t *db_env = NULL;
  ham_u32_t env_flags = HAM_ENABLE_TRANSACTIONS | HAM_AUTO_RECOVERY;

  GString *path_str = g_string_new (path);
  gzochid_storage_context *context = 
    calloc (1, sizeof (gzochid_storage_context));

  g_string_append (path_str, HDB_FILENAME);
  
  if (g_file_test (path_str->str, G_FILE_TEST_EXISTS))
    {
      int ret = ham_env_open (&db_env, path_str->str, env_flags, NULL);
      if (ret != HAM_SUCCESS)
	return report_env_error (ret, path_str);
    }
  else 
    {
      int ret = 0;
      if (! g_file_test (path, G_FILE_TEST_EXISTS))
	{
	  gzochid_notice
	    ("HDB environment %s does not exist; creating...", path);
	  if (g_mkdir (path, 493) != 0)
	    {
	      gzochid_err ("Unable to create HDB data directory %s.", path);
	      return NULL;
	    }
	}

      ret = ham_env_create (&db_env, path_str->str, env_flags, 0644, NULL);

      if (ret != HAM_SUCCESS)
	{
	  gzochid_err ("Unable to create HDB environment %s: %s", 
		       path, ham_strerror (ret));
	  g_string_free (path_str, TRUE);
	  return NULL;
	}
    }

  context->environment = db_env;
  g_string_free (path_str, TRUE);

  return context;
}

static gboolean ends_with (char *str, char *suffix)
{
  char *start = strrchr (str, suffix[0]);
  return start && strcmp (start, suffix) == 0;
}

static ham_u16_t path_to_name (char *path)
{
  if (ends_with (path, "/meta"))
    return GZOCHID_STORAGE_HDB_META;
  else if (ends_with (path, "/names"))
    return GZOCHID_STORAGE_HDB_NAMES;
  else if (ends_with (path, "/oids"))
    return GZOCHID_STORAGE_HDB_OIDS;
  else return 0;
}

gzochid_storage_store *gzochid_storage_open 
(gzochid_storage_context *context, char *path)
{
  ham_db_t *db = NULL;
  ham_env_t *db_env = (ham_env_t *) context->environment;
  gzochid_storage_store *store = NULL;
  u_int16_t db_name = path_to_name (path);
  int ret = 0;

  if (db_name == 0)
    {
      gzochid_err ("No HDB mapping for database %s.", path);
      return NULL;
    }

  store = calloc (1, sizeof (gzochid_storage_store));

  ret = ham_env_open_db (db_env, &db, db_name, 0, NULL);
  if (ret == HAM_DATABASE_NOT_FOUND)
    {
      ret = ham_env_create_db (db_env, &db, db_name, 0, NULL);
      if (ret != HAM_SUCCESS)
	{
	  gzochid_err
	    ("Unable to create HDB database %s: %s", path, ham_strerror (ret));
	  return NULL;
	}
    }
  else if (ret != HAM_SUCCESS)
    {
      gzochid_err
	("Unable to open HDB database %s: %s", path, ham_strerror (ret));
      return NULL;
    }
    
  store->database = db;
  store->context = context;
  g_mutex_init (&store->mutex);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  ham_db_t *db = (ham_db_t *) store->database;

  g_mutex_clear (&store->mutex);
  ham_db_close (db, 0);
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
  ham_db_t *db = (ham_db_t *) store->database;
  ham_key_t db_key;
  ham_record_t db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_data, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  ret = ham_db_find (db, NULL, &db_key, &db_data, 0);
  if (ret == HAM_SUCCESS)
    {
      char *data = NULL;
      if (db_data.data != NULL)
	{
	  if (len != NULL)
	    *len = db_data.size;
	  data = malloc (sizeof (char) * db_data.size);
	  data = memcpy (data, db_data.data, db_data.size);
	}
      return data;
    }
  else 
    {
      if (ret != HAM_KEY_NOT_FOUND)
	gzochid_err ("Failed to retrieve key %s: %s", key, ham_strerror (ret));
      return NULL;
    }
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_key_t db_key;
  ham_record_t db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_data, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  db_data.data = data;
  db_data.size = data_len;

  ret = ham_db_insert (db, NULL, &db_key, &db_data, HAM_OVERWRITE);

  if (ret != HAM_SUCCESS)
    gzochid_err ("Failed to store key %s: %s", key, ham_strerror (ret));
}

int gzochid_storage_delete 
(gzochid_storage_store *store, char *key, size_t key_len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_key_t db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  db_key.data = key;
  db_key.size = key_len;

  ret = ham_db_erase (db, NULL, &db_key, 0);

  if (ret != HAM_SUCCESS)
    {
      gzochid_err ("Failed to delete key %s: %s", key, ham_strerror (ret));
      if (ret == HAM_KEY_NOT_FOUND)
	return GZOCHID_STORAGE_ENOTFOUND;
      else return GZOCHID_STORAGE_EFAILURE;
    }
  return 0;
}

char *gzochid_storage_first_key (gzochid_storage_store *store, size_t *len)
{
  char *data = NULL;
  ham_db_t *db = (ham_db_t *) store->database;
  ham_cursor_t *cursor = NULL;
  ham_key_t db_key;
  ham_record_t db_value;
  int ret = 0;

  assert (ham_cursor_create (&cursor, db, NULL, 0) == HAM_SUCCESS);
  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_value, 0, sizeof (ham_record_t));

  ret = ham_cursor_find (cursor, &db_key, &db_value, HAM_FIND_GEQ_MATCH);
  if (ret != HAM_SUCCESS)
    {
      if (ret != HAM_KEY_NOT_FOUND)
	gzochid_err
	  ("Failed to seek to first key: %s", ham_strerror (ret));
      assert (ham_cursor_close (cursor) == HAM_SUCCESS);
      return NULL;
    }

  assert (ham_cursor_close (cursor) == HAM_SUCCESS);

  if (db_value.data != NULL)
    {
      if (len != NULL)
	*len = db_value.size;
      data = malloc (sizeof (char) * db_value.size);
      data = memcpy (data, db_value.data, db_value.size);
    }
  return data;
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_cursor_t *cursor = NULL;
  ham_key_t db_key;
  ham_record_t db_value;
  int ret = 0;

  assert (ham_cursor_create (&cursor, db, NULL, 0) == HAM_SUCCESS);
  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_value, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  ret = ham_cursor_find (cursor, &db_key, &db_value, HAM_FIND_GT_MATCH);
  assert (ham_cursor_close (cursor) == HAM_SUCCESS);

  if (ret == HAM_SUCCESS)
    {
      char *data = NULL;
      if (db_value.data != NULL)
	{
	  if (len != NULL)
	    *len = db_value.size;
	  data = malloc (sizeof (char) * db_value.size);
	  data = memcpy (data, db_value.data, db_value.size);
	}
      return data;
    }
  else 
    {
      if (ret != HAM_KEY_NOT_FOUND)
	gzochid_err ("Failed to advance cursor: %s", ham_strerror (ret));
      return NULL;
    }
}

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_context *context)
{
  ham_env_t *db_env = (ham_env_t *) context->environment;

  gzochid_storage_transaction *transaction = 
    calloc (1, sizeof (gzochid_storage_transaction));
  ham_txn_t *txn = NULL;

  assert (ham_txn_begin (&txn, db_env, NULL, NULL, 0) == HAM_SUCCESS);

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
  ham_txn_t *txn = (ham_txn_t *) tx->txn;

  assert (ham_txn_commit (txn, 0) == HAM_SUCCESS);
  free (tx);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  ham_txn_t *txn = (ham_txn_t *) tx->txn;

  assert (ham_txn_abort (txn, 0) == HAM_SUCCESS);
  free (tx);
}

void gzochid_storage_transaction_prepare (gzochid_storage_transaction *tx)
{
}

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_txn_t *txn = (ham_txn_t *) tx->txn;
  ham_key_t db_key;
  ham_record_t db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_data, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  ret = ham_db_find (db, txn, &db_key, &db_data, 0);

  if (ret == HAM_SUCCESS)
    {
      char *data = NULL;
      if (db_data.data != NULL)
	{
	  if (len != NULL)
	    *len = db_data.size;
	  data = malloc (sizeof (char) * db_data.size);
	  data = memcpy (data, db_data.data, db_data.size);
	}
      return data;
    }
  else 
    {
      if (ret != HAM_KEY_NOT_FOUND)
	{
	  gzochid_warning 
	    ("Failed to retrieve key %s in transaction: %s", 
	     key, ham_strerror (ret)); 
	  tx->rollback = TRUE;
	  tx->should_retry = ret == HAM_TXN_CONFLICT;
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
 size_t key_len, char *data, size_t data_len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_txn_t *txn = (ham_txn_t *) tx->txn;
  ham_key_t db_key;
  ham_record_t db_data;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_data, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  db_data.data = data;
  db_data.size = data_len;

  ret = ham_db_insert (db, txn, &db_key, &db_data, HAM_OVERWRITE);

  if (ret != HAM_SUCCESS)
    {
      gzochid_warning 
	("Failed to store key %s in transaction: %s", key, ham_strerror (ret)); 
      tx->rollback = TRUE;
      tx->should_retry = ret == HAM_TXN_CONFLICT;
    }
}

int gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_txn_t *txn = (ham_txn_t *) tx->txn;
  ham_key_t db_key;
  int ret = 0;

  memset (&db_key, 0, sizeof (ham_key_t));
  db_key.data = key;
  db_key.size = key_len;

  ret = ham_db_erase (db, txn, &db_key, 0);

  if (ret != HAM_SUCCESS)
    {
      gzochid_warning
	("Failed to delete key %s in transaction: %s", key, 
	 ham_strerror (ret)); 
      
      if (ret == HAM_KEY_NOT_FOUND)
	return GZOCHID_STORAGE_ENOTFOUND;
      else
	{
	  tx->rollback = TRUE;
	  tx->should_retry = ret == HAM_TXN_CONFLICT;
	  return GZOCHID_STORAGE_ETXFAILURE;
	}
    }
  else return 0;
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, size_t *len)
{
  char *data = NULL;
  ham_db_t *db = (ham_db_t *) store->database;
  ham_txn_t *txn = (ham_txn_t *) tx->txn;
  ham_cursor_t *cursor = NULL;
  ham_key_t db_key;
  ham_record_t db_value;
  int ret = 0;

  assert (ham_cursor_create (&cursor, db, txn, 0) == HAM_SUCCESS);
  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_value, 0, sizeof (ham_record_t));

  ret = ham_cursor_find (cursor, &db_key, &db_value, HAM_FIND_GEQ_MATCH);
  assert (ham_cursor_close (cursor) == HAM_SUCCESS);

  if (ret != HAM_SUCCESS)
    {
      gzochid_warning
	("Failed to seek to first key in transaction: %s", ham_strerror (ret));
      tx->rollback = TRUE;
      tx->should_retry = ret == HAM_TXN_CONFLICT;

      return NULL;
    }

  if (db_value.data != NULL)
    {
      if (len != NULL)
	*len = db_value.size;
      data = malloc (sizeof (char) * db_value.size);
      data = memcpy (data, db_value.data, db_value.size);
    }
  return data;
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  ham_db_t *db = (ham_db_t *) store->database;
  ham_txn_t *txn = (ham_txn_t *) tx->txn;
  ham_cursor_t *cursor = NULL;
  ham_key_t db_key;
  ham_record_t db_value;
  int ret = 0;

  assert (ham_cursor_create (&cursor, db, txn, 0) == HAM_SUCCESS);

  memset (&db_key, 0, sizeof (ham_key_t));
  memset (&db_value, 0, sizeof (ham_record_t));

  db_key.data = key;
  db_key.size = key_len;

  ret = ham_cursor_find (cursor, &db_key, &db_value, HAM_FIND_GT_MATCH);
  assert (ham_cursor_close (cursor) == HAM_SUCCESS);

  if (ret == HAM_SUCCESS)
    {
      char *data = NULL;
      if (db_value.data != NULL)
	{
	  if (len != NULL)
	    *len = db_value.size;
	  data = malloc (sizeof (char) * db_value.size);
	  data = memcpy (data, db_value.data, db_value.size);
	}
      return data;
    }
  else 
    {
      if (ret != HAM_KEY_NOT_FOUND)
	{
	  gzochid_warning
	    ("Failed to advance cursor in transaction: %s", ham_strerror (ret));
	  tx->rollback = TRUE;
	  tx->should_retry = ret == HAM_TXN_CONFLICT;
	}
      return NULL;
    }
}
