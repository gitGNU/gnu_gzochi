/* treefile.c: A toy storage engine implementation for gzochid.
 * Copyright (C) 2015 Julian Graham
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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gzochid-storage.h"

/* 
   The following data structures and functions comprise a "toy" storage engine
   implementation based on GLib's `GTree' data structure, which is used to hold
   data in memory during the lifetime of a transaction. At the boundaries of a
   transaction, data is read from and written to a text file, with the keys and
   values separate by newlines. No attempt is made to escape embedded newlines.

   This implementation chooses not to provide controls around concurrent access
   the storage on disk - multiple threads may execute transactions against a
   store, but they are not guaranteed to the properties of consistency or
   isolation.

   For these reasons, this engine should only be used for simple, isolated test
   cases!
*/

/* The named file on disk that stores the contents of the database. */

struct _treefile
{
  char *path; /* The database filename. */
  GIOChannel *channel; /* Handle for reading to / writing from the database. */
};

typedef struct _treefile treefile;

/* A datum (key or value) to be stored within the GTree. */

struct _treefile_datum
{
  unsigned char *data; /* The data. */
  size_t data_len; /* The length of the data. */
};

typedef struct _treefile_datum treefile_datum;

/* Used to hold contextual data while traversing the GTree. */

struct _treefile_traversal_context
{
  treefile_datum reference_datum; /* The datum being searched / compared. */
  treefile_datum match_datum; /* The matched datum, if any. */
};

typedef struct _treefile_traversal_context treefile_traversal_context;

/* Compares two `treefile_datum' structures, returning -1, 0, or 1, 
   respectively, if the first is byte-lexicographically less than, equal to, or
   greater than the second. */

static gint
compare_datum (gconstpointer a, gconstpointer b)
{
  size_t i = 0, j = 0;

  const treefile_datum *datum_a = a;
  const treefile_datum *datum_b = b;

  while (i < datum_a->data_len && j < datum_b->data_len)
    {
      if (datum_a->data[i] < datum_b->data[j])
	return -1;
      else if (datum_b->data[j] < datum_a->data[i])
	return 1;

      i++;
      j++;
    }

  /* If the two buffers equivalent up to the length of the smaller buffer, then
     the shorter one is less than the longer one. */

  return (datum_a->data_len - i) - (datum_b->data_len - j);
}

static gzochid_storage_context *
initialize (char *path)
{
  gzochid_storage_context *context = 
    calloc (1, sizeof (gzochid_storage_context));

  context->environment = strdup (path);

  return context;
}

static void
close_context (gzochid_storage_context *context)
{
  free (context);
}

static void
destroy_context (char *path)
{
  g_rmdir (path);
}

/* Sets or resets the character handling for the GLib IO Channel used to read or
   write the treefile. In particular, set the encoding to `NULL' to put the
   Channel in "binary" mode and set the line terminator to be a single newline
   to support embedded `NULL' characters. */

static void
set_channel_format (GIOChannel *channel)
{
  g_io_channel_set_encoding (channel, NULL, NULL);
  g_io_channel_set_line_term (channel, "\n", 1);
}

static gzochid_storage_store *
open (gzochid_storage_context *context, char *path, unsigned int flags)
{
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));
  treefile *treefile = malloc (sizeof (struct _treefile));

  treefile->path = strdup (path);

  /* If the `CREATE' flag is given, use the "w+" mode for `fopen', which has the
     effect of truncating the file. */

  if (flags & GZOCHID_STORAGE_CREATE)
    treefile->channel = g_io_channel_new_file (path, "w+", NULL);
  else treefile->channel = g_io_channel_new_file (path, "r+", NULL);

  assert (treefile->channel != NULL);

  set_channel_format (treefile->channel);

  store->context = context;
  store->database = treefile;
  g_mutex_init (&store->mutex);

  return store;
}

static void
close_store (gzochid_storage_store *store)
{
  treefile *treefile = store->database;

  g_io_channel_shutdown (treefile->channel, TRUE, NULL);
  free (treefile->path);
  free (treefile);

  g_mutex_clear (&store->mutex);
  free (store);
}

static void
destroy_store (gzochid_storage_context *context, char *path)
{
  g_remove (path);
}

static void
lock (gzochid_storage_store *store)
{
  g_mutex_lock (&store->mutex);
}

static void
unlock (gzochid_storage_store *store)
{
  g_mutex_unlock (&store->mutex);
}

static gzochid_storage_transaction *
transaction_begin (gzochid_storage_context *context)
{
  gzochid_storage_transaction *tx =
    calloc (1, sizeof (gzochid_storage_transaction));

  tx->txn = g_hash_table_new (g_direct_hash, g_direct_equal);
  
  return tx;
}

static gzochid_storage_transaction *
transaction_begin_timed (gzochid_storage_context *context,
			 struct timeval timeout)
{
  /* Timed transactions are not implemented! */

  assert (1 == 0);
}

/* Destructively transfer the contents of the specified `GString' to the
   specified `treefile_datum'. */

static void
move_to_datum (treefile_datum *datum, GString *str)
{
  datum->data = malloc (sizeof (unsigned char) * str->len - 1);
  datum->data_len = str->len - 1;

  memcpy (datum->data, str->str, str->len - 1);
  g_string_erase (str, 0, str->len);	      
}

/* Allows lazy enlistment of the store in a transaction. Bootstraps an in-memory
   GTree with the contents of the store file. */

static GTree *
ensure_store_tx (gzochid_storage_transaction *tx, gzochid_storage_store *store)
{
  GHashTable *ht = tx->txn;
  GTree *tree = g_hash_table_lookup (ht, store->database);
  
  if (tree == NULL)
    {
      GString *str = g_string_new ("");
      treefile *treefile = store->database;
      GIOStatus status = g_io_channel_seek_position 
	(treefile->channel, 0, G_SEEK_SET, NULL);

      assert (status == G_IO_STATUS_NORMAL);
      tree = g_tree_new (compare_datum);

      do 
	{
	  /* Use `g_io_channel_read_line_string' instead of plain old
	     `g_io_channel_read_line' because the latter uses `g_strndup' to
	     transfer bytes to the return value, which has the effect of
	     truncating lines with embedded `NULL' bytes. Is this a bug? */
	  
	  status = g_io_channel_read_line_string
	    (treefile->channel, str, NULL, NULL);

	  if (status == G_IO_STATUS_NORMAL)
	    {
	      treefile_datum *key_datum = malloc (sizeof (treefile_datum));
	      treefile_datum *value_datum = malloc (sizeof (treefile_datum));
	      GError *err = NULL;

	      move_to_datum (key_datum, str);
	      
	      status = g_io_channel_read_line_string
		(treefile->channel, str, NULL, &err);	      
	      
	      assert (status == G_IO_STATUS_NORMAL);

	      move_to_datum (value_datum, str);
	      g_tree_insert (tree, key_datum, value_datum);
	    }
	}
      while (status == G_IO_STATUS_NORMAL);
      
      g_hash_table_insert (ht, store->database, tree);
      g_string_free (str, TRUE);
    }

  return tree;
}

static char *
get_internal (gzochid_storage_transaction *tx, gzochid_storage_store *store,
	      char *key, size_t key_len, size_t *value_len)
{
  char *ret = NULL;
  GTree *tree = ensure_store_tx (tx, store);
  treefile_datum key_datum;
  treefile_datum *value_datum = NULL;

  key_datum.data = (unsigned char *) key;
  key_datum.data_len = key_len;
  
  value_datum = g_tree_lookup (tree, &key_datum);
  
  if (value_datum == NULL)
    return NULL;
  
  ret = malloc (sizeof (unsigned char) * value_datum->data_len);
  memcpy (ret, value_datum->data, value_datum->data_len);

  if (value_len != NULL)
    *value_len = value_datum->data_len;

  return ret;
}

static char *
transaction_get (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, size_t *value_len)
{
  return get_internal (tx, store, key, key_len, value_len);
}

/* Write a single key-value datum pair out to the store's file channel. Keys and
   values are separated by the newline (\n) character.
*/

static gboolean
write_keyvalue (gpointer key, gpointer value, gpointer data)
{
  GIOChannel *channel = data;

  treefile_datum *key_datum = key;
  treefile_datum *value_datum = value;

  char *nl = "\n";

  g_io_channel_write_chars 
    (channel, (char *) key_datum->data, key_datum->data_len, NULL, NULL);
  g_io_channel_write_chars (channel, nl, 1, NULL, NULL);
  g_io_channel_write_chars 
    (channel, (char *) value_datum->data, value_datum->data_len, NULL, NULL);
  g_io_channel_write_chars (channel, nl, 1, NULL, NULL);

  return FALSE;
}

static void
write_treefile (gpointer key, gpointer value, gpointer user_data)
{  
  treefile *treefile = key;

/* Before flushing the current transaction to disk, close and re-open the
   database file (truncating it in the process) for the store being flushed.
*/

  g_io_channel_shutdown (treefile->channel, TRUE, NULL);

  treefile->channel = g_io_channel_new_file (treefile->path, "w+", NULL);
  set_channel_format (treefile->channel);
  
  g_tree_foreach (value, write_keyvalue, treefile->channel);
  g_io_channel_flush (treefile->channel, NULL);
}

static void
transaction_prepare (gzochid_storage_transaction *tx)
{
}

static void
transaction_commit (gzochid_storage_transaction *tx)
{
  g_hash_table_foreach (tx->txn, write_treefile, NULL);
  g_hash_table_destroy (tx->txn);
  free (tx);
}

static void
transaction_rollback (gzochid_storage_transaction *tx)
{
  g_hash_table_destroy (tx->txn);
  free (tx);  
}

static char *
get (gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  char *value = NULL;
  gzochid_storage_transaction *tx = transaction_begin (store->context);

  value = transaction_get (tx, store, key, key_len, len);
  transaction_rollback (tx);

  return value;
}

static void
transaction_put (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, char *value, size_t value_len)
{
  GTree *tree = ensure_store_tx (tx, store);
  treefile_datum *key_datum = malloc (sizeof (treefile_datum));
  treefile_datum *value_datum = malloc (sizeof (treefile_datum));

  key_datum->data = malloc (sizeof (unsigned char) * key_len);
  memcpy (key_datum->data, key, key_len);
  key_datum->data_len = key_len;

  value_datum->data = malloc (sizeof (unsigned char) * value_len);
  memcpy (value_datum->data, value, value_len);
  value_datum->data_len = value_len;
  
  g_tree_replace (tree, key_datum, value_datum);
}

static void
put (gzochid_storage_store *store, char *key, size_t key_len, char *value,
     size_t value_len)
{
  gzochid_storage_transaction *tx = transaction_begin (store->context);
  transaction_put (tx, store, key, key_len, value, value_len);
  transaction_commit (tx);
}

static int
transaction_delete (gzochid_storage_transaction *tx,
		    gzochid_storage_store *store, char *key, size_t key_len)
{
  GTree *tree = ensure_store_tx (tx, store);
  treefile_datum key_datum;

  key_datum.data = (unsigned char *) key;
  key_datum.data_len = key_len;

  if (! g_tree_remove (tree, &key_datum))
    return GZOCHID_STORAGE_ENOTFOUND;
  else return 0;
}

static int
delete (gzochid_storage_store *store, char *key, size_t key_len)
{
  int ret = 0;
  gzochid_storage_transaction *tx = transaction_begin (store->context);
  
  ret = transaction_delete (tx, store, key, key_len);
  
  if (ret == 0)
    transaction_commit (tx);
  else
    {
      assert (ret != GZOCHID_STORAGE_ETXFAILURE);
      transaction_rollback (tx);
    }

  return ret;
}

static gboolean
key_after (gpointer key, gpointer value, gpointer data)
{
  treefile_traversal_context *context = data;
  treefile_datum *current_key_datum = key;

  if (compare_datum (&context->reference_datum, current_key_datum) < 0)
    {
      context->match_datum = *current_key_datum;
      return TRUE;
    }
  else return FALSE;
}

static char *
transaction_first_key (gzochid_storage_transaction *tx,
		       gzochid_storage_store *store, size_t *key_len)
{
  GTree *tree = ensure_store_tx (tx, store);

  if (g_tree_nnodes (tree) > 0)
    {
      char *ret = NULL;
      treefile_traversal_context context = { { NULL, 0 }, { NULL, 0 } };

      g_tree_foreach (tree, key_after, &context);
      ret = malloc (sizeof (unsigned char) * context.match_datum.data_len);
      memcpy (ret, context.match_datum.data, context.match_datum.data_len);
      
      if (key_len != NULL)
	*key_len = context.match_datum.data_len;

      return ret;
    }
  else return NULL;
}

static char *
first_key (gzochid_storage_store *store, size_t *key_len)
{
  char *key = NULL;

  gzochid_storage_transaction *tx = transaction_begin (store->context);
  key = transaction_first_key (tx, store, key_len);
  transaction_rollback (tx);

  return key;
}

static char *
transaction_next_key (gzochid_storage_transaction *tx,
		      gzochid_storage_store *store, char *key, size_t key_len,
		      size_t *next_key_len)
{
  GTree *tree = ensure_store_tx (tx, store);

  if (g_tree_nnodes (tree) > 0)
    {
      char *ret = NULL;
      treefile_traversal_context context = { { NULL, 0 }, { NULL, 0 } };

      context.reference_datum.data = (unsigned char *) key;
      context.reference_datum.data_len = key_len;

      g_tree_foreach (tree, key_after, &context);

      if (context.match_datum.data != NULL)
	{
	  ret = malloc (sizeof (unsigned char) * context.match_datum.data_len);
	  memcpy (ret, context.match_datum.data, context.match_datum.data_len);
      
	  if (next_key_len != NULL)
	    *next_key_len = context.match_datum.data_len;
	  
	  return ret;
	}
      else return NULL;
    }
  else return NULL;
}

static char *
next_key (gzochid_storage_store *store, char *key, size_t key_len,
	  size_t *next_key_len)
{
  char *next_key = NULL;

  gzochid_storage_transaction *tx = transaction_begin (store->context);
  next_key = transaction_next_key (tx, store, key, key_len, next_key_len);
  transaction_rollback (tx);

  return next_key;
}

static char *
transaction_get_for_update (gzochid_storage_transaction *tx,
			    gzochid_storage_store *store, char *key,
			    size_t key_len, size_t *value_len)
{
  return get_internal (tx, store, key, key_len, value_len);
}

static gzochid_storage_engine_interface interface = 
  {
    "treefile",

    initialize,
    close_context,
    destroy_context,
    open,
    close_store,
    destroy_store,
    lock,
    unlock,

    get,
    put,
    delete,
    first_key,
    next_key,
    
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
