/* mock-storage.c: Test-time storage engine using GTrees.
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "../storage.h"

static gint g_string_compare (gconstpointer a, gconstpointer b, gpointer data)
{
  GString *s1 = (GString *) a;
  GString *s2 = (GString *) b;

  int i = 0;
  while (i < s1->len && i < s2->len)
    {
      if (s1->str[i] < s2->str[i])
	return -1;
      else if (s1->str[i] > s2->str[i])
	return 1;
      i++;
    }

  if (s1->len == s2->len)
    return 0;
  else if (s1->len < s2->len)
    return -1;
  else return 1;
}

static void g_string_destroy (gpointer data)
{
  g_string_free ((GString *) data, TRUE);
}

gzochid_storage_context *gzochid_storage_initialize (char *path)
{
  return NULL;
}

gzochid_storage_store *gzochid_storage_open 
(gzochid_storage_context *context, char *path)
{
  gzochid_storage_store *store = calloc (1, sizeof (gzochid_storage_store));

  store->mutex = g_mutex_new ();
  store->database = g_tree_new_full
    (g_string_compare, NULL, g_string_destroy, g_string_destroy);

  return store;
}

void gzochid_storage_close (gzochid_storage_store *store)
{
  g_mutex_free (store->mutex);
  g_tree_destroy ((GTree *) store->database);

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
  GString *str = g_string_new_len (key, key_len);
  GString *val = g_tree_lookup ((GTree *) store->database, str);

  g_string_free (str, TRUE);

  if (val != NULL)
    {
      char *data = malloc (sizeof (char) * val->len);
      memcpy (data, val->str, val->len);

      if (len != NULL)
	*len = val->len;
      
      return data;
    }
  else return NULL;
}

void gzochid_storage_put 
(gzochid_storage_store *store, char *key, size_t key_len, char *data, 
 size_t data_len)
{
  g_tree_insert 
    ((GTree *) store->database, 
     g_string_new_len (key, key_len), 
     g_string_new_len (data, data_len));
}

void gzochid_storage_delete 
(gzochid_storage_store *store, char *key, size_t key_len)
{
  GString *str = g_string_new_len (key, key_len);
  g_tree_remove ((GTree *) store->database, str);
  g_string_free (str, TRUE);
}

static gboolean first_key (gpointer key, gpointer value, gpointer data)
{
  *((GString **) data) = (GString *) key;

  return TRUE;
}

static gboolean first_key_after (gpointer key, gpointer value, gpointer data)
{
  gboolean ret = FALSE;
  GString *ref = ((GString **) data)[0];
  GString *last_key = ((GString **) data)[1];

  if (last_key != NULL && g_string_equal (ref, last_key))
    ret = TRUE;

  ((GString **) data)[1] = (GString *) key;
  return ret;
}

char *gzochid_storage_first_key (gzochid_storage_store *store, size_t *len)
{
  GString *key = NULL;
  g_tree_foreach ((GTree *) store->database, first_key, &key);

  if (key != NULL)
    {
      if (len != NULL)
	*len = key->len;
      return key->str;
    }
  else return NULL;
}

char *gzochid_storage_next_key 
(gzochid_storage_store *store, char *key, size_t key_len, size_t *len)
{
  GString *keys[2] = { NULL, NULL };
  keys[0] = g_string_new_len (key, key_len);
  g_tree_foreach ((GTree *) store->database, first_key, &keys);

  if (keys[1] != NULL)
    {
      if (len != NULL)
	*len = keys[1]->len;
      return keys[1]->str;
    }
  else return NULL;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin_timed
(gzochid_storage_context *context, struct timeval tv)
{
  gzochid_storage_transaction *tx = 
    calloc (1, sizeof (gzochid_storage_transaction));
  
  tx->context = context;

  return tx;
}

gzochid_storage_transaction *gzochid_storage_transaction_begin
(gzochid_storage_context *context)
{
  gzochid_storage_transaction *tx = 
    calloc (1, sizeof (gzochid_storage_transaction));
  
  tx->context = context;

  return tx;
}

void gzochid_storage_transaction_commit (gzochid_storage_transaction *tx)
{
  free (tx);
}

void gzochid_storage_transaction_rollback (gzochid_storage_transaction *tx)
{
  free (tx);
}

void gzochid_storage_transaction_prepare (gzochid_storage_transaction *tx)
{
}

char *gzochid_storage_transaction_get 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  return gzochid_storage_get (store, key, key_len, len);
}

char *gzochid_storage_transaction_get_for_update
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  return gzochid_storage_get (store, key, key_len, len);
}

void gzochid_storage_transaction_put
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, char *data, size_t data_len)
{
  gzochid_storage_put (store, key, key_len, data, data_len);
}

void gzochid_storage_transaction_delete
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len)
{
  gzochid_storage_delete (store, key, key_len);
}

char *gzochid_storage_transaction_first_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, size_t *len)
{
  return gzochid_storage_first_key (store, len);
}

char *gzochid_storage_transaction_next_key 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, char *key, 
 size_t key_len, size_t *len)
{
  return gzochid_storage_next_key (store, key, key_len, len);
}
