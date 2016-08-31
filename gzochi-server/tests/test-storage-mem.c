/* test-storage-mem.c: Test routines for storage-mem.c in gzochid.
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
#include <stdlib.h>
#include <string.h>

#include "gzochid-storage.h"
#include "storage-mem.h"

struct test_storage_fixture
{
  gzochid_storage_context *context;
  gzochid_storage_store *store;
};

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
                 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static void
test_storage_fixture_setup
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  fixture->context = gzochid_storage_engine_interface_mem.initialize ("");
  fixture->store = gzochid_storage_engine_interface_mem.open 
    (fixture->context, "", 0);
}

static void
test_storage_fixture_teardown
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  gzochid_storage_engine_interface_mem.close_store (fixture->store);
  gzochid_storage_engine_interface_mem.close_context (fixture->context);
}

static void
test_storage_mem_initialize ()
{
  gzochid_storage_context *context = 
    gzochid_storage_engine_interface_mem.initialize ("");
  
  g_assert (context != NULL);

  gzochid_storage_engine_interface_mem.close_context (context);
}

static void 
test_storage_mem_open ()
{
  gzochid_storage_context *context = 
    gzochid_storage_engine_interface_mem.initialize ("");
  gzochid_storage_store *store = 
    gzochid_storage_engine_interface_mem.open (context, "", 0);
  
  g_assert (store != NULL);

  gzochid_storage_engine_interface_mem.close_store (store);
  gzochid_storage_engine_interface_mem.close_context (context);
}

static void
test_storage_mem_tx_put_get_commit_get
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  char *value = NULL;
  size_t value_len = 0;

  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);
  gzochid_storage_engine_interface_mem.transaction_put 
    (tx, fixture->store, "foo", 4, "bar", 4);
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert_cmpstr (value, ==, "bar");
  g_assert_cmpint (value_len, ==, 4);
  free (value);

  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);
  
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);

  g_assert_cmpstr (value, ==, "bar");
  g_assert_cmpint (value_len, ==, 4);  

  free (value);
}

static void
test_storage_mem_tx_put_rollback_get
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  char *value = NULL;
  size_t value_len = 0;

  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);
  gzochid_storage_engine_interface_mem.transaction_put 
    (tx, fixture->store, "foo", 4, "bar", 4);
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert_cmpstr (value, ==, "bar");
  g_assert_cmpint (value_len, ==, 4);  
  gzochid_storage_engine_interface_mem.transaction_rollback (tx);
  free (value);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);

  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);
  
  g_assert (value == NULL);
}

static void
test_storage_mem_tx_delete_get_commit_get
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  int ret = 0;
  char *value = NULL;
  size_t value_len = 0;

  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);
  gzochid_storage_engine_interface_mem.transaction_put 
    (tx, fixture->store, "foo", 4, "bar", 4);

  ret = gzochid_storage_engine_interface_mem.transaction_delete 
    (tx, fixture->store, "foo", 4);
  g_assert_cmpint (ret, ==, 0);

  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);
  g_assert (value == NULL);

  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);

  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, "foo", 4, &value_len);
  g_assert (value == NULL);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);
}

static void
test_storage_mem_tx_delete_rollback_get
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  char *value = NULL;
  size_t value_len = 0;
  gzochid_storage_transaction *tx =
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);
    
  gzochid_storage_engine_interface_mem.transaction_put
    (tx, fixture->store, "foo", 4, "bar", 4);

  gzochid_storage_engine_interface_mem.transaction_prepare (tx);
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin 
    (fixture->context);
  gzochid_storage_engine_interface_mem.transaction_delete 
    (tx, fixture->store, "foo", 4);
  gzochid_storage_engine_interface_mem.transaction_rollback (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);

  value = gzochid_storage_engine_interface_mem.transaction_get
    (tx, fixture->store, "foo", 4, &value_len);

  g_assert_cmpstr (value, ==, "bar");
  g_assert_cmpint (value_len, ==, 4);
  free (value);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);

}

struct test_storage_fixture_concurrent
{
  struct test_storage_fixture *base_fixture;
  
  GMutex mutex;
  GCond cond;
  unsigned int latch;

  gboolean tx1_rollback;
  gboolean tx2_rollback;
};

static void
test_storage_fixture_concurrent_setup
(struct test_storage_fixture_concurrent *fixture, gconstpointer user_data)
{
  fixture->base_fixture = malloc (sizeof (struct test_storage_fixture));
  test_storage_fixture_setup (fixture->base_fixture, user_data);

  g_mutex_init (&fixture->mutex);
  g_cond_init (&fixture->cond);

  fixture->latch = 0;
  fixture->tx1_rollback = FALSE;
  fixture->tx2_rollback = FALSE;
}

static void
test_storage_fixture_concurrent_teardown
(struct test_storage_fixture_concurrent *fixture, gconstpointer user_data)
{
  test_storage_fixture_teardown (fixture->base_fixture, user_data);
  free (fixture->base_fixture);

  g_mutex_clear (&fixture->mutex);
  g_cond_clear (&fixture->cond);
}

static void
decrement_latch (struct test_storage_fixture_concurrent *fixture)
{
  g_mutex_lock (&fixture->mutex);

  fixture->latch--;

  g_cond_signal (&fixture->cond);
  g_mutex_unlock (&fixture->mutex);
}

static void
wait_latch_zero (struct test_storage_fixture_concurrent *fixture)
{
  g_mutex_lock (&fixture->mutex);

  while (fixture->latch > 0)
    g_cond_wait (&fixture->cond, &fixture->mutex);

  g_mutex_unlock (&fixture->mutex);
}

static gpointer
deadlock_simple_thread_1 (gpointer data)
{
  struct test_storage_fixture_concurrent *fixture = data;
  gzochid_storage_transaction *tx1 = 
    gzochid_storage_engine_interface_mem.transaction_begin 
    (fixture->base_fixture->context);
  char *value1 = gzochid_storage_engine_interface_mem.transaction_get 
    (tx1, fixture->base_fixture->store, "\001", 1, NULL);

  free (value1);

  decrement_latch (fixture);
  wait_latch_zero (fixture);
  
  gzochid_storage_engine_interface_mem.transaction_put 
    (tx1, fixture->base_fixture->store, "\047", 1, "quux2", 6);

  if (tx1->rollback)
    fixture->tx1_rollback = TRUE;
  
  gzochid_storage_engine_interface_mem.transaction_rollback (tx1);

  return NULL;
}

static gpointer
deadlock_simple_thread_2 (gpointer data)
{
  struct test_storage_fixture_concurrent *fixture = data;
  gzochid_storage_transaction *tx2 = 
    gzochid_storage_engine_interface_mem.transaction_begin 
    (fixture->base_fixture->context);
  char *value2 = gzochid_storage_engine_interface_mem.transaction_get 
    (tx2, fixture->base_fixture->store, "\047", 1, NULL);

  free (value2);

  decrement_latch (fixture);
  wait_latch_zero (fixture);

  gzochid_storage_engine_interface_mem.transaction_put 
    (tx2, fixture->base_fixture->store, "\001", 1, "bar2", 5);

  if (tx2->rollback)
    fixture->tx2_rollback = TRUE;

  gzochid_storage_engine_interface_mem.transaction_rollback (tx2);

  return NULL;
}

static void
test_storage_mem_tx_deadlock_simple
(struct test_storage_fixture_concurrent *fixture, gconstpointer user_data)
{
  GThread *thread1 = NULL;
  GThread *thread2 = NULL;

  gzochid_storage_transaction *tx =
    gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->base_fixture->context);

  int i = 0;
  char c = 0;
  char buf[1024];
  
  char *value = NULL;
  size_t value_len = 0;

  for (; i < 40; i++)
    {
      c = (char) i;
      memset (buf, c, 1024);
      gzochid_storage_engine_interface_mem.transaction_put 
	(tx, fixture->base_fixture->store, &c, 1, buf, 1024);
    }
  
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  g_test_log_set_fatal_handler (ignore_warnings, NULL);
  
  fixture->latch = 2;

  thread1 = g_thread_new
    ("deadlock-simple-thread-1", deadlock_simple_thread_1, fixture);
  thread2 = g_thread_new 
    ("deadlock-simple-thread-2", deadlock_simple_thread_2, fixture);

  g_thread_join (thread1);
  g_thread_join (thread2);

  g_assert (fixture->tx1_rollback ^ fixture->tx2_rollback);
}

static void
test_storage_mem_tx_split_root
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  int i = 0;
  char c = 0;
  char buf[1024];
  
  char *value = NULL;
  size_t value_len = 0;
  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);

  for (; i < 40; i++)
    {
      c = (char) i;
      memset (buf, c, 1024);
      gzochid_storage_engine_interface_mem.transaction_put 
	(tx, fixture->store, &c, 1, buf, 1024);
    }
  
  gzochid_storage_engine_interface_mem.transaction_commit (tx);
  
  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);
  
  c = 0;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 0);
  g_assert_cmpint (value_len, ==, 1024);
  free (value);

  c = 39;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 39);
  g_assert_cmpint (value_len, ==, 1024);
  free (value);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);

}

static void
test_storage_mem_tx_split_internal
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  int i = 0;
  char c = 0;
  char buf[1024];

  char *value = NULL;
  size_t value_len = 0;
  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);

  for (; i < 40; i++)
    {
      c = (char) i * 10;
      memset (buf, c, 1024);
      
      gzochid_storage_engine_interface_mem.transaction_put 
	(tx, fixture->store, &c, 1, buf, 1024);
    }
  for (i = 1; i < 40; i++)
    {
      c = (char) i;
      memset (buf, c, 1024);

      gzochid_storage_engine_interface_mem.transaction_put 
	(tx, fixture->store, &c, 1, buf, 1024);
    }
  
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);

  c = 1;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 1);
  g_assert_cmpint (value_len, ==, 1024);
  free (value);

  c = 90;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 90);
  g_assert_cmpint (value_len, ==, 1024);
  free (value);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);
}

static void
test_storage_mem_tx_merge_internal
(struct test_storage_fixture *fixture, gconstpointer user_data)
{
  int i = 0;
  char c = 0;

  char *value = NULL;
  size_t value_len = 0;
  gzochid_storage_transaction *tx = 
    gzochid_storage_engine_interface_mem.transaction_begin (fixture->context);

  for (; i < 20; i++)
    {
      c = (char) i;
      gzochid_storage_engine_interface_mem.transaction_put 
	(tx, fixture->store, &c, 1, &c, 1);
    }
  for (i = 5; i < 15; i++)
    {
      int ret = 0;
      c = (char) i;
      ret = gzochid_storage_engine_interface_mem.transaction_delete 
	(tx, fixture->store, &c, 1);

      g_assert_cmpint (ret, ==, 0);
    }
  
  gzochid_storage_engine_interface_mem.transaction_commit (tx);

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (fixture->context);

  c = 0;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 0);
  g_assert_cmpint (value_len, ==, 1);
  free (value);

  c = 19;
  value = gzochid_storage_engine_interface_mem.transaction_get 
    (tx, fixture->store, &c, 1, &value_len);

  g_assert (value != NULL);
  g_assert_cmpint (*value, ==, 19);
  g_assert_cmpint (value_len, ==, 1);
  free (value);

  gzochid_storage_engine_interface_mem.transaction_rollback (tx);  
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/storage-mem/initialize", test_storage_mem_initialize);
  g_test_add_func ("/storage-mem/open", test_storage_mem_open);

  g_test_add
    ("/storage-mem/tx/put-get-commit-get", struct test_storage_fixture, NULL, 
     test_storage_fixture_setup, test_storage_mem_tx_put_get_commit_get, 
     test_storage_fixture_teardown);
  g_test_add
    ("/storage-mem/tx/put-rollback-get", struct test_storage_fixture, NULL, 
     test_storage_fixture_setup, test_storage_mem_tx_put_rollback_get, 
     test_storage_fixture_teardown);
  g_test_add
    ("/storage-mem/tx/delete-get-commit-get", struct test_storage_fixture, NULL,
     test_storage_fixture_setup, test_storage_mem_tx_delete_get_commit_get, 
     test_storage_fixture_teardown);
  g_test_add
    ("/storage-mem/tx/delete-rollback-get", struct test_storage_fixture, NULL,
     test_storage_fixture_setup, test_storage_mem_tx_delete_rollback_get, 
     test_storage_fixture_teardown);

  g_test_add
    ("/storage-mem/tx/deadlock/simple", struct test_storage_fixture_concurrent,
     NULL, test_storage_fixture_concurrent_setup, 
     test_storage_mem_tx_deadlock_simple, 
     test_storage_fixture_concurrent_teardown);

  g_test_add
    ("/storage-mem/tx/split/root", struct test_storage_fixture, NULL,
     test_storage_fixture_setup, test_storage_mem_tx_split_root,
     test_storage_fixture_teardown);
  g_test_add
    ("/storage-mem/tx/split/internal", struct test_storage_fixture, NULL,
     test_storage_fixture_setup, test_storage_mem_tx_split_internal,
     test_storage_fixture_teardown);

  g_test_add
    ("/storage-mem/tx/merge/internal", struct test_storage_fixture, NULL,
     test_storage_fixture_setup, test_storage_mem_tx_merge_internal,
     test_storage_fixture_teardown);

  return g_test_run ();
}
