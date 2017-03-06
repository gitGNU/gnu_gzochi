/* test-oids-dataclient.c: Tests for oids-dataclient.c in gzochid.
 * Copyright (C) 2017 Julian Graham
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
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "dataclient.h"
#include "oids-dataclient.h"
#include "tx.h"

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static gpointer
reserve_oids_inner (gpointer data)
{
  gpointer *args = data;
  gzochid_data_oids_block block;
  gzochid_dataclient_oids_callback callback = args[1];
  gpointer callback_data = args[2];
  
  block.block_start = 1;
  block.block_size = 100;
  
  callback (block, callback_data);

  free (args);
  return NULL;
}

struct _GzochidDataClient
{
  GObject parent_instance;
  
  GThreadFunc reserve_oids;
  GThread *thread;

  GMutex mutex;
  GCond cond;
};

G_DEFINE_TYPE (GzochidDataClient, gzochid_data_client, G_TYPE_OBJECT);

static void
gzochid_data_client_finalize (GObject *obj)
{
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (obj);
  
  g_thread_join (client->thread);
  g_mutex_clear (&client->mutex);
  g_cond_clear (&client->cond);
  
  G_OBJECT_CLASS (gzochid_data_client_parent_class)->finalize (obj);
}

static void
gzochid_data_client_class_init (GzochidDataClientClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_data_client_finalize;
}

static void
gzochid_data_client_init (GzochidDataClient *self)
{
  self->reserve_oids = reserve_oids_inner;
  self->thread = NULL;

  g_mutex_init (&self->mutex);
  g_cond_init (&self->cond);
}

void
gzochid_dataclient_reserve_oids (GzochidDataClient *client, char *app,
				 gzochid_dataclient_oids_callback callback,
				 gpointer data)
{
  gpointer *args = malloc (sizeof (gpointer) * 3);

  args[0] = client;
  args[1] = callback;
  args[2] = data;
  
  client->thread = g_thread_new ("fake-main-loop", client->reserve_oids, args);
}

static void
test_oids_dataclient_reserve ()
{
  GzochidDataClient *dataclient = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  gzochid_oid_allocation_strategy *strategy =
    gzochid_dataclient_oid_strategy_new (dataclient, "test");
  gzochid_data_oids_block block;
  GError *err = NULL;
  
  g_assert (gzochid_oids_reserve_block (strategy, &block, &err));
  g_assert_no_error (err);
  g_assert_cmpint (block.block_start, ==, 1);
  g_assert_cmpint (block.block_size, ==, 100);

  gzochid_oid_allocation_strategy_free (strategy);  
  g_object_unref (dataclient);
}

static gpointer
reserve_oids_timeout (gpointer data)
{
  gpointer *args = data;
  GzochidDataClient *client = GZOCHID_DATA_CLIENT (args[0]);

  g_mutex_lock (&client->mutex);
  g_cond_signal (&client->cond);

  if (!g_cond_wait_until
      (&client->cond, &client->mutex, g_get_monotonic_time () + 5000000))
    g_assert (FALSE);
  
  g_mutex_unlock (&client->mutex);

  reserve_oids_inner (data);
  
  return NULL;
}

static int
noop_prepare (gpointer data)
{
  return TRUE;
}

static void
noop_commit_rollback (gpointer data)
{
}

gzochid_transaction_participant test_participant =
  { "test", noop_prepare, noop_commit_rollback, noop_commit_rollback };

static void
oids_dataclient_timeout_inner (gpointer data)
{
  GzochidDataClient *dataclient = g_object_new (GZOCHID_TYPE_DATA_CLIENT, NULL);
  gzochid_oid_allocation_strategy *strategy =
    gzochid_dataclient_oid_strategy_new (dataclient, "test");
  gzochid_data_oids_block block;
  GError *err = NULL;

  dataclient->reserve_oids = reserve_oids_timeout;

  gzochid_transaction_join (&test_participant, NULL);

  g_mutex_lock (&dataclient->mutex);
  g_assert_false (gzochid_oids_reserve_block (strategy, &block, &err));

  if (!g_cond_wait_until
      (&dataclient->cond, &dataclient->mutex,
       g_get_monotonic_time () + 5000000))
    g_test_fail ();

  g_cond_signal (&dataclient->cond);
  g_mutex_unlock (&dataclient->mutex);
  
  gzochid_oid_allocation_strategy_free (strategy);
  g_object_unref (dataclient);
}

static void
test_oids_dataclient_timeout ()
{
  g_test_log_set_fatal_handler (ignore_warnings, NULL);

  gzochid_transaction_execute_timed
    (oids_dataclient_timeout_inner, NULL, (struct timeval) { 0, 0 });
}

int
main (int argc, char *argv[])
{
#if GLIB_CHECK_VERSION (2, 36, 0)
  /* No need for `g_type_init'. */
#else
  g_type_init ();
#endif /* GLIB_CHECK_VERSION */

  g_test_init (&argc, &argv, NULL);

  g_test_add_func
    ("/oids-dataclient/reserve/simple", test_oids_dataclient_reserve);
  g_test_add_func
    ("/oids-dataclient/reserve/timeout", test_oids_dataclient_timeout);
  
  return g_test_run ();
}
