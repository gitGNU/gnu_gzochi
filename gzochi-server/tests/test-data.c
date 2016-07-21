/* test-data.c: Test routines for data.c in gzochid.
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "data.h"
#include "game.h"
#include "io.h"
#include "oids.h"
#include "oids-storage.h"
#include "storage-mem.h"
#include "tx.h"

static gboolean serialized = FALSE;
static gboolean deserialized = FALSE;
static gboolean finalized = FALSE;

static void 
test_serializer 
(gzochid_application_context *context, void *ptr, GByteArray *out, GError **err)
{
  serialized = TRUE;
}

static void 
test_failure_serializer
(gzochid_application_context *context, void *ptr, GByteArray *out, GError **err)
{
  g_set_error (err, GZOCHID_IO_ERROR, GZOCHID_IO_ERROR_SERIALIZATION,
	       "Failed to serialize record.");
}

static void *test_deserializer 
(gzochid_application_context *context, GByteArray *in, GError **err)
{
  deserialized = TRUE;
  return g_string_new_len (in->data, in->len);
}

static void test_finalizer (gzochid_application_context *context, void *ptr)
{
  g_string_free ((GString *) ptr, TRUE);
  finalized = TRUE;
}

gzochid_io_serialization test_serialization = 
  { test_serializer, test_deserializer, test_finalizer };

gzochid_io_serialization test_serialization_failure_serializer = 
  { test_failure_serializer, test_deserializer, test_finalizer };

static void 
reset_serialization_state ()
{
  serialized = FALSE;
  deserialized = FALSE;
  finalized = FALSE;
}

static void 
fetch_reference (gpointer data)
{
  gzochid_application_context *context = data;
  gzochid_data_managed_reference *ref = NULL;

  ref = gzochid_data_create_reference_to_oid (context, &test_serialization, 0);
  gzochid_data_dereference (ref, NULL);
}

static void 
application_context_init (gzochid_application_context *context)
{
  gzochid_context *base = (gzochid_context *) context;
  gzochid_game_context *game_context = gzochid_game_context_new (NULL);
  base->parent = (gzochid_context *) game_context;

  game_context->storage_engine = malloc (sizeof (gzochid_storage_engine));
  game_context->storage_engine->interface = 
    &gzochid_storage_engine_interface_mem;

  context->storage_context = 
    gzochid_storage_engine_interface_mem.initialize ("/dev/null");
  context->meta = gzochid_storage_engine_interface_mem.open
    (context->storage_context, "/dev/null", 0);
  context->oids = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);
  context->names = gzochid_storage_engine_interface_mem.open 
    (context->storage_context, "/dev/null", 0);

  context->oid_strategy = gzochid_storage_oid_strategy_new
    (game_context->storage_engine->interface, context->storage_context,
     context->meta);
}

static void
application_context_shutdown (gzochid_application_context *context)
{
  gzochid_context *base = (gzochid_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) base->parent;

  free (game_context->storage_engine);
  gzochid_storage_engine_interface_mem.close_store (context->meta);
  gzochid_storage_engine_interface_mem.close_store (context->oids);
  gzochid_storage_engine_interface_mem.close_store (context->names);
  gzochid_storage_engine_interface_mem.close_context (context->storage_context);

  gzochid_oid_allocation_strategy_free (context->oid_strategy);
  
  gzochid_game_context_free (game_context);
}

static void test_data_reference_finalize ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_storage_transaction *tx = NULL;
  guint64 zero = 0;
  
  application_context_init (context);
  reset_serialization_state ();

  tx = gzochid_storage_engine_interface_mem.transaction_begin
    (context->storage_context);

  gzochid_storage_engine_interface_mem.transaction_put 
    (tx, context->oids, (char *) &zero, sizeof (guint64), "foo", 4);

  gzochid_storage_engine_interface_mem.transaction_prepare (tx);  
  gzochid_storage_engine_interface_mem.transaction_commit (tx);  
  
  gzochid_transaction_execute (fetch_reference, context);

  g_assert (!serialized);
  g_assert (deserialized);
  g_assert (finalized);

  application_context_shutdown (context);
  gzochid_application_context_free (context);
}

static void
flush_reference_failure (gpointer data)
{
  GString *str = g_string_new ("test-string");
  gzochid_application_context *context = data;

  gzochid_data_create_reference
    (context, &test_serialization_failure_serializer, str, NULL);
}

static void
test_data_transaction_prepare_flush_reference_failure ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_transaction_result result;

  application_context_init (context);
  reset_serialization_state ();
  result = gzochid_transaction_execute (flush_reference_failure, context);

  g_assert_cmpint (GZOCHID_TRANSACTION_SHOULD_RETRY, ==, result);

  g_assert (!serialized);
  g_assert (!deserialized);
  g_assert (finalized);  

  application_context_shutdown (context);
  gzochid_application_context_free (context);
}

static void 
binding_exists_timed_out (gpointer data)
{
  GError *error = NULL;
  gzochid_application_context *context = data;
  gboolean exists = gzochid_data_binding_exists (context, "test", &error);

  g_assert (! exists);
  g_assert_error (error, GZOCHID_DATA_ERROR, GZOCHID_DATA_ERROR_TRANSACTION);
  g_error_free (error);
}

static 
void test_data_transaction_timeout ()
{
  struct timeval tm = { 0, 0 };
  gzochid_application_context *context = gzochid_application_context_new ();

  application_context_init (context);
  gzochid_transaction_execute_timed (binding_exists_timed_out, context, tm);
  application_context_shutdown (context);
  gzochid_application_context_free (context);
}

int 
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/data/reference/finalize", test_data_reference_finalize);
  g_test_add_func ("/data/transaction/prepare/flush-reference-failure",
		   test_data_transaction_prepare_flush_reference_failure);
  g_test_add_func ("/data/transaction/timeout", test_data_transaction_timeout);

  return g_test_run ();
}
