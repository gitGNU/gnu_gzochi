/* test-data-protocol.c: Test routines for data-protocol.c in gzochid.
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

#include "data-protocol.h"
#include "oids.h"

static void
test_reserve_oids_response ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_oids_block oids_block;
  gzochid_data_reserve_oids_response *response1 = NULL;
  gzochid_data_reserve_oids_response *response2 = NULL;

  oids_block.block_start = 123;
  oids_block.block_size = 100;
  
  response1 = gzochid_data_reserve_oids_response_new ("test", &oids_block);
  
  gzochid_data_protocol_reserve_oids_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_reserve_oids_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_cmpint (response2->block.block_start, ==, 123);
  g_assert_cmpint (response2->block.block_size, ==, 100);
  
  gzochid_data_reserve_oids_response_free (response1);
  gzochid_data_reserve_oids_response_free (response2);

  g_bytes_unref (bytes);
}

static void
test_changeset ()
{
  GBytes *bytes = NULL, *data = NULL;
  GByteArray *arr = g_byte_array_new ();

  GArray *changes = g_array_sized_new
    (FALSE, TRUE, sizeof (gzochid_data_change), 4);
  
  gzochid_data_changeset *changeset1 = NULL;
  gzochid_data_changeset *changeset2 = NULL;

  gzochid_data_change *object_change1a =
    &g_array_index (changes, gzochid_data_change, 0);
  gzochid_data_change *object_change2a =
    &g_array_index (changes, gzochid_data_change, 1);

  gzochid_data_change *binding_change1a =
    &g_array_index (changes, gzochid_data_change, 2);
  gzochid_data_change *binding_change2a =
    &g_array_index (changes, gzochid_data_change, 3);

  gzochid_data_change *object_change1b = NULL;
  gzochid_data_change *object_change2b = NULL;
  gzochid_data_change *binding_change1b = NULL;
  gzochid_data_change *binding_change2b = NULL;
    
  g_array_set_size (changes, 4);

  object_change1a->store = strdup ("oids");
  object_change1a->delete = TRUE;
  object_change1a->key = g_bytes_new_static ("1", 2);

  object_change2a->store = strdup ("oids");
  object_change2a->key = g_bytes_new_static ("2", 2);
  object_change2a->data = g_bytes_new_static ("foo", 4);

  binding_change1a->store = strdup ("names");
  binding_change1a->delete = TRUE;
  binding_change1a->key = g_bytes_new_static ("foo", 4);

  binding_change2a->store = strdup ("names");
  binding_change2a->key = g_bytes_new_static ("bar", 4);
  binding_change2a->data = g_bytes_new_static ("3", 2);
  
  changeset1 = gzochid_data_changeset_new ("test", changes);
  
  gzochid_data_protocol_changeset_write (changeset1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  changeset2 = gzochid_data_protocol_changeset_read (bytes);

  g_assert_nonnull (changeset2);
  
  object_change1b = &g_array_index (changes, gzochid_data_change, 0);
  object_change2b = &g_array_index (changes, gzochid_data_change, 1);

  binding_change1b = &g_array_index (changes, gzochid_data_change, 2);
  binding_change2b = &g_array_index (changes, gzochid_data_change, 3);

  g_assert_true (g_bytes_equal (object_change1a->key, object_change1b->key));
  g_assert_true (object_change1b->delete);
  g_assert_true (g_bytes_equal (object_change2a->key, object_change2b->key));
  g_assert_false (object_change2b->delete);
  g_assert_true (g_bytes_equal (object_change2a->data, object_change2b->data));

  g_assert_true (g_bytes_equal (binding_change1a->key, binding_change1b->key));
  g_assert_true (binding_change1b->delete);
  g_assert_true (g_bytes_equal (binding_change2a->key, binding_change2b->key));
  g_assert_false (binding_change2b->delete);
  g_assert_true (g_bytes_equal (binding_change2a->data,
				binding_change2b->data));
  
  gzochid_data_changeset_free (changeset1);
  gzochid_data_changeset_free (changeset2);

  free (object_change1a->store);
  g_bytes_unref (object_change1a->key);
  
  free (object_change2a->store);
  g_bytes_unref (object_change2a->key);
  g_bytes_unref (object_change2a->data);

  free (binding_change1a->store);
  g_bytes_unref (binding_change1a->key);

  free (binding_change2a->store);
  g_bytes_unref (binding_change2a->key);
  g_bytes_unref (binding_change2a->data);

  g_bytes_unref (bytes);
  g_array_unref (changes);
}

static void
test_data_response_success ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;
  GBytes *data = g_bytes_new_static ("foo\000bar", 8);

  response1 = gzochid_data_response_new ("test", "oids", TRUE, data);

  gzochid_data_protocol_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_cmpstr (response2->store, ==, "oids");
  g_assert_true (response2->success);
  g_assert_nonnull (response2->data);
  g_assert_true (g_bytes_equal (data, response2->data));
  
  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (data);  
  g_bytes_unref (bytes);
}

static void
test_data_response_not_found ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  response1 = gzochid_data_response_new ("test", "names", TRUE, NULL);

  gzochid_data_protocol_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_cmpstr (response2->store, ==, "names");
  g_assert_true (response2->success);
  g_assert_null (response2->data);
  
  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (bytes);
}

static void
test_data_response_failure ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_response *response1 = NULL;
  gzochid_data_response *response2 = NULL;

  response1 = gzochid_data_response_new ("test", "oids", FALSE, NULL);

  gzochid_data_protocol_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_cmpstr (response2->store, ==, "oids");
  g_assert_false (response2->success);
  g_assert_null (response2->data);
  
  gzochid_data_response_free (response1);
  gzochid_data_response_free (response2);

  g_bytes_unref (bytes);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func
    ("/data-protocol/reserve-oids-response", test_reserve_oids_response);
  g_test_add_func ("/data-protocol/changeset", test_changeset);
  g_test_add_func
    ("/data-protocol/data-response/success", test_data_response_success);
  g_test_add_func ("/data-protocol/data-response/not-found",
		   test_data_response_not_found);
  g_test_add_func
    ("/data-protocol/data-response/failure", test_data_response_failure);
  
  return g_test_run ();
}
