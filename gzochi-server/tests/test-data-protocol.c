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
#include <gmp.h>
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

  mpz_init (oids_block.block_start);
  mpz_set_ui (oids_block.block_start, 123);

  oids_block.block_size = 100;
  
  response1 = gzochid_data_reserve_oids_response_new ("test", &oids_block);
  mpz_clear (oids_block.block_start);
  
  gzochid_data_protocol_reserve_oids_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_reserve_oids_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert (mpz_cmp_ui (response2->block.block_start, 123) == 0);
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

  GArray *object_changes = g_array_sized_new
    (FALSE, TRUE, sizeof (gzochid_data_object_change), 2);
  GArray *binding_changes = g_array_sized_new
    (FALSE, TRUE, sizeof (gzochid_data_binding_change), 2);
  
  gzochid_data_changeset *changeset1 = NULL;
  gzochid_data_changeset *changeset2 = NULL;

  gzochid_data_object_change *object_change1 =
    &g_array_index (object_changes, gzochid_data_object_change, 0);
  gzochid_data_object_change *object_change2 =
    &g_array_index (object_changes, gzochid_data_object_change, 1);

  gzochid_data_binding_change *binding_change1 =
    &g_array_index (binding_changes, gzochid_data_binding_change, 0);
  gzochid_data_binding_change *binding_change2 =
    &g_array_index (binding_changes, gzochid_data_binding_change, 1);

  g_array_set_size (object_changes, 2);
  g_array_set_size (binding_changes, 2);
  
  object_change1->delete = TRUE;  
  mpz_init (object_change1->oid);
  mpz_set_ui (object_change1->oid, 1);

  data = g_bytes_new_static ("foo", 4);
  
  mpz_init (object_change2->oid);
  mpz_set_ui (object_change2->oid, 2);
  object_change2->data = g_bytes_ref (data);

  binding_change1->delete = TRUE;
  binding_change1->name = strdup ("foo");

  binding_change2->name = strdup ("bar");
  mpz_init (binding_change2->oid);
  mpz_set_ui (binding_change2->oid, 3);
  
  changeset1 = gzochid_data_changeset_new
    ("test", object_changes, binding_changes);
  
  gzochid_data_protocol_changeset_write (changeset1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  changeset2 = gzochid_data_protocol_changeset_read (bytes);

  g_assert_nonnull (changeset2);
  
  object_change1 = &g_array_index
    (object_changes, gzochid_data_object_change, 0);
  object_change2 = &g_array_index
    (object_changes, gzochid_data_object_change, 1);

  binding_change1 = &g_array_index
    (binding_changes, gzochid_data_binding_change, 0);
  binding_change2 = &g_array_index
    (binding_changes, gzochid_data_binding_change, 1);

  g_assert (mpz_cmp_ui (object_change1->oid, 1) == 0);
  g_assert_true (object_change1->delete);
  g_assert (mpz_cmp_ui (object_change2->oid, 2) == 0);
  g_assert_false (object_change2->delete);
  g_assert_true (g_bytes_equal (data, object_change2->data));

  g_assert_cmpstr (binding_change1->name, ==, "foo");
  g_assert_true (binding_change1->delete);
  g_assert_cmpstr (binding_change2->name, ==, "bar");
  g_assert_false (binding_change2->delete);
  g_assert (mpz_cmp_ui (binding_change2->oid, 3) == 0);
  
  gzochid_data_changeset_free (changeset1);
  gzochid_data_changeset_free (changeset2);
  
  g_array_unref (object_changes);
  g_array_unref (binding_changes);

  g_bytes_unref (bytes);
  g_bytes_unref (data);
}

static void
test_object_response_success ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_object_response *response1 = NULL;
  gzochid_data_object_response *response2 = NULL;
  GBytes *data = g_bytes_new_static ("foo\000bar", 8);

  response1 = gzochid_data_object_response_new ("test", TRUE, data);

  gzochid_data_protocol_object_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_object_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_nonnull (response2->data);
  g_assert_true (g_bytes_equal (data, response2->data));
  
  gzochid_data_object_response_free (response1);
  gzochid_data_object_response_free (response2);

  g_bytes_unref (data);  
  g_bytes_unref (bytes);
}

static void
test_object_response_not_found ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_object_response *response1 = NULL;
  gzochid_data_object_response *response2 = NULL;

  response1 = gzochid_data_object_response_new ("test", TRUE, NULL);

  gzochid_data_protocol_object_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_object_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_null (response2->data);
  
  gzochid_data_object_response_free (response1);
  gzochid_data_object_response_free (response2);

  g_bytes_unref (bytes);
}

static void
test_object_response_failure ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_object_response *response1 = NULL;
  gzochid_data_object_response *response2 = NULL;

  response1 = gzochid_data_object_response_new ("test", FALSE, NULL);

  gzochid_data_protocol_object_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_object_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_false (response2->success);
  g_assert_null (response2->data);
  
  gzochid_data_object_response_free (response1);
  gzochid_data_object_response_free (response2);

  g_bytes_unref (bytes);
}

static void
test_binding_response_success ()
{
  mpz_t oid;
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  mpz_init (oid);
  mpz_set_ui (oid, 123);
  
  response1 = gzochid_data_binding_response_oid_new ("test", oid);
  
  gzochid_data_protocol_binding_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_true (response2->present);
  g_assert (mpz_cmp_ui (response2->oid, 123) == 0);
  
  gzochid_data_binding_response_free (response1);
  gzochid_data_binding_response_free (response2);
  g_bytes_unref (bytes);
  mpz_clear (oid);
}

static void
test_binding_response_not_found ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  response1 = gzochid_data_binding_response_new ("test", TRUE);
  
  gzochid_data_protocol_binding_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_false (response2->present);
  
  gzochid_data_binding_response_free (response1);
  gzochid_data_binding_response_free (response2);
  g_bytes_unref (bytes);
}

static void
test_binding_response_failure ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_response *response1 = NULL;
  gzochid_data_binding_response *response2 = NULL;

  response1 = gzochid_data_binding_response_new ("test", FALSE);
  
  gzochid_data_protocol_binding_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_false (response2->success);
  g_assert_false (response2->present);
  
  gzochid_data_binding_response_free (response1);
  gzochid_data_binding_response_free (response2);
  g_bytes_unref (bytes);
}

static void
test_binding_key_response_success ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_key_response *response1 =
    gzochid_data_binding_key_response_new ("test", TRUE, "foo");
  gzochid_data_binding_key_response *response2 = NULL;
  
  gzochid_data_protocol_binding_key_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_key_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_cmpstr (response2->name, ==, "foo");
  
  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_key_response_free (response2);
  g_bytes_unref (bytes);
}

static void
test_binding_key_response_not_found ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_key_response *response1 =
    gzochid_data_binding_key_response_new ("test", TRUE, NULL);
  gzochid_data_binding_key_response *response2 = NULL;

  gzochid_data_protocol_binding_key_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_key_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_true (response2->success);
  g_assert_null (response2->name);
    
  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_key_response_free (response2);
  g_bytes_unref (bytes);
}

static void
test_binding_key_response_failure ()
{
  GBytes *bytes = NULL;
  GByteArray *arr = g_byte_array_new ();
  gzochid_data_binding_key_response *response1 =
    gzochid_data_binding_key_response_new ("test", FALSE, NULL);
  gzochid_data_binding_key_response *response2 = NULL;

  gzochid_data_protocol_binding_key_response_write (response1, arr);

  bytes = g_byte_array_free_to_bytes (arr);
  response2 = gzochid_data_protocol_binding_key_response_read (bytes);

  g_assert_nonnull (response2);
  g_assert_cmpstr (response2->app, ==, "test");
  g_assert_false (response2->success);
  g_assert_null (response2->name);
    
  gzochid_data_binding_key_response_free (response1);
  gzochid_data_binding_key_response_free (response2);
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
    ("/data-protocol/object-response/success", test_object_response_success);
  g_test_add_func ("/data-protocol/object-response/not-found",
		   test_object_response_not_found);
  g_test_add_func
    ("/data-protocol/object-response/failure", test_object_response_failure);
  g_test_add_func
    ("/data-protocol/binding-response/success", test_binding_response_success);
  g_test_add_func ("/data-protocol/binding-response/not-found",
		   test_binding_response_not_found);
  g_test_add_func
    ("/data-protocol/binding-response/failure", test_binding_response_failure);
  g_test_add_func ("/data-protocol/binding-key-response/success",
		   test_binding_key_response_success);
  g_test_add_func ("/data-protocol/binding-key-response/not-found",
		   test_binding_key_response_not_found);
  g_test_add_func ("/data-protocol/binding-key-response/failure",
		   test_binding_key_response_failure);
  
  return g_test_run ();
}
