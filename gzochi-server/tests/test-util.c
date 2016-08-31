/* test-util.c: Test routines for util.c in gzochid.
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
#include <sys/time.h>

#include "util.h"

static void
test_util_serialize_boolean ()
{
  GByteArray *out = g_byte_array_new ();

  gzochid_util_serialize_boolean (TRUE, out);
  g_assert_cmpint (out->len, ==, 1);
  g_assert_cmpint (out->data[0], ==, 1);
  g_byte_array_remove_index (out, 0);

  gzochid_util_serialize_boolean (FALSE, out);
  g_assert_cmpint (out->len, ==, 1);
  g_assert_cmpint (out->data[0], ==, 0);

  g_byte_array_unref (out);
}

static void
test_util_serialize_int ()
{
  GByteArray *out = g_byte_array_new ();
  gzochid_util_serialize_int (1234, out);
  g_assert_cmpint (out->len, ==, 4);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x04);
  g_assert_cmpint (out->data[3], ==, 0xd2);
  g_byte_array_unref (out);
}

static void
test_util_serialize_uint64 ()
{
  GByteArray *out = g_byte_array_new ();
  gzochid_util_serialize_uint64 (12345678910111213, out);
  g_assert_cmpint (out->len, ==, 8);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x2b);
  g_assert_cmpint (out->data[2], ==, 0xdc);
  g_assert_cmpint (out->data[3], ==, 0x54);
  g_assert_cmpint (out->data[4], ==, 0x5d);
  g_assert_cmpint (out->data[5], ==, 0xf2);
  g_assert_cmpint (out->data[6], ==, 0xbd);
  g_assert_cmpint (out->data[7], ==, 0xed);
  g_byte_array_unref (out);
}

static void
test_util_serialize_bytes ()
{
  GByteArray *out = g_byte_array_new ();
  unsigned char bytes[3] = { 'a', 'b', 'c' };

  gzochid_util_serialize_bytes (bytes, 3, out);
  g_assert_cmpint (out->len, ==, 7);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x03);
  g_assert_cmpint (out->data[4], ==, 0x61);
  g_assert_cmpint (out->data[5], ==, 0x62);
  g_assert_cmpint (out->data[6], ==, 0x63);

  g_byte_array_unref (out);
}

static void
test_util_serialize_string ()
{
  GByteArray *out = g_byte_array_new ();

  gzochid_util_serialize_string ("Hello, world!", out);
  g_assert_cmpint (out->len, ==, 18);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x0e);

  g_assert_cmpstr (out->data + 4, ==, "Hello, world!");

  g_byte_array_unref (out);
}

static void
test_util_serialize_list ()
{
  GByteArray *out = g_byte_array_new ();
  GList *lst = g_list_append 
    (g_list_append (g_list_append (NULL, "a"), "b"), "c");

  gzochid_util_serialize_list 
    (lst, (void (*) (gpointer, GByteArray *)) gzochid_util_serialize_string,
     out);

  g_assert_cmpint (out->len, ==, 22);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x03);
  g_assert_cmpint (out->data[4], ==, 0x00);
  g_assert_cmpint (out->data[5], ==, 0x00);
  g_assert_cmpint (out->data[6], ==, 0x00);
  g_assert_cmpint (out->data[7], ==, 0x02);
  g_assert_cmpint (out->data[8], ==, 0x61);
  g_assert_cmpint (out->data[9], ==, 0x00);
  g_assert_cmpint (out->data[10], ==, 0x00);
  g_assert_cmpint (out->data[11], ==, 0x00);
  g_assert_cmpint (out->data[12], ==, 0x00);
  g_assert_cmpint (out->data[13], ==, 0x02);
  g_assert_cmpint (out->data[14], ==, 0x62);
  g_assert_cmpint (out->data[15], ==, 0x00);
  g_assert_cmpint (out->data[16], ==, 0x00);
  g_assert_cmpint (out->data[17], ==, 0x00);
  g_assert_cmpint (out->data[18], ==, 0x00);
  g_assert_cmpint (out->data[19], ==, 0x02);
  g_assert_cmpint (out->data[20], ==, 0x63);
  g_assert_cmpint (out->data[21], ==, 0x00);

  g_list_free (lst);
  g_byte_array_unref (out);
}

static void
test_util_serialize_sequence ()
{
  GByteArray *out = g_byte_array_new ();
  GSequence *seq = g_sequence_new (NULL);

  g_sequence_append (seq, "a");
  g_sequence_append (seq, "b");
  g_sequence_append (seq, "c");
  
  gzochid_util_serialize_sequence
    (seq, (void (*) (gpointer, GByteArray *)) gzochid_util_serialize_string,
     out);

  g_assert_cmpint (out->len, ==, 22);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x03);
  g_assert_cmpint (out->data[4], ==, 0x00);
  g_assert_cmpint (out->data[5], ==, 0x00);
  g_assert_cmpint (out->data[6], ==, 0x00);
  g_assert_cmpint (out->data[7], ==, 0x02);
  g_assert_cmpint (out->data[8], ==, 0x61);
  g_assert_cmpint (out->data[9], ==, 0x00);
  g_assert_cmpint (out->data[10], ==, 0x00);
  g_assert_cmpint (out->data[11], ==, 0x00);
  g_assert_cmpint (out->data[12], ==, 0x00);
  g_assert_cmpint (out->data[13], ==, 0x02);
  g_assert_cmpint (out->data[14], ==, 0x62);
  g_assert_cmpint (out->data[15], ==, 0x00);
  g_assert_cmpint (out->data[16], ==, 0x00);
  g_assert_cmpint (out->data[17], ==, 0x00);
  g_assert_cmpint (out->data[18], ==, 0x00);
  g_assert_cmpint (out->data[19], ==, 0x02);
  g_assert_cmpint (out->data[20], ==, 0x63);
  g_assert_cmpint (out->data[21], ==, 0x00);

  g_sequence_free (seq);
  g_byte_array_unref (out);
}

static void
test_util_serialize_hash_table ()
{
  GByteArray *out = g_byte_array_new ();
  GHashTable *ht = g_hash_table_new (g_str_hash, g_str_equal);

  g_hash_table_insert (ht, "foo", "bar");

  gzochid_util_serialize_hash_table
    (ht, 
     (void (*) (gpointer, GByteArray *)) gzochid_util_serialize_string, 
     (void (*) (gpointer, GByteArray *)) gzochid_util_serialize_string, 
     out);

  g_assert_cmpint (out->len, ==, 20);
  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x01);
  g_assert_cmpint (out->data[4], ==, 0x00);
  g_assert_cmpint (out->data[5], ==, 0x00);
  g_assert_cmpint (out->data[6], ==, 0x00);
  g_assert_cmpint (out->data[7], ==, 0x04);
  g_assert_cmpint (out->data[8], ==, 0x66);
  g_assert_cmpint (out->data[9], ==, 0x6f);
  g_assert_cmpint (out->data[10], ==, 0x6f);
  g_assert_cmpint (out->data[11], ==, 0x00);
  g_assert_cmpint (out->data[12], ==, 0x00);
  g_assert_cmpint (out->data[13], ==, 0x00);
  g_assert_cmpint (out->data[14], ==, 0x00);
  g_assert_cmpint (out->data[15], ==, 0x04);
  g_assert_cmpint (out->data[16], ==, 0x62);
  g_assert_cmpint (out->data[17], ==, 0x61);
  g_assert_cmpint (out->data[18], ==, 0x72);
  g_assert_cmpint (out->data[19], ==, 0x00);

  g_hash_table_destroy (ht);
  g_byte_array_unref (out);
}

static void
test_util_serialize_timeval ()
{
  GByteArray *out = g_byte_array_new ();
  struct timeval tv = { 1, 2 };

  gzochid_util_serialize_timeval (tv, out);

  g_assert_cmpint (out->data[0], ==, 0x00);
  g_assert_cmpint (out->data[1], ==, 0x00);
  g_assert_cmpint (out->data[2], ==, 0x00);
  g_assert_cmpint (out->data[3], ==, 0x01);
  g_assert_cmpint (out->data[4], ==, 0x00);
  g_assert_cmpint (out->data[5], ==, 0x00);
  g_assert_cmpint (out->data[6], ==, 0x00);
  g_assert_cmpint (out->data[7], ==, 0x02);

  g_byte_array_unref (out);
}

static void
test_util_deserialize_boolean ()
{
  GByteArray *in = g_byte_array_new ();

  g_byte_array_append (in, "\001", 1);

  g_assert (gzochid_util_deserialize_boolean (in));

  g_byte_array_append (in, "\000", 1);
  g_assert (!gzochid_util_deserialize_boolean (in));
  
  g_byte_array_unref (in);
}

static void
test_util_deserialize_int ()
{
  GByteArray *in = g_byte_array_new ();

  g_byte_array_append (in, "\001\002\003\004", 4);
  g_assert_cmpint (gzochid_util_deserialize_int (in), ==, 16909060);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_uint64 ()
{
  GByteArray *in = g_byte_array_new ();

  g_byte_array_append (in, "\001\002\003\004\005\006\007\008", 8);
  g_assert_cmpint (gzochid_util_deserialize_uint64 (in), ==, 72623859790382848);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_bytes ()
{
  GByteArray *in = g_byte_array_new ();
  int len = 0;
  unsigned char *bytes = NULL;

  g_byte_array_append (in, "\000\000\000\003\144\145\146", 7);

  bytes = gzochid_util_deserialize_bytes (in, &len);
  
  g_assert_cmpint (len, ==, 3);
  g_assert_cmpint (bytes[0], ==, 0x64);
  g_assert_cmpint (bytes[1], ==, 0x65);
  g_assert_cmpint (bytes[2], ==, 0x66);

  free (bytes);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_string ()
{
  GByteArray *in = g_byte_array_new ();
  char *str = NULL;

  g_byte_array_append (in, "\000\000\000\020", 4);
  g_byte_array_append (in, "Goodbye, world!", 16);

  str = gzochid_util_deserialize_string (in);
  g_assert_cmpstr (str, ==, "Goodbye, world!");

  free (str);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_list ()
{
  GByteArray *in = g_byte_array_new ();
  GList *lst = NULL;
  
  g_byte_array_append
    (in, "\000\000\000\003"
     "\000\000\000\002\144\000"
     "\000\000\000\002\145\000"
     "\000\000\000\002\146\000", 22);

  lst = gzochid_util_deserialize_list 
    (in, (gpointer (*) (GByteArray *)) gzochid_util_deserialize_string);
  
  g_assert_cmpstr ((char *) g_list_nth_data (lst, 0), ==, "d");
  g_assert_cmpstr ((char *) g_list_nth_data (lst, 1), ==, "e");
  g_assert_cmpstr ((char *) g_list_nth_data (lst, 2), ==, "f");

  g_list_free (lst);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_sequence ()
{
  GByteArray *in = g_byte_array_new ();
  GSequence *seq = NULL;
  GSequenceIter *iter = NULL;

  g_byte_array_append
    (in, "\000\000\000\003"
     "\000\000\000\002\144\000"
     "\000\000\000\002\145\000"
     "\000\000\000\002\146\000", 22);

  seq = gzochid_util_deserialize_sequence
    (in, (gpointer (*) (GByteArray *)) gzochid_util_deserialize_string, free);
  iter = g_sequence_get_begin_iter (seq);
  
  g_assert_cmpstr ((char *) g_sequence_get (iter), ==, "d");
  iter = g_sequence_iter_next (iter);
  g_assert (!g_sequence_iter_is_end (iter));
  g_assert_cmpstr ((char *) g_sequence_get (iter), ==, "e");
  iter = g_sequence_iter_next (iter);
  g_assert (!g_sequence_iter_is_end (iter));
  g_assert_cmpstr ((char *) g_sequence_get (iter), ==, "f");
  iter = g_sequence_iter_next (iter);
  g_assert (g_sequence_iter_is_end (iter));

  g_sequence_free (seq);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_hash_table ()
{
  GByteArray *in = g_byte_array_new ();
  GHashTable *ht = NULL;
  
  g_byte_array_append
    (in, "\000\000\000\001"
     "\000\000\000\004\142\141\172\000"
     "\000\000\000\004\161\165\170\000", 20);

  ht = gzochid_util_deserialize_hash_table 
    (in, g_str_hash, g_str_equal,
     (gpointer (*) (GByteArray *)) gzochid_util_deserialize_string,
     (gpointer (*) (GByteArray *)) gzochid_util_deserialize_string);

  g_assert_cmpint (g_hash_table_size (ht), ==, 1);
  g_assert (g_hash_table_contains (ht, "baz"));
  g_assert_cmpstr (g_hash_table_lookup (ht, "baz"), ==, "qux");

  g_hash_table_destroy (ht);
  g_byte_array_unref (in);
}

static void
test_util_deserialize_timeval ()
{
  GByteArray *in = g_byte_array_new ();
  struct timeval tv;

  g_byte_array_append (in, "\000\000\000\003\000\000\000\004", 8);
  tv = gzochid_util_deserialize_timeval (in);
  
  g_assert_cmpint (tv.tv_sec, ==, 3);
  g_assert_cmpint (tv.tv_usec, ==, 4);

  g_byte_array_unref (in);
}

static void
test_util_guint64_data_compare ()
{
  guint64 zero = 0;
  guint64 one = 1;

  g_assert_cmpint (gzochid_util_guint64_data_compare (&zero, &one, NULL), <, 0);
  g_assert_cmpint (gzochid_util_guint64_data_compare (&one, &one, NULL), ==, 0);
  g_assert_cmpint (gzochid_util_guint64_data_compare (&one, &zero, NULL), >, 0);
}

static void
test_util_bytes_compare_null_first_simple ()
{
  GBytes *b1 = g_bytes_new_static ("foo1", 5);
  GBytes *b2 = g_bytes_new_static ("foo2", 5);

  g_assert_cmpint (gzochid_util_bytes_compare_null_first (b1, b2), <, 0);

  g_bytes_unref (b1);
  g_bytes_unref (b2);
}

static void
test_util_bytes_compare_null_first_null ()
{
  GBytes *b1 = g_bytes_new_static ("foo1", 5);

  g_assert_cmpint (gzochid_util_bytes_compare_null_first (b1, NULL), >, 0);

  g_bytes_unref (b1);
}

static void
test_util_bytes_compare_null_last_simple ()
{
  GBytes *b1 = g_bytes_new_static ("foo1", 5);
  GBytes *b2 = g_bytes_new_static ("foo2", 5);

  g_assert_cmpint (gzochid_util_bytes_compare_null_last (b1, b2), <, 0);

  g_bytes_unref (b1);
  g_bytes_unref (b2);
}

static void
test_util_bytes_compare_null_last_null ()
{
  GBytes *b1 = g_bytes_new_static ("foo1", 5);

  g_assert_cmpint (gzochid_util_bytes_compare_null_first (b1, NULL), <, 0);

  g_bytes_unref (b1);
}

static void
test_util_encode_oid_simple ()
{
  guint64 encoded_one_zero_zero = gzochid_util_encode_oid (256);
  guint64 encoded_two = gzochid_util_encode_oid (2);

  GBytes *one_zero_zero_bytes = g_bytes_new_static
    (&encoded_one_zero_zero, sizeof (guint64));
  GBytes *two_bytes = g_bytes_new_static (&encoded_two, sizeof (guint64));

  g_assert_cmpint (g_bytes_compare (two_bytes, one_zero_zero_bytes), <, 0);
  
  g_bytes_unref (one_zero_zero_bytes);
  g_bytes_unref (two_bytes);
}

static void
test_util_decode_oid_simple ()
{
  GBytes *one_zero_zero_bytes = g_bytes_new_static
    ("\x00\x00\x00\x00\x00\x01\x00\x00", sizeof (guint64));
  GBytes *two_bytes = g_bytes_new_static
    ("\x00\x00\x00\x00\x00\x00\x00\x02", sizeof (guint64));

  guint64 one_zero_zero = 0;
  guint64 two = 0;

  memcpy
    (&one_zero_zero, g_bytes_get_data (one_zero_zero_bytes, NULL),
     sizeof (guint64));
  memcpy (&two, g_bytes_get_data (two_bytes, NULL), sizeof (guint64));

  g_assert_cmpint
    (gzochid_util_decode_oid (two), <, gzochid_util_decode_oid (one_zero_zero));
  
  g_bytes_unref (one_zero_zero_bytes);
  g_bytes_unref (two_bytes);
}

static gpointer
copy_int (gconstpointer src, gpointer data)
{
  gint *x = malloc (sizeof (gint));
  const gint *y = src;
  
  *x = *y;
  
  return x;
}

static void
test_util_list_copy_deep ()
{
  gint *x = malloc (sizeof (gint));
  gint *y = NULL;
  
  GList *l1 = NULL;
  GList *l2 = NULL;

  *x = 1;
  
  l1 = g_list_append (NULL, x);
  l2 = gzochid_util_list_copy_deep (l1, copy_int, NULL);

  y = l2->data;
  
  g_assert_cmpint (*x, ==, *y);

  *x = 2;

  g_assert_cmpint (*x, !=, *y);
  
  g_list_free_full (l1, g_free);
  g_list_free_full (l2, g_free);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/util/serialize/boolean", test_util_serialize_boolean);
  g_test_add_func ("/util/serialize/int", test_util_serialize_int);
  g_test_add_func ("/util/serialize/uint64", test_util_serialize_uint64);
  g_test_add_func ("/util/serialize/bytes", test_util_serialize_bytes);
  g_test_add_func ("/util/serialize/string", test_util_serialize_string);
  g_test_add_func ("/util/serialize/list", test_util_serialize_list);
  g_test_add_func ("/util/serialize/sequence", test_util_serialize_sequence);
  g_test_add_func 
    ("/util/serialize/hash_table", test_util_serialize_hash_table);
  g_test_add_func ("/util/serialize/timeval", test_util_serialize_timeval);

  g_test_add_func ("/util/deserialize/boolean", test_util_deserialize_boolean);
  g_test_add_func ("/util/deserialize/int", test_util_deserialize_int);
  g_test_add_func ("/util/deserialize/uint64", test_util_deserialize_uint64);
  g_test_add_func ("/util/deserialize/bytes", test_util_deserialize_bytes);
  g_test_add_func ("/util/deserialize/string", test_util_deserialize_string);
  g_test_add_func ("/util/deserialize/list", test_util_deserialize_list);
  g_test_add_func 
    ("/util/deserialize/sequence", test_util_deserialize_sequence);
  g_test_add_func 
    ("/util/deserialize/hash_table", test_util_deserialize_hash_table);
  g_test_add_func ("/util/deserialize/timeval", test_util_deserialize_timeval);

  g_test_add_func
    ("/util/guint64_data_compare/simple", test_util_guint64_data_compare);
  g_test_add_func
    ("/util/bytes_compare_null_first/simple",
     test_util_bytes_compare_null_first_simple);
  g_test_add_func
    ("/util/bytes_compare_null_first/null",
     test_util_bytes_compare_null_first_null);
  g_test_add_func
    ("/util/bytes_compare_null_last/simple",
     test_util_bytes_compare_null_last_simple);
  g_test_add_func
    ("/util/bytes_compare_null_last/null",
     test_util_bytes_compare_null_last_simple);

  g_test_add_func ("/util/encode_oid/simple", test_util_encode_oid_simple);
  g_test_add_func ("/util/decode_oid/simple", test_util_decode_oid_simple);

  g_test_add_func ("/util/list_copy_deep/simple", test_util_list_copy_deep);
  
  return g_test_run ();
}
