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
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "util.h"

static void
test_util_serialize_boolean ()
{
  GString *out = g_string_new ("");

  gzochid_util_serialize_boolean (TRUE, out);
  g_assert_cmpint (out->len, ==, 1);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 1);
  g_string_erase (out, 0, 1);

  gzochid_util_serialize_boolean (FALSE, out);
  g_assert_cmpint (out->len, ==, 1);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0);

  g_string_free (out, FALSE);
}

static void
test_util_serialize_int ()
{
  GString *out = g_string_new ("");
  gzochid_util_serialize_int (1234, out);
  g_assert_cmpint (out->len, ==, 4);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x04);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0xd2);
  g_string_free (out, FALSE);
}

static void
test_util_serialize_mpz ()
{
  GString *out = g_string_new ("");
  char *str = NULL;
  mpz_t i;

  mpz_init (i);
  mpz_set_si (i, -100);
  str = mpz_get_str (NULL, 16, i);
  gzochid_util_serialize_mpz (i, out);

  g_assert_cmpint (out->len, ==, 8);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x04);
  g_assert_cmpstr (str, ==, out->str + 4);

  free (str);
  mpz_clear (i);
  g_string_free (out, FALSE);
}

static void
test_util_serialize_bytes ()
{
  GString *out = g_string_new ("");
  unsigned char bytes[3] = { 'a', 'b', 'c' };

  gzochid_util_serialize_bytes (bytes, 3, out);
  g_assert_cmpint (out->len, ==, 7);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x03);
  g_assert_cmpint ((unsigned char) out->str[4], ==, 0x61);
  g_assert_cmpint ((unsigned char) out->str[5], ==, 0x62);
  g_assert_cmpint ((unsigned char) out->str[6], ==, 0x63);

  g_string_free (out, FALSE);
}

static void
test_util_serialize_string ()
{
  GString *out = g_string_new ("");

  gzochid_util_serialize_string ("Hello, world!", out);
  g_assert_cmpint (out->len, ==, 18);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x0e);

  g_assert_cmpstr (out->str + 4, ==, "Hello, world!");

  g_string_free (out, FALSE);
}

static void
test_util_serialize_list ()
{
  GString *out = g_string_new ("");
  GList *lst = g_list_append 
    (g_list_append (g_list_append (NULL, "a"), "b"), "c");

  gzochid_util_serialize_list 
    (lst, (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);

  g_assert_cmpint (out->len, ==, 22);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x03);
  g_assert_cmpint ((unsigned char) out->str[4], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[5], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[6], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[7], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[8], ==, 0x61);
  g_assert_cmpint ((unsigned char) out->str[9], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[10], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[11], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[12], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[13], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[14], ==, 0x62);
  g_assert_cmpint ((unsigned char) out->str[15], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[16], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[17], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[18], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[19], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[20], ==, 0x63);
  g_assert_cmpint ((unsigned char) out->str[21], ==, 0x00);

  g_list_free (lst);
  g_string_free (out, FALSE);
}

static void
test_util_serialize_sequence ()
{
  GString *out = g_string_new ("");
  GSequence *seq = g_sequence_new (NULL);

  g_sequence_append (seq, "a");
  g_sequence_append (seq, "b");
  g_sequence_append (seq, "c");
  
  gzochid_util_serialize_sequence
    (seq, (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);

  g_assert_cmpint (out->len, ==, 22);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x03);
  g_assert_cmpint ((unsigned char) out->str[4], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[5], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[6], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[7], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[8], ==, 0x61);
  g_assert_cmpint ((unsigned char) out->str[9], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[10], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[11], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[12], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[13], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[14], ==, 0x62);
  g_assert_cmpint ((unsigned char) out->str[15], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[16], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[17], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[18], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[19], ==, 0x02);
  g_assert_cmpint ((unsigned char) out->str[20], ==, 0x63);
  g_assert_cmpint ((unsigned char) out->str[21], ==, 0x00);

  g_sequence_free (seq);
  g_string_free (out, FALSE);
}

static void
test_util_serialize_hash_table ()
{
  GString *out = g_string_new ("");
  GHashTable *ht = g_hash_table_new (g_str_hash, g_str_equal);

  g_hash_table_insert (ht, "foo", "bar");

  gzochid_util_serialize_hash_table
    (ht, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, 
     out);

  g_assert_cmpint (out->len, ==, 20);
  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x01);
  g_assert_cmpint ((unsigned char) out->str[4], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[5], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[6], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[7], ==, 0x04);
  g_assert_cmpint ((unsigned char) out->str[8], ==, 0x66);
  g_assert_cmpint ((unsigned char) out->str[9], ==, 0x6f);
  g_assert_cmpint ((unsigned char) out->str[10], ==, 0x6f);
  g_assert_cmpint ((unsigned char) out->str[11], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[12], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[13], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[14], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[15], ==, 0x04);
  g_assert_cmpint ((unsigned char) out->str[16], ==, 0x62);
  g_assert_cmpint ((unsigned char) out->str[17], ==, 0x61);
  g_assert_cmpint ((unsigned char) out->str[18], ==, 0x72);
  g_assert_cmpint ((unsigned char) out->str[19], ==, 0x00);

  g_hash_table_destroy (ht);
  g_string_free (out, FALSE);
}

static void
test_util_serialize_timeval ()
{
  GString *out = g_string_new ("");
  struct timeval tv = { 1, 2 };

  gzochid_util_serialize_timeval (tv, out);

  g_assert_cmpint ((unsigned char) out->str[0], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[1], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[2], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[3], ==, 0x01);
  g_assert_cmpint ((unsigned char) out->str[4], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[5], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[6], ==, 0x00);
  g_assert_cmpint ((unsigned char) out->str[7], ==, 0x02);

  g_string_free (out, FALSE);
}

static void
test_util_deserialize_boolean ()
{
  GString *in = g_string_new_len ("\001", 1);

  g_assert (gzochid_util_deserialize_boolean (in));

  g_string_append_len (in, "\000", 1);
  g_assert (!gzochid_util_deserialize_boolean (in));
  
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_int ()
{
  GString *in = g_string_new_len ("\001\002\003\004", 4);
  g_assert_cmpint (gzochid_util_deserialize_int (in), ==, 16909060);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_mpz ()
{
  GString *in = g_string_new_len ("\000\000\000\003\066\064\000", 7);
  mpz_t i;

  mpz_init (i);
  gzochid_util_deserialize_mpz (in, i);
  g_assert (mpz_cmp_ui (i, 100) == 0);

  mpz_clear (i);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_bytes ()
{
  GString *in = g_string_new_len ("\000\000\000\003\144\145\146", 7);
  int len = 0;
  unsigned char *bytes = gzochid_util_deserialize_bytes (in, &len);

  g_assert_cmpint (len, ==, 3);
  g_assert_cmpint (bytes[0], ==, 0x64);
  g_assert_cmpint (bytes[1], ==, 0x65);
  g_assert_cmpint (bytes[2], ==, 0x66);

  free (bytes);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_string ()
{
  GString *in = g_string_new_len ("\000\000\000\020", 4);
  char *str = NULL;

  g_string_append_len (in, "Goodbye, world!", 16);

  str = gzochid_util_deserialize_string (in);
  g_assert_cmpstr (str, ==, "Goodbye, world!");

  free (str);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_list ()
{
  GString *in = g_string_new_len 
    ("\000\000\000\003"
     "\000\000\000\002\144\000"
     "\000\000\000\002\145\000"
     "\000\000\000\002\146\000", 22);
  GList *lst = gzochid_util_deserialize_list 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string);

  g_assert_cmpstr ((char *) g_list_nth_data (lst, 0), ==, "d");
  g_assert_cmpstr ((char *) g_list_nth_data (lst, 1), ==, "e");
  g_assert_cmpstr ((char *) g_list_nth_data (lst, 2), ==, "f");

  g_list_free (lst);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_sequence ()
{
  GString *in = g_string_new_len 
    ("\000\000\000\003"
     "\000\000\000\002\144\000"
     "\000\000\000\002\145\000"
     "\000\000\000\002\146\000", 22);
  GSequence *seq = gzochid_util_deserialize_sequence
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string, free);
  GSequenceIter *iter = g_sequence_get_begin_iter (seq);
  
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
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_hash_table ()
{
  GString *in = g_string_new_len 
    ("\000\000\000\001"
     "\000\000\000\004\142\141\172\000"
     "\000\000\000\004\161\165\170\000", 20);
  GHashTable *ht = gzochid_util_deserialize_hash_table 
    (in, g_str_hash, g_str_equal,
     (gpointer (*) (GString *)) gzochid_util_deserialize_string,
     (gpointer (*) (GString *)) gzochid_util_deserialize_string);

  g_assert_cmpint (g_hash_table_size (ht), ==, 1);
  g_assert (g_hash_table_contains (ht, "baz"));
  g_assert_cmpstr (g_hash_table_lookup (ht, "baz"), ==, "qux");

  g_hash_table_destroy (ht);
  g_string_free (in, FALSE);
}

static void
test_util_deserialize_timeval ()
{
  GString *in = g_string_new_len ("\000\000\000\003\000\000\000\004", 8);
  struct timeval tv = gzochid_util_deserialize_timeval (in);
  
  g_assert_cmpint (tv.tv_sec, ==, 3);
  g_assert_cmpint (tv.tv_usec, ==, 4);

  g_string_free (in, FALSE);
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

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/util/serialize/boolean", test_util_serialize_boolean);
  g_test_add_func ("/util/serialize/int", test_util_serialize_int);
  g_test_add_func ("/util/serialize/mpz", test_util_serialize_mpz);
  g_test_add_func ("/util/serialize/bytes", test_util_serialize_bytes);
  g_test_add_func ("/util/serialize/string", test_util_serialize_string);
  g_test_add_func ("/util/serialize/list", test_util_serialize_list);
  g_test_add_func ("/util/serialize/sequence", test_util_serialize_sequence);
  g_test_add_func 
    ("/util/serialize/hash_table", test_util_serialize_hash_table);
  g_test_add_func ("/util/serialize/timeval", test_util_serialize_timeval);

  g_test_add_func ("/util/deserialize/boolean", test_util_deserialize_boolean);
  g_test_add_func ("/util/deserialize/int", test_util_deserialize_int);
  g_test_add_func ("/util/deserialize/mpz", test_util_deserialize_mpz);
  g_test_add_func ("/util/deserialize/bytes", test_util_deserialize_bytes);
  g_test_add_func ("/util/deserialize/string", test_util_deserialize_string);
  g_test_add_func ("/util/deserialize/list", test_util_deserialize_list);
  g_test_add_func 
    ("/util/deserialize/sequence", test_util_deserialize_sequence);
  g_test_add_func 
    ("/util/deserialize/hash_table", test_util_deserialize_hash_table);
  g_test_add_func ("/util/deserialize/timeval", test_util_deserialize_timeval);

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
  
  return g_test_run ();
}
