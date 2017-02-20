/* util.c: Assorted utility routines for gzochid
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

#include <ctype.h>
#include <glib.h>
#include <gzochi-common.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

void
gzochid_util_serialize_boolean (gboolean bool, GByteArray *out)
{
  unsigned char bool_str[1];
  
  bool_str[0] = bool ? 0x1 : 0x0;
  g_byte_array_append (out, bool_str, 1);
}

void
gzochid_util_serialize_int (int n, GByteArray *out)
{
  unsigned char n_str[4];

  gzochi_common_io_write_int (n, n_str, 0);
  g_byte_array_append (out, n_str, 4);
}

void
gzochid_util_serialize_bytes (unsigned char *str, int len, GByteArray *out)
{
  gzochid_util_serialize_int (len, out);
  g_byte_array_append (out, str, len);
}

void
gzochid_util_serialize_string (char *str, GByteArray *out)
{
  gzochid_util_serialize_bytes ((unsigned char *) str, strlen (str) + 1, out);
}

void
gzochid_util_serialize_uint64 (guint64 oid, GByteArray *out)
{
  unsigned char n_str[8];

  gzochi_common_io_write_long (oid, n_str, 0);
  g_byte_array_append (out, n_str, 8);
}

void
gzochid_util_serialize_oid (guint64 oid, GByteArray *out)
{
  gzochid_util_serialize_uint64 (oid, out);
}

void
gzochid_util_serialize_list (GList *list,
			     void (*serializer) (gpointer, GByteArray *),
			     GByteArray *out)
{
  unsigned char len_str[4];
  int len = g_list_length (list);
  GList *list_ptr = list;

  gzochi_common_io_write_int (len, len_str, 0);
  g_byte_array_append (out, len_str, 4);
  
  while (list_ptr != NULL)
    {
      serializer (list_ptr->data, out);
      list_ptr = list_ptr->next;
    }
}

void
gzochid_util_serialize_sequence (GSequence *sequence,
				 void (*serializer) (gpointer, GByteArray *),
				 GByteArray *out)
{
  unsigned char len_str[4];
  int len = g_sequence_get_length (sequence);
  GSequenceIter *iter = g_sequence_get_begin_iter (sequence);
  
  gzochi_common_io_write_int (len, len_str, 0);
  g_byte_array_append (out, len_str, 4);

  while (!g_sequence_iter_is_end (iter))
    {
      serializer (g_sequence_get (iter), out);
      iter = g_sequence_iter_next (iter);
    }
}

void
gzochid_util_serialize_hash_table
(GHashTable *hashtable, void (*key_serializer) (gpointer, GByteArray *),
 void (*value_serializer) (gpointer, GByteArray *), GByteArray *out)
{
  unsigned char len_str[4];
  int len = g_hash_table_size (hashtable);
  GHashTableIter iter;
  gpointer key, value;

  gzochi_common_io_write_int (len, len_str, 0);
  g_byte_array_append (out, len_str, 4);

  g_hash_table_iter_init (&iter, hashtable);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      key_serializer (key, out);
      value_serializer (value, out);
    }
}

void gzochid_util_serialize_timeval (struct timeval tv, GByteArray *out)
{
  unsigned char str[4];

  gzochi_common_io_write_int (tv.tv_sec, str, 0);
  g_byte_array_append (out, str, 4);

  gzochi_common_io_write_int (tv.tv_usec, str, 0);
  g_byte_array_append (out, str, 4);
}

gboolean
gzochid_util_deserialize_boolean (GByteArray *in)
{
  gboolean ret = in->data[0] == 0x1 ? TRUE : FALSE;
  g_byte_array_remove_index (in, 0);
  return ret;
}

int
gzochid_util_deserialize_int (GByteArray *in)
{
  int ret = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);
  return ret;
}

unsigned char *
gzochid_util_deserialize_bytes (GByteArray *in, int *len)
{
  int str_len = gzochi_common_io_read_int (in->data, 0);
  unsigned char *i_str = malloc (sizeof (unsigned char) * str_len);

  memcpy (i_str, in->data + 4, str_len);
  g_byte_array_remove_range (in, 0, 4 + str_len);

  if (len != NULL)
    *len = str_len;

  return i_str;
}

char *
gzochid_util_deserialize_string (GByteArray *in)
{
  return (char *) gzochid_util_deserialize_bytes (in, NULL);
}

guint64
gzochid_util_deserialize_uint64 (GByteArray *in)
{
  guint64 ret = gzochi_common_io_read_long (in->data, 0);
  g_byte_array_remove_range (in, 0, sizeof (guint64));
  return ret;
}

guint64
gzochid_util_deserialize_oid (GByteArray *in)
{
  return gzochid_util_deserialize_uint64 (in);
}

GList *
gzochid_util_deserialize_list (GByteArray *in,
			       gpointer (*deserializer) (GByteArray *))
{
  GList *ret = NULL;
  int len = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);

  while (len > 0)
    {
      ret = g_list_append (ret, deserializer (in));
      len--;
    }

  return ret;
}

GSequence *
gzochid_util_deserialize_sequence (GByteArray *in,
				   gpointer (*deserializer) (GByteArray *),
				   GDestroyNotify destroy_fn)
{
  GSequence *ret = g_sequence_new (destroy_fn);
  int len = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);

  while (len > 0)
    {
      g_sequence_append (ret, deserializer (in));
      len--;
    }

  return ret;
}

GHashTable *
gzochid_util_deserialize_hash_table (GByteArray *in, GHashFunc hash_func,
				     GEqualFunc key_equal_func, 
				     gpointer (*kd) (GByteArray *), 
				     gpointer (*vd) (GByteArray *))
{
  GHashTable *ret = g_hash_table_new (hash_func, key_equal_func);
  int len = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);

  while (len > 0)
    {
      gpointer key = kd (in);
      gpointer value = vd (in);

      g_hash_table_insert (ret, key, value);
      len--;
    }

  return ret;
}

struct timeval
gzochid_util_deserialize_timeval (GByteArray *in)
{
  struct timeval tv;

  tv.tv_sec = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);
  tv.tv_usec = gzochi_common_io_read_int (in->data, 0);
  g_byte_array_remove_range (in, 0, 4);

  return tv;
}

gint
gzochid_util_string_data_compare (gconstpointer a, gconstpointer b,
				  gpointer user_data)
{
  return g_strcmp0 ((const char *) a, (const char *) b);
}

gint
gzochid_util_guint64_data_compare (gconstpointer a, gconstpointer b,
				   gpointer user_data)
{
  const guint64 an = *(const guint64 *) a;
  const guint64 bn = *(const guint64 *) b;

  return an < bn ? -1 : an > bn ? 1 : 0;
}

gint
gzochid_util_bytes_compare_null_first (gconstpointer o1, gconstpointer o2)
{
  if (o1 == NULL)
    return o2 == NULL ? 0 : -1;
  else return o2 == NULL ? 1 : g_bytes_compare (o1, o2);
}

gint
gzochid_util_bytes_compare_null_last (gconstpointer o1, gconstpointer o2)
{
  if (o1 == NULL)
    return o2 == NULL ? 0 : 1;
  else return o2 == NULL ? -1 : g_bytes_compare (o1, o2);
}

guint64
gzochid_util_encode_oid (guint64 oid)
{
#if G_BYTE_ORDER == G_BIG_ENDIAN
  return oid;
#else
  return GUINT64_TO_BE (oid);
#endif
}

guint64
gzochid_util_decode_oid (guint64 oid)
{
#if G_BYTE_ORDER == G_BIG_ENDIAN
  return oid;
#else
  return GUINT64_FROM_BE (oid);
#endif
}

GList *
gzochid_util_list_copy_deep (GList *list, GCopyFunc func, gpointer user_data)
{
  GList *new_list = NULL;
  GList *list_ptr = list;

  while (list_ptr != NULL)
    {
      new_list = g_list_prepend (new_list, func (list_ptr->data, user_data));
      list_ptr = list_ptr->next;
    }

  return g_list_reverse (new_list);
}

/* Converts the lower 4 bits of the specified byte to its ASCII hexadecimal
   representation; i.e., 0-9a-f. */

static inline char
nibble_to_hex_char (unsigned char n)
{
  return n < 10 ? n + 48 : n + 87;
}

void
gzochid_util_format_bytes (GBytes *bytes, char *buf, size_t buf_len)
{
  size_t bytes_len = 0;
  int bytes_offset = 0, buf_offset = 0, max_offset = buf_len - 1;
  const unsigned char *bytes_data = g_bytes_get_data (bytes, &bytes_len);

  for (; bytes_offset < bytes_len && buf_offset < max_offset; bytes_offset++)
    {
      unsigned char b = bytes_data[bytes_offset];

      if (isgraph (b))
	{
	  if (bytes_offset == bytes_len - 1 || buf_offset < max_offset - 1)
	    buf[buf_offset++] = b;
	  else buf[buf_offset++] = '_';
	}
      else
	{
	  if (max_offset - buf_offset >= 4)
	    {
	      buf[buf_offset++] = '\\';
	      buf[buf_offset++] = 'x';
	      buf[buf_offset++] = nibble_to_hex_char (b / 16);
	      buf[buf_offset++] = nibble_to_hex_char (b % 16);
	    }
	  else
	    {
	      buf[buf_offset++] = '_';
	      break;
	    }
	}
    }
  
  buf[buf_offset] = 0;
}
