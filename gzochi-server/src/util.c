/* util.c: Assorted utility routines for gzochid
 * Copyright (C) 2011 Julian Graham
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
#include <gzochi-common.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

void gzochid_util_serialize_boolean (gboolean bool, GString *out)
{
  char bool_str[1];
  
  bool_str[0] = bool ? 0x1 : 0x0;
  g_string_append_len (out, bool_str, 1);
}

void gzochid_util_serialize_bytes (char *str, int len, GString *out)
{
  char len_str[4];

  gzochi_common_io_write_int (len, len_str, 0);
  g_string_append_len (out, len_str, 4);
  g_string_append_len (out, str, len);
}

void gzochid_util_serialize_string (char *str, GString *out)
{
  gzochid_util_serialize_bytes (str, strlen (str), out);
}

void gzochid_util_serialize_mpz (mpz_t i, GString *out)
{
  char *i_str = mpz_get_str (NULL, 16, i);
  gzochid_util_serialize_string (i_str, out);
  free (i_str);
}

void gzochid_util_serialize_list 
(GList *list, void (*serializer) (gpointer, GString *),  GString *out)
{
  char len_str[4];
  int len = g_list_length (list);
  GList *list_ptr = list;

  gzochi_common_io_write_int (len, len_str, 0);
  g_string_append_len (out, len_str, 4);
  
  while (list_ptr != NULL)
    {
      serializer (list_ptr->data, out);
      list_ptr = list_ptr->next;
    }
}

gboolean gzochid_util_deserialize_boolean (GString *in)
{
  gboolean ret = in->str[0] == 0x1 ? TRUE : FALSE;
  g_string_erase (in, 0, 1);
  return ret;
}

char *gzochid_util_deserialize_bytes (GString *in, int *len)
{
  int str_len = gzochi_common_io_read_int (in->str, 0);
  char *i_str = strndup (in->str + 4, str_len);
  g_string_erase (in, 0, 4 + str_len);

  if (len != NULL)
    *len = str_len;

  return i_str;
}

char *gzochid_util_deserialize_string (GString *in)
{
  return gzochid_util_deserialize_bytes (in, NULL);
}

void gzochid_util_deserialize_mpz (GString *in, mpz_t o)
{
  char *o_str = gzochid_util_deserialize_string (in);
  mpz_set_str (o, o_str, 16);
  free (o_str);
}

GList *gzochid_util_deserialize_list 
(GString *in, gpointer (*deserializer) (GString *))
{
  GList *ret = NULL;
  int len = gzochi_common_io_read_int (in->str, 0);
  g_string_erase (in, 0, 4);

  while (len > 0)
    {
      ret = g_list_append (ret, deserializer (in));
      len--;
    }

  return ret;
}
