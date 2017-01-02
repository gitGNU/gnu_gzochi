/* test-protocol-common.c: Test routines for protocol-common.c in gzochid.
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
#include <stddef.h>

#include "protocol-common.h"

static void
test_read_str ()
{
  size_t len = 0;
  
  g_assert_cmpstr
    (gzochid_protocol_read_str ("foo\x00\x01\x02\x03", 8, &len), ==, "foo");
  g_assert_cmpint (len, ==, 4);
}

static void
test_read_str_error ()
{
  g_assert (gzochid_protocol_read_str ("foo", 3, NULL) == NULL);
}

static void
test_read_bytes ()
{
  GBytes *expected = g_bytes_new_static ("\x01\x02\x03", 3);
  GBytes *actual = gzochid_protocol_read_bytes ("\x00\x03\x01\x02\x03", 5);

  g_assert (g_bytes_equal (expected, actual));
  
  g_bytes_unref (expected);
  g_bytes_unref (actual);
}

static void
test_read_bytes_error ()
{
  g_assert (gzochid_protocol_read_bytes ("\x00\x03\x01\0x02", 4) == NULL);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/protocol-common/read-str", test_read_str);
  g_test_add_func ("/protocol-common/read-str/error", test_read_str_error);
  g_test_add_func ("/protocol-common/read-bytes", test_read_bytes);
  g_test_add_func ("/protocol-common/read-bytes/error", test_read_bytes_error);
  
  return g_test_run ();
}
