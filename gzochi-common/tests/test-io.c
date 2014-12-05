/* test-io.c: Test routines for io.c in libgzochicommon.
 * Copyright (C) 2014 Julian Graham
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
#include <stdio.h>

#include "io.h"

static void test_read_int (void)
{
  unsigned char i[4] = { 0x3a, 0xde, 0x68, 0xb1 };

  fprintf (stderr, "/io/read-int: ");

  assert (gzochi_common_io_read_int (i, 0) == 987654321);

  fprintf (stderr, "OK\n");
}

static void test_read_short (void)
{
  unsigned char i[2] = { 0x10, 0xe1 };

  fprintf (stderr, "/io/read-short: ");

  assert (gzochi_common_io_read_short (i, 0) == 4321);

  fprintf (stderr, "OK\n");
}

static void test_write_int (void)
{
  unsigned char i[4] = { 0x0, 0x0, 0x0, 0x0 };

  fprintf (stderr, "/io/write-int: "); 

  gzochi_common_io_write_int (123456789, i, 0);

  assert (i[0] == 0x07);
  assert (i[1] == 0x5b);
  assert (i[2] == 0xcd);
  assert (i[3] == 0x15);  

  fprintf (stderr, "OK\n");
}

static void test_write_short (void)
{
  unsigned char s[2] = { 0x0, 0x0 };

  fprintf (stderr, "/io/write-short: "); 

  gzochi_common_io_write_short (1234, s, 0);

  assert (s[0] == 0x04);
  assert (s[1] == 0xd2);

  fprintf (stderr, "OK\n");
}

int main (int argc, char *argv[])
{
  test_read_int ();
  test_read_short ();
  test_write_int ();
  test_write_short ();

  return 0;
}
