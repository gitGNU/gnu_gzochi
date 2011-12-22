/* io.c: Common serialization routines for libgzochi-common
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

#include "io.h"

int gzochi_common_io_read_int (unsigned char *bytes, int offset)
{
  return (bytes[offset] << 24)
    + (bytes[offset + 1] << 16)
    + (bytes[offset + 2] << 8)
    + (bytes[offset + 3]);
}

short gzochi_common_io_read_short (unsigned char *bytes, int offset)
{
  return (bytes[offset] << 8) + bytes[offset + 1];
}

void gzochi_common_io_write_int (int val, unsigned char *bytes, int offset)
{
  bytes[offset] = val >> 24;
  bytes[offset + 1] = val >> 16 & 0xff;
  bytes[offset + 2] = val >> 8 & 0xff;
  bytes[offset + 3] = val & 0xff;
}

void gzochi_common_io_write_short (short val, unsigned char *bytes, int offset)
{
  bytes[offset] = val >> 8 & 0xff;
  bytes[offset + 1] = val & 0xff;
}
