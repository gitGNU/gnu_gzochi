/* io.h: Prototypes and declarations for io.c
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

#ifndef GZOCHI_COMMON_IO_H
#define GZOCHI_COMMON_IO_H

/* Read a big-endian representation of a 64-bit integer from the specified byte
   array, starting from the specified offset. There must be at least 8 bytes
   in the array following the offset. */

long long gzochi_common_io_read_long (const unsigned char *, int);

int gzochi_common_io_read_int (const unsigned char *, int);
short gzochi_common_io_read_short (const unsigned char *, int);

/* Write the eight-byte, big-endian representation of the specified 64-bit 
   integer to the specified byte starting from the specified offset. There must
   be at least 8 bytes in the array following the offset. */

void gzochi_common_io_write_long (long long, unsigned char *, int);

void gzochi_common_io_write_int (int, unsigned char *, int);
void gzochi_common_io_write_short (short, unsigned char *, int);

#endif /* GZOCHI_COMMON_IO_H */
