/* io.h: Prototypes and declarations for io.c
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

#ifndef GZOCHI_COMMON_IO_H
#define GZOCHI_COMMON_IO_H

int gzochi_common_io_read_int (char *, int);
short gzochi_common_io_read_short (char *, int);

void gzochi_common_io_write_int (int, char *, int);
void gzochi_common_io_write_short (short, char *, int);

#endif /* GZOCHI_COMMON_IO_H */
