/* protocol-common.h: Prototypes and declarations for protocol-common.c
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

#ifndef GZOCHID_PROTOCOL_COMMON_H
#define GZOCHID_PROTOCOL_COMMON_H

#include <glib.h>
#include <stddef.h>

/*
  Finds the bounds of the `NULL'-terminated string that begins at `bytes', 
  returning a pointer to that string and setting `str_len', if it is specified,
  to the length of the string *including* the `NULL' terminator. Returns `NULL'
  if the string is not `NULL'-terminated. 
*/

const char *gzochid_protocol_read_str (const unsigned char *, const size_t,
				       size_t *);

/*
  Reads the run length encoded (via a two-byte big-endian prefix) byte buffer
  and returns it. 
*/

GBytes *gzochid_protocol_read_bytes (const unsigned char *, const size_t);


#endif /* GZOCHID_PROTOCOL_COMMON_H */
