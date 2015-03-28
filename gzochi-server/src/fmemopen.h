/* fmemopen.h: Prototypes and declarations for fmemopen.c
 * Copyright (C) 2015 Julian Graham
 *
 * Ported from
 * https://github.com/NimbusKit/memorymapping/blob/master/src/fmemopen.c
 * Originally ported from
 * https://github.com/ingenuitas/python-tesseract/blob/master/fmemopen.c
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

#ifndef GZOCHID_FMEMOPEN_H
#define GZOCHID_FMEMOPEN_H

#include <stddef.h>
#include <stdio.h>

/**
 * A BSD port of the fmemopen Linux method using funopen.
 *
 * man docs for fmemopen: http://linux.die.net/man/3/fmemopen
 *
 * man docs for funopen:
 * https://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man3/funopen.3.html
 *
 * This method is ported from Jeff Verkoeyen's implementation for Nimbus, which
 * was in turn ported from ingenuitas' python-tesseract project.
 *
 * You must call fclose on the returned file pointer or memory will be leaked.
 *
 * @param buf The data that will be used to back the FILE* methods. Must be at
 *            least @c size bytes.
 * @param size The size of the @c buf data.
 * @param mode The permitted stream operation modes.
 * @return A pointer that can be used in the fread/fwrite/fseek/fclose family 
 *         of methods. If a failure occurred NULL will be returned.
 */
FILE *fmemopen (void *buf, size_t size, const char *mode);

#endif /* GZOCHID_FMEMOPEN_H */
