/* io.h: Prototypes and declarations for io.c
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

#ifndef GZOCHID_IO_H
#define GZOCHID_IO_H

#include <glib.h>

#define GZOCHID_IO_ERROR gzochid_io_error_quark ()

GQuark gzochid_io_error_quark (void);

typedef enum
  {
    GZOCHID_IO_ERROR_SERIALIZATION
  }
  GzochidIoError;

struct _gzochid_application_context;

typedef struct _gzochid_io_serialization
{
  void (*serializer) 
    (struct _gzochid_application_context *, void *, GByteArray *, GError **);
  void *(*deserializer) 
    (struct _gzochid_application_context *, GByteArray *, GError **);
  void (*finalizer) (struct _gzochid_application_context *, void *);
} gzochid_io_serialization;

#endif /* GZOCHID_IO_H */
