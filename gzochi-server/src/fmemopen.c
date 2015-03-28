/* fmemopen.c: funopen-based implementation of fmemopen for BSD compatibility
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

#ifndef HAVE_FMEMOPEN

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "fmemopen.h"

struct fmem
{
  size_t pos;
  size_t size;
  char *buffer;
};

typedef struct fmem fmem_t;

static int
readfn (void *handler, char *buf, int size)
{
  fmem_t *mem = handler;
  size_t available = mem->size - mem->pos;
  
  if (size > available)
    size = available;

  memcpy (buf, mem->buffer + mem->pos, sizeof (char) * size);
  mem->pos += size;

  return size;
}

static int
writefn (void *handler, const char *buf, int size)
{
  fmem_t *mem = handler;
  size_t available = mem->size - mem->pos;

  if (size > available)
    size = available;

  memcpy (mem->buffer + mem->pos, buf, sizeof (char) * size);
  mem->pos += size;

  return size;
}

static fpos_t
seekfn (void *handler, fpos_t offset, int whence)
{
  size_t pos;
  fmem_t *mem = handler;

  switch (whence)
    {
    case SEEK_SET:
      {
	if (offset >= 0)
	  pos = (size_t) offset;
	else pos = 0;

	break;
      }
    case SEEK_CUR:
      {
	if (offset >= 0 || (size_t) (-offset) <= mem->pos)
	  pos = mem->pos + (size_t)offset;
	else pos = 0;

	break;
      }
    case SEEK_END: pos = mem->size + (size_t) offset; break;
    default: return -1;
    }
  
  if (pos > mem->size)
    return -1;

  mem->pos = pos;
  return (fpos_t) pos;
}

static int
closefn (void *handler) 
{
  free (handler);
  return 0;
}

FILE *
fmemopen(void *buf, size_t size, const char *mode)
{
  fmem_t* mem = (fmem_t *) malloc (sizeof (fmem_t));
  
  memset (mem, 0, sizeof (fmem_t));

  mem->size = size;
  mem->buffer = buf;

  return funopen (mem, readfn, writefn, seekfn, closefn);
}

#endif /* HAVE_FMEMOPEN */
