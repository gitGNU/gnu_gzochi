/* log.c: Log-writing routines for gzochid
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

#include <stdarg.h>
#include <stdio.h>
#include <syslog.h>

#include "log.h"

void gzochid_err (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vsyslog (LOG_ERR, msg, args);
  vfprintf (stderr, msg, args);
  fprintf (stderr, "\n");
  va_end (args);
}

void gzochid_warning (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vsyslog (LOG_WARNING, msg, args);
  vfprintf (stderr, msg, args);
  fprintf (stderr, "\n");
  va_end (args);
}

void gzochid_notice (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vsyslog (LOG_NOTICE, msg, args);
  vfprintf (stderr, msg, args);
  fprintf (stderr, "\n");
  va_end (args);
}

void gzochid_info (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vsyslog (LOG_INFO, msg, args);
  vfprintf (stderr, msg, args);
  fprintf (stderr, "\n");
  va_end (args);
}

void gzochid_debug (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vsyslog (LOG_DEBUG, msg, args);
  vfprintf (stderr, msg, args);
  fprintf (stderr, "\n");
  va_end (args);
}
