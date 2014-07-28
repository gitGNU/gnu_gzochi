/* mock-txlog.c: Test-time replacements for txlog.c routines.
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

#include <stdarg.h>
#include <stdio.h>

#include "../app.h"

void gzochid_tx_log
(gzochid_application_context *context, int priority, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}

void gzochid_tx_err (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}

void gzochid_tx_warning (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}

void gzochid_tx_notice (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}

void gzochid_tx_info (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}

void gzochid_tx_debug (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  vprintf (msg, args);
  va_end (args);
}
