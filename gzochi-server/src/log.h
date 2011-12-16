/* log.h: Prototypes and declarations for log.c
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

#ifndef GZOCHID_LOG_H
#define GZOCHID_LOG_H

#include <stdarg.h>

void gzochid_vlog (int, char *, va_list);
void gzochid_log (int, char *, ...);
void gzochid_err (char *, ...);
void gzochid_warning (char *, ...);
void gzochid_notice (char *, ...);
void gzochid_info (char *, ...);
void gzochid_debug (char *, ...);

#endif /* GZOCHID_LOG_H */
