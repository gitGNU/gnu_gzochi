/* log.h: Prototypes and declarations for log.c
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

#ifndef GZOCHID_LOG_H
#define GZOCHID_LOG_H

#include <glib.h>

#define GZOCHID_LOG_LEVEL_TRACE (1 << G_LOG_LEVEL_USER_SHIFT)

/* Convenience macro for logging at `GZOCHID_LOG_LEVEL_TRACE' with same 
   semantics as `g_debug', `g_warning', etc. */

#define gzochid_trace(...) g_log (G_LOG_DOMAIN,		   \
				  GZOCHID_LOG_LEVEL_TRACE, \
				  __VA_ARGS__)

void gzochid_install_log_handler (GLogLevelFlags);

/*
  Returns `TRUE' if visible log output will be produced by the default handler 
  (which must have been previously installed by `gzochid_install_log_handler') 
  for the specified log domain and log level.

  This function supports a special case in which it is expensive to generate the
  varargs values passed to `g_log' (or some variant thereof) such as is the case
  when rendering a `GBytes' with `gzochid_util_format_bytes'.
*/

gboolean gzochid_log_level_visible (const gchar *, GLogLevelFlags);

#endif /* GZOCHID_LOG_H */
