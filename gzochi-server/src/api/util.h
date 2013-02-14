/* util.h: Prototypes and declarations for util.c
 * Copyright (C) 2013 Julian Graham
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

#ifndef GZOCHID_API_UTIL_H
#define GZOCHID_API_UTIL_H

#include "../app.h"

gzochid_application_context *gzochid_api_ensure_current_application_context 
(void);

void gzochid_api_check_transaction (void);

void gzochid_api_util_init (void);

#endif /* GZOCHID_API_UTIL_H */
