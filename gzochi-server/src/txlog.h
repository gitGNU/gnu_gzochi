/* txlog.h: Prototypes and declarations for txlog.c
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

#ifndef GZOCHID_TX_LOG_H
#define GZOCHID_TX_LOG_H

#include "app.h"

void gzochid_tx_err (gzochid_application_context *, char *, ...);
void gzochid_tx_warning (gzochid_application_context *, char *, ...);
void gzochid_tx_notice (gzochid_application_context *, char *, ...);
void gzochid_tx_info (gzochid_application_context *, char *, ...);
void gzochid_tx_debug (gzochid_application_context *, char *, ...);

#endif /* GZOCHID_TX_LOG_H */
