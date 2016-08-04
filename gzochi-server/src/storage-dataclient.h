/* storage-dataclient.h: Prototypes and decalarations for storage-dataclient.c
 * Copyright (C) 2016 Julian Graham
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

#ifndef GZOCHID_STORAGE_DATACLIENT_H
#define GZOCHID_STORAGE_DATACLIENT_H

#include <glib.h>

#include "dataclient.h"
#include "gzochid-storage.h"

extern gzochid_storage_engine_interface
gzochid_storage_engine_interface_dataclient;

void gzochid_dataclient_storage_context_set_dataclient
(gzochid_storage_context *, GzochidDataClient *);

void gzochid_dataclient_storage_release_lock (gzochid_storage_store *,
					      GBytes *);

#endif /* GZOCHID_STORAGE_DATACLIENT_H */
