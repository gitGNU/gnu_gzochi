/* channel.h: Prototypes and declarations for channel.c
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

#ifndef GZOCHI_CHANNEL_H
#define GZOCHI_CHANNEL_H

#include "session.h"

gzochi_client_channel *gzochi_client_channel_new 
(gzochi_client_session *, char *, unsigned char *, int);
void gzochi_client_channel_free (gzochi_client_channel *);

#endif /* GZOCHI_CHANNEL_H */
