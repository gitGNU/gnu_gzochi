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

#ifndef GZOCHID_CHANNEL_H
#define GZOCHID_CHANNEL_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "io.h"
#include "session.h"
#include "task.h"

typedef struct _gzochid_channel gzochid_channel;

extern gzochid_io_serialization gzochid_channel_serialization;

gzochid_channel *gzochid_channel_create (gzochid_application_context *, char *);
gzochid_channel *gzochid_channel_get (gzochid_application_context *, char *);
gzochid_channel *gzochid_channel_new (char *);
void gzochid_channel_free (gzochid_channel *);

void gzochid_channel_join 
(gzochid_application_context *, gzochid_channel *, gzochid_client_session *);
void gzochid_channel_leave 
(gzochid_application_context *, gzochid_channel *, gzochid_client_session *);
void gzochid_channel_send 
(gzochid_application_context *, gzochid_channel *,  unsigned char *, short);
void gzochid_channel_close (gzochid_application_context *, gzochid_channel *);

/* Sets the specified `mpz' to the oid of the specified channel's Scheme
   representation. */

void gzochid_channel_scm_oid (gzochid_channel *, mpz_t);

/*
  Returns the specified channel's name.
  
  The returned pointer is owned by the channel and should not be modified or
  freed by the caller. 
*/   

const char *gzochid_channel_name (gzochid_channel *);

#endif /* GZOCHID_CHANNEL_H */
