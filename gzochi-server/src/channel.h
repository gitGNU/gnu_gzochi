/* channel.h: Prototypes and declarations for channel.c
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

#ifndef GZOCHID_CHANNEL_H
#define GZOCHID_CHANNEL_H

#include <glib.h>

#include "app.h"
#include "durable-task.h"
#include "io.h"
#include "session.h"
#include "task.h"

typedef struct _gzochid_channel gzochid_channel;

extern gzochid_io_serialization gzochid_channel_serialization;

extern gzochid_application_task_serialization
gzochid_channel_operation_task_serialization;

/*
  Create and return a new channel with the specified name in the specified game
  application context.

  Returns `NULL' if the transaction could not be created; e.g., if the current
  transaction is marked for rollback.
*/

gzochid_channel *gzochid_channel_create (gzochid_application_context *, char *);
gzochid_channel *gzochid_channel_get (gzochid_application_context *, char *);

/*
  Initialize and return a new channel with the specified name. 
  
  Note that this function does not enqueue the channel for persistence nor bind
  it to a Scheme object. Call `gzochid_channel_create' to do those things.
*/

gzochid_channel *gzochid_channel_new (char *);

/*
  Frees the memory associated with the specified channel.

  Note that this function does not enqueue the channel for persistence nor bind
  it to a Scheme object. Call `gzochid_channel_create' to do those things.
*/

void gzochid_channel_free (gzochid_channel *);

void gzochid_channel_join 
(gzochid_application_context *, gzochid_channel *, gzochid_client_session *);
void gzochid_channel_leave 
(gzochid_application_context *, gzochid_channel *, gzochid_client_session *);
void gzochid_channel_send 
(gzochid_application_context *, gzochid_channel *,  unsigned char *, short);
void gzochid_channel_close (gzochid_application_context *, gzochid_channel *);

/* Returns the oid of the specified channel's Scheme representation. */

guint64 gzochid_channel_scm_oid (gzochid_channel *);

/*
  Returns the specified channel's name.
  
  The returned pointer is owned by the channel and should not be modified or
  freed by the caller. 
*/   

const char *gzochid_channel_name (gzochid_channel *);

#endif /* GZOCHID_CHANNEL_H */
