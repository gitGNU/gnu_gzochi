/* queue.h: Prototypes and declarations for queue.c
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

#ifndef GZOCHID_QUEUE_H
#define GZOCHID_QUEUE_H

#include <glib.h>

#include "app.h"
#include "io.h"

/* The following functions and data structures implement a simple FIFO queue
   capable of efficient, durable persistence of its contents using the gzochid 
   data storage services. 

   The elements in a queue may be of heterogeneous type, but note that since
   a `gzochid_io_serialization' argument must be provided to 
   `gzochid_durable_queue_pop' for practical purposes the contents of a queue
   should usually be of static, uniform type.
*/

/* The queue structure. */

typedef struct _gzochid_durable_queue gzochid_durable_queue;

/* Create and return a new queue structure associated with the specified gzochi
   game application context. */

gzochid_durable_queue *gzochid_durable_queue_new
(gzochid_application_context *);

/*
  Frees the memory associated with the specified queue.

  In most cases, if the queue is being managed by the application container's
  data services, this function will be called automatically when a reference to
  the queue goes out of scope, e.g., at the end of a transaction. */

void gzochid_durable_queue_free (gzochid_durable_queue *);

/* Appends a serializable object to the end of the specific queue. */

void gzochid_durable_queue_offer (gzochid_durable_queue *,
				  gzochid_io_serialization *, gpointer,
				  GError **);

/* Deserializes and returns the object at the head of the specified queue, or 
   `NULL' if the queue is empty. */

gpointer gzochid_durable_queue_peek (gzochid_durable_queue *,
				     gzochid_io_serialization *, GError **);

/* Deserializes and removes the object at the head of the specified queue, or 
   `NULL' if the queue is empty. */

gpointer gzochid_durable_queue_pop (gzochid_durable_queue *,
				    gzochid_io_serialization *, GError **);

/* The serialization struct for the `gzochid_durable_queue' type. */

extern gzochid_io_serialization gzochid_durable_queue_serialization;

#endif /* GZOCHID_QUEUE_H */
