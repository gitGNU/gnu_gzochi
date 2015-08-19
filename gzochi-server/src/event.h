/* event.h: Prototypes and declarations for event.c
 * Copyright (C) 2015 Julian Graham
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

#ifndef GZOCHID_EVENT_H
#define GZOCHID_EVENT_H

#include <glib.h>
#include <sys/time.h>

enum _gzochid_application_event_type
  {
    BYTES_READ, /* Bytes have been read from the data store. */
    BYTES_WRITTEN, /* Bytes have been written to the data store. */

    TRANSACTION_START, /* An application transaction has been started. */
    TRANSACTION_COMMIT, /* A transaction has been committed. */
    TRANSACTION_ROLLBACK /* A transaction has been rolled back. */
  };

typedef enum _gzochid_application_event_type gzochid_application_event_type;

struct _gzochid_application_event
{
  gzochid_application_event_type type; /* The type of event. */
  struct timeval timestamp; /* The time the event was fired. */
};

typedef struct _gzochid_application_event gzochid_application_event;

struct _gzochid_application_transaction_event
{
  gzochid_application_event base;
  struct timeval duration; /* The transaction duration. */
};

typedef struct _gzochid_application_transaction_event
gzochid_application_transaction_event;

struct _gzochid_application_data_event
{
  gzochid_application_event base;
  unsigned long bytes; /* The number of bytes. */
};

typedef struct _gzochid_application_data_event gzochid_application_data_event;

typedef struct _gzochid_application_event_source 
gzochid_application_event_source;

gzochid_application_event_source *gzochid_application_event_source_new (void);
void gzochid_application_event_source_free
(gzochid_application_event_source *);

typedef void (*gzochid_application_event_handler) 
(gzochid_application_event *, gpointer);

/* Create and return a new application event of the specified type, which must
   be `TRANSACTION_START'. Do not free the memory referenced by the returned 
   pointer; it will be freed after the event has been dispatched. */

gzochid_application_event *gzochid_application_event_new
(gzochid_application_event_type);

/* Create and return a new application event of the specified type, which must
   be one of `BYTES_READ' or `BYTES_WRITTEN', with the specified byte count. Do
   not free the memory referenced by the returned pointer; it will be freed 
   after the event has been dispatched. */

gzochid_application_event *gzochid_application_data_event_new
(gzochid_application_event_type, unsigned long);

/* Create and return a new application event of the specified type, which must
   be one of `TRANSACTION_COMMIT' or `TRANSACTION_ROLLBACK', along with the
   specified transaction duration. Do not free the memory referenced by the 
   returned pointer; it will be freed after the event has been dispatched. */

gzochid_application_event *gzochid_application_transaction_event_new
(gzochid_application_event_type, struct timeval);

/* Attaches the specified application event handler to the event source.*/

void gzochid_application_event_attach 
(gzochid_application_event_source *, gzochid_application_event_handler, 
 gpointer);

/* Dispatches an event to the event source, triggering all registered handlers.
   The application event's memory will be freed once all handlers have been
   invoked. */

void gzochid_application_event_dispatch
(gzochid_application_event_source *, gzochid_application_event *);

#endif /* GZOCHID_EVENT_H */
