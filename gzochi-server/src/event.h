/* event.h: Prototypes and declarations for event.c
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

#ifndef GZOCHID_EVENT_H
#define GZOCHID_EVENT_H

#include <glib.h>
#include <sys/time.h>

typedef enum _gzochid_application_event_type
  {
    TRANSACTION_START, /* An application transaction has been started. */
    TRANSACTION_COMMIT, /* A transaction has been committed. */
    TRANSACTION_ROLLBACK /* A transaction has been rolled back. */
  }
  gzochid_application_event_type;

typedef struct _gzochid_application_event
{
  gzochid_application_event_type type;
  struct timeval timestamp;
} gzochid_application_event;

typedef struct _gzochid_application_event_source 
gzochid_application_event_source;

gzochid_application_event_source *gzochid_application_event_source_new (void);
void gzochid_application_event_source_free 
(gzochid_application_event_source *);

typedef void (*gzochid_application_event_handler) 
(gzochid_application_event *, gpointer);

/**
   Attaches the specified application event handler to the event source.
 */
void gzochid_application_event_attach 
(gzochid_application_event_source *, gzochid_application_event_handler, 
 gpointer);

/**
   Dispatches an event to the event source, triggering all registered handlers.
   The application event's memory will be freed once all handlers have been
   invoked.
 */
void gzochid_application_event_dispatch
(gzochid_application_event_source *, gzochid_application_event *);

#endif /* GZOCHID_EVENT_H */
