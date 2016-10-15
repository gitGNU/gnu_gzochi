/* event.h: Prototypes and declarations for event.c
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

#ifndef GZOCHID_EVENT_H
#define GZOCHID_EVENT_H

#include <glib.h>
#include <glib-object.h>
#include <sys/time.h>

/* Enumeration of base event types. */

enum _gzochid_event_type
  {
    MESSAGE_RECEIVED, /* A message has been received from a client session. */
    MESSAGE_SENT, /* A message has been sent to a client session. */
    
    TRANSACTION_START /* An application transaction has been started. */
  };

typedef enum _gzochid_event_type gzochid_event_type;

/* Enumeration of data-related event types. */

enum _gzochid_data_event_type
  {
    BYTES_READ, /* Bytes have been read from the data store. */
    BYTES_WRITTEN /* Bytes have been written to the data store. */
  };

typedef enum _gzochid_data_event_type gzochid_data_event_type;

/* Enumeration of transaction-related event types. */

enum _gzochid_transaction_event_type
  {
    TRANSACTION_COMMIT, /* A transaction has been committed. */
    TRANSACTION_ROLLBACK /* A transaction has been rolled back. */
  };

typedef enum _gzochid_transaction_event_type gzochid_transaction_event_type;

/* GObject type definition for base event type. */

#define GZOCHID_TYPE_EVENT gzochid_event_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_DERIVABLE_TYPE' can be used. */

GType gzochid_event_get_type (void);

/*
  The base event object. Visible here to support the creation of event 
  sub-types outside of event.c. The following properties are available: 
  
  type - the event type, as an int that can be mapped to as type-specific enum
  timestamp-us - the event timestamp, in microseconds since the Unix epoch 
*/

struct _GzochidEvent
{
  GObject parent_instance; /* The base struct, for casting. */

  /* Pointer to the instance's private struct. */

  struct _GzochidEventPrivate *priv; 
};

typedef struct _GzochidEvent GzochidEvent;

struct _GzochidEventClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidEventClass GzochidEventClass;

static inline GzochidEvent *
GZOCHID_EVENT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_event_get_type (), GzochidEvent);
}

/* End boilerplate. */

/* GObject type definition for data event sub-type. */

#define GZOCHID_TYPE_DATA_EVENT gzochid_data_event_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_data_event_get_type (void);

typedef struct _GzochidDataEvent GzochidDataEvent;

struct _GzochidDataEventClass
{
  GzochidEventClass parent_class;
};

typedef struct _GzochidDataEventClass GzochidDataEventClass;

static inline GzochidDataEvent *
GZOCHID_DATA_EVENT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_data_event_get_type (), GzochidDataEvent);
}

/* End boilerplate. */

/* GObject type definition for transaction event sub-type. */

#define GZOCHID_TYPE_TRANSACTION_EVENT gzochid_transaction_event_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_transaction_event_get_type (void);

typedef struct _GzochidTransactionEvent GzochidTransactionEvent;

struct _GzochidTransactionEventClass
{
  GzochidEventClass parent_class;
};

typedef struct _GzochidTransactionEventClass GzochidTransactionEventClass;

static inline GzochidTransactionEvent *
GZOCHID_TRANSACTION_EVENT (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_transaction_event_get_type (), GzochidTransactionEvent);
}

/* End boilerplate. */

/* The gzochid event loop object is a lightweight wrapper around GLib's 
   `GMainLoop' and `GMainContext', to make it easy to share and inject those
   objects using gzochid's dependency injector. */

#define GZOCHID_TYPE_EVENT_LOOP gzochid_event_loop_get_type ()

/* The following boilerplate can be consolidated once GLib 2.44 makes it into
   Debian stable and `G_DECLARE_FINAL_TYPE' can be used. */

GType gzochid_event_loop_get_type (void);

typedef struct _GzochidEventLoop GzochidEventLoop;

struct _GzochidEventLoopClass
{
  GObjectClass parent_class;
};

typedef struct _GzochidEventLoopClass GzochidEventLoopClass;

static inline GzochidEventLoop *
GZOCHID_EVENT_LOOP (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, gzochid_event_loop_get_type (), GzochidEventLoop);
}

/* End boilerplate. */

/* Start the specified event loop, launching a new thread to run its 
   iterations. It is a programming error (and will violate an assertion) to
   start an event loop more than once. */

void gzochid_event_loop_start (GzochidEventLoop *);

/* Stop the specified event loop, terminating its associated thread. After
   this function returns, no further event notifications will take place until
   the event loop is started again. */

void gzochid_event_loop_stop (GzochidEventLoop *);

typedef struct _gzochid_event_source gzochid_event_source;

gzochid_event_source *gzochid_event_source_new (void);

/* Attach the specified event source to the specified main loop. This function
   is a thin wrapper around GLib's `g_source_attach', and, as such, may be
   called when the event loop is running or when it is stopped. */

void gzochid_event_source_attach (GzochidEventLoop *, gzochid_event_source *);

typedef void (*gzochid_event_handler) (GzochidEvent *, gpointer);

/* Attaches the specified event handler to the event source.*/

void gzochid_event_attach
(gzochid_event_source *, gzochid_event_handler, gpointer);

/* Dispatches an event to the event source, triggering all registered 
   handlers. */

void gzochid_event_dispatch (gzochid_event_source *, GzochidEvent *);

#endif /* GZOCHID_EVENT_H */
