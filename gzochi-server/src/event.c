/* event.c: Application event bus implementation for gzochid
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

#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "event.h"

#define DEFAULT_EVENT_POLL_INTERVAL_MS 1000

/* The base event object. */

struct _GzochidEvent
{
  GObject parent_instance;

  /* The event type. This will be an enum value scoped to the GObject type.
     I.e., For GzochidDataEvents, this will be one of the values in 
     `gzochid_data_event_type'. */

  int type;

  /* The event timestamp, in microseconds since the Unix epoch. */

  guint64 timestamp_us; 
};

/* Boilerplate setup for the base event object. */

G_DEFINE_TYPE (GzochidEvent, gzochid_event, G_TYPE_OBJECT);

enum gzochid_event_properties
  {
    PROP_EVENT_TYPE = 1,
    PROP_EVENT_TIMESTAMP_US,
    N_EVENT_PROPERTIES
  };

static GParamSpec *event_properties[N_EVENT_PROPERTIES] = { NULL };

static void
gzochid_event_get_property (GObject *object, guint property_id,
			    GValue *value, GParamSpec *pspec)
{
  GzochidEvent *event = GZOCHID_EVENT (object);

  switch (property_id)
    {
    case PROP_EVENT_TYPE:
      g_value_set_int (value, event->type);
      break;
      
    case PROP_EVENT_TIMESTAMP_US:
      g_value_set_uint64 (value, event->timestamp_us);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_event_set_property (GObject *object, guint property_id,
			    const GValue *value, GParamSpec *pspec)
{
  GzochidEvent *event = GZOCHID_EVENT (object);

  switch (property_id)
    {
    case PROP_EVENT_TYPE:
      event->type = g_value_get_int (value);
      break;
      
    case PROP_EVENT_TIMESTAMP_US:
      event->timestamp_us = g_value_get_uint64 (value);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_event_class_init (GzochidEventClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gzochid_event_get_property;
  object_class->set_property = gzochid_event_set_property;

  event_properties[PROP_EVENT_TYPE] = g_param_spec_int
    ("type", "type", "The gzochi event type", INT_MIN, INT_MAX, 0,
     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  
  event_properties[PROP_EVENT_TIMESTAMP_US] = g_param_spec_uint64
    ("timestamp-us", "timestamp", "The gzochi event timestamp, in microseconds",
     0, G_MAXUINT64, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_EVENT_PROPERTIES, event_properties);
}

static void
gzochid_event_init (GzochidEvent *self)
{
  /* Initialize the event timestamp to the current time; in most cases, this 
     won't need to be overridden by clients. */     
  
  self->timestamp_us = g_get_monotonic_time ();
}

/* The data event object. */

struct _GzochidDataEvent
{
  GzochidEvent parent_instance; /* The parent event instance. */
  
  guint64 bytes; /* The number of bytes. */
};

/* Boilerplate setup for the data event sub-type. */

G_DEFINE_TYPE (GzochidDataEvent, gzochid_data_event, GZOCHID_TYPE_EVENT);

enum gzochid_data_event_properties
  {
    PROP_DATA_EVENT_BYTES = 1,
    N_DATA_EVENT_PROPERTIES
  };

static GParamSpec *data_event_properties[N_DATA_EVENT_PROPERTIES] = { NULL };

static void
gzochid_data_event_get_property (GObject *object, guint property_id,
				 GValue *value, GParamSpec *pspec)
{
  GzochidDataEvent *data_event = GZOCHID_DATA_EVENT (object);

  switch (property_id)
    {
    case PROP_DATA_EVENT_BYTES:
      g_value_set_uint64 (value, data_event->bytes);
      break;

    default:      
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_data_event_set_property (GObject *object, guint property_id,
				 const GValue *value, GParamSpec *pspec)
{
  GzochidDataEvent *data_event = GZOCHID_DATA_EVENT (object);

  switch (property_id)
    {
    case PROP_DATA_EVENT_BYTES:
      data_event->bytes = g_value_get_uint64 (value);
      break;

    default:      
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_data_event_class_init (GzochidDataEventClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gzochid_data_event_get_property;
  object_class->set_property = gzochid_data_event_set_property;

  data_event_properties[PROP_DATA_EVENT_BYTES] = g_param_spec_uint64
    ("bytes", "bytes", "The gzochi data event byte count", 0, G_MAXUINT64, 0,
     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_DATA_EVENT_PROPERTIES, data_event_properties);
}

static void
gzochid_data_event_init (GzochidDataEvent *self)
{
}

/* The transaction event object. */

struct _GzochidTransactionEvent
{
  GzochidEvent parent_instance; /* The parent event instance. */
  
  guint64 duration_us; /* The transaction duration, in microseconds. */
};

/* Boilerplate setup for the transaction event sub-type. */

G_DEFINE_TYPE (GzochidTransactionEvent, gzochid_transaction_event,
	       GZOCHID_TYPE_EVENT);

enum gzochid_transaction_event_properties
  {
    PROP_TRANSACTION_EVENT_DURATION_US = 1,
    N_TRANSACTION_EVENT_PROPERTIES
  };

static GParamSpec *
transaction_event_properties[N_TRANSACTION_EVENT_PROPERTIES] = { NULL };

static void
gzochid_transaction_event_get_property (GObject *object, guint property_id,
					GValue *value, GParamSpec *pspec)
{
  GzochidTransactionEvent *tx_event = GZOCHID_TRANSACTION_EVENT (object);

  switch (property_id)
    {
    case PROP_TRANSACTION_EVENT_DURATION_US:
      g_value_set_uint64 (value, tx_event->duration_us);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_transaction_event_set_property (GObject *object, guint property_id,
					const GValue *value, GParamSpec *pspec)
{
  GzochidTransactionEvent *tx_event = GZOCHID_TRANSACTION_EVENT (object);

  switch (property_id)
    {
    case PROP_TRANSACTION_EVENT_DURATION_US:
      tx_event->duration_us = g_value_get_uint64 (value);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_transaction_event_class_init (GzochidTransactionEventClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gzochid_transaction_event_get_property;
  object_class->set_property = gzochid_transaction_event_set_property;

  transaction_event_properties[PROP_TRANSACTION_EVENT_DURATION_US] =
    g_param_spec_uint64 ("duration-us", "duration", "", 0, G_MAXUINT64, 0,
			 G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties
    (object_class, N_TRANSACTION_EVENT_PROPERTIES,
     transaction_event_properties);
}

static void
gzochid_transaction_event_init (GzochidTransactionEvent *self)
{
}

struct _gzochid_event_handler_registration
{
  gzochid_event_handler handler;
  gpointer user_data;
};

typedef struct _gzochid_event_handler_registration
gzochid_event_handler_registration;

struct _gzochid_event_source
{
  GSource base;
  GList *handlers;
  GQueue *events;
  GMutex mutex;
};

static gboolean
prepare (GSource *source, gint *timeout)
{
  gzochid_event_source *event_source = (gzochid_event_source *) source;

  if (g_queue_is_empty (event_source->events))
    {
      *timeout = DEFAULT_EVENT_POLL_INTERVAL_MS;
      return FALSE;
    }
  else return TRUE;
}

static gboolean
check (GSource *source)
{
  gzochid_event_source *event_source = (gzochid_event_source *) source;
  
  return !g_queue_is_empty (event_source->events);
}

static void
dispatch_inner_inner (gpointer data, gpointer user_data)
{
  gzochid_event_handler_registration *registration = data;

  registration->handler (user_data, registration->user_data);
}

static void
dispatch_inner (gpointer data, gpointer user_data)
{
  gzochid_event_source *event_source = user_data;
  
  g_list_foreach 
    (event_source->handlers, dispatch_inner_inner, data);

  g_object_unref (data);
}

static gboolean
dispatch (GSource *source, GSourceFunc cb, gpointer user_data)
{
  gzochid_event_source *event_source = (gzochid_event_source *) source;

  g_mutex_lock (&event_source->mutex);
  g_queue_foreach (event_source->events, dispatch_inner, event_source);
  g_queue_clear (event_source->events);
  g_mutex_unlock (&event_source->mutex);

  return TRUE;
}

static void
finalize (GSource *source)
{
}

static GSourceFuncs event_funcs = { prepare, check, dispatch, finalize };

gzochid_event_source *
gzochid_event_source_new ()
{
  gzochid_event_source *source = (gzochid_event_source *) g_source_new
    (&event_funcs, sizeof (gzochid_event_source));

  source->events = g_queue_new ();
  source->handlers = NULL;
  g_mutex_init (&source->mutex);
  
  return source;
}

void
gzochid_event_source_free (gzochid_event_source *source)
{
  g_mutex_clear (&source->mutex);
  g_list_free (source->handlers);
  g_queue_free (source->events);
  g_source_unref ((GSource *) source);
}

void
gzochid_event_attach (gzochid_event_source *source, 
		      gzochid_event_handler handler,
		      gpointer user_data)
{
  gzochid_event_handler_registration *registration = 
    malloc (sizeof (gzochid_event_handler_registration));

  registration->handler = handler;
  registration->user_data = user_data;

  source->handlers = g_list_append (source->handlers, registration);
}

void
gzochid_event_dispatch (gzochid_event_source *source, GzochidEvent *event)
{
  g_object_ref (event);
  
  g_mutex_lock (&source->mutex);
  g_queue_push_tail (source->events, event);
  g_mutex_unlock (&source->mutex);
}
