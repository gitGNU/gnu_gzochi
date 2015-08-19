/* event.c: Application event bus implementation for gzochid
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "event.h"

#define DEFAULT_EVENT_POLL_INTERVAL_MS 1000

struct _gzochid_application_event_handler_registration
{
  gzochid_application_event_handler handler;
  gpointer user_data;
};

typedef struct _gzochid_application_event_handler_registration
gzochid_application_event_handler_registration;

struct _gzochid_application_event_source
{
  GSource base;
  GList *handlers;
  GQueue *events;
  GMutex mutex;
};

static gboolean
prepare (GSource *source, gint *timeout)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;

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
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;
  
  return !g_queue_is_empty (event_source->events);
}

static void
dispatch_inner_inner (gpointer data, gpointer user_data)
{
  gzochid_application_event_handler_registration *registration = data;

  registration->handler (user_data, registration->user_data);
}

static void
free_event (gzochid_application_event *event)
{
  switch (event->type)
    {
    case BYTES_READ:
    case BYTES_WRITTEN:
      g_slice_free (gzochid_application_data_event,
		    (gzochid_application_data_event *) event);
      break;

    case TRANSACTION_START:
      g_slice_free (gzochid_application_event, event);
      break;

    case TRANSACTION_COMMIT:
    case TRANSACTION_ROLLBACK:
      g_slice_free (gzochid_application_transaction_event,
		    (gzochid_application_transaction_event *) event);
      break;

    default: assert (1 == 0);
    }
}

static void
dispatch_inner (gpointer data, gpointer user_data)
{
  gzochid_application_event_source *event_source = user_data;
  
  g_list_foreach 
    (event_source->handlers, dispatch_inner_inner, data);
  free_event (data);
}

static gboolean
dispatch (GSource *source, GSourceFunc cb, gpointer user_data)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;

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

gzochid_application_event_source *
gzochid_application_event_source_new ()
{
  gzochid_application_event_source *source = 
    (gzochid_application_event_source *) g_source_new
    (&event_funcs, sizeof (gzochid_application_event_source));

  source->events = g_queue_new ();
  source->handlers = NULL;
  g_mutex_init (&source->mutex);
  
  return source;
}

void
gzochid_application_event_source_free (gzochid_application_event_source *source)
{
  g_mutex_clear (&source->mutex);
  g_list_free (source->handlers);
  g_queue_free (source->events);
  g_source_unref ((GSource *) source);
}

static void
event_init (gzochid_application_event *base_event,
	    gzochid_application_event_type type)
{
  base_event->type = type;
  gettimeofday (&base_event->timestamp, NULL);
}

gzochid_application_event *
gzochid_application_data_event_new (gzochid_application_event_type type,
				    unsigned long bytes)
{
  gzochid_application_data_event *event =
    g_slice_alloc (sizeof (gzochid_application_data_event));

  assert (type == BYTES_READ || type == BYTES_WRITTEN);

  event_init ((gzochid_application_event *) event, type);
  event->bytes = bytes;
  return (gzochid_application_event *) event;
}

gzochid_application_event *
gzochid_application_event_new (gzochid_application_event_type type)
{
  gzochid_application_message_event *event =
    g_slice_alloc (sizeof (gzochid_application_event));

  assert (type == TRANSACTION_START);
  
  event_init (event, type);
  return event;
}

gzochid_application_event *
gzochid_application_transaction_event_new (gzochid_application_event_type type,
					   struct timeval duration)
{
  gzochid_application_transaction_event *event =
    g_slice_alloc (sizeof (gzochid_application_transaction_event));

  assert (type == TRANSACTION_COMMIT || type == TRANSACTION_ROLLBACK);

  event_init ((gzochid_application_event *) event, type);
  event->duration = duration;
  return (gzochid_application_event *) event;
}

void
gzochid_application_event_attach (gzochid_application_event_source *source, 
				  gzochid_application_event_handler handler,
				  gpointer user_data)
{
  gzochid_application_event_handler_registration *registration = 
    malloc (sizeof (gzochid_application_event_handler_registration));

  registration->handler = handler;
  registration->user_data = user_data;

  source->handlers = g_list_append (source->handlers, registration);
}

void
gzochid_application_event_dispatch (gzochid_application_event_source *source,
				    gzochid_application_event *event)
{
  g_mutex_lock (&source->mutex);
  g_queue_push_tail (source->events, event);
  g_mutex_unlock (&source->mutex);
}
