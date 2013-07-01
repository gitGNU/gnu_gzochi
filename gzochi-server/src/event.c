/* event.c: Application event bus implementation for gzochid
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

#include <glib.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "event.h"

typedef struct _gzochid_application_event_handler_registration
{
  gzochid_application_event_handler handler;
  gpointer user_data;
} gzochid_application_event_handler_registration;

struct _gzochid_application_event_source
{
  GSource base;
  GList *handlers;
  GQueue *events;
  GMutex *mutex;
};

static gboolean prepare (GSource *source, gint *timeout)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;
  
  return !g_queue_is_empty (event_source->events);
}

static gboolean check (GSource *source)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;
  
  return !g_queue_is_empty (event_source->events);
}

static void dispatch_inner_inner (gpointer data, gpointer user_data)
{
  gzochid_application_event_handler_registration *registration = 
    (gzochid_application_event_handler_registration *) data;

  registration->handler 
    ((gzochid_application_event *) user_data, registration->user_data);
}

static void dispatch_inner (gpointer data, gpointer user_data)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) user_data;
  
  g_list_foreach 
    (event_source->handlers, dispatch_inner_inner, data);
  free (data);
}

static gboolean dispatch (GSource *source, GSourceFunc cb, gpointer user_data)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;

  g_mutex_lock (event_source->mutex);
  g_queue_foreach (event_source->events, dispatch_inner, event_source);
  g_queue_clear (event_source->events);
  g_mutex_unlock (event_source->mutex);

  return TRUE;
}

static void finalize (GSource *source)
{
}

static GSourceFuncs event_funcs = { prepare, check, dispatch, finalize };

gzochid_application_event_source *gzochid_application_event_source_new ()
{
  gzochid_application_event_source *source = 
    (gzochid_application_event_source *) g_source_new
    (&event_funcs, sizeof (gzochid_application_event_source));

  source->events = g_queue_new ();
  source->handlers = NULL;
  source->mutex = g_mutex_new ();
  
  return source;
}

void gzochid_application_event_attach 
(gzochid_application_event_source *source, 
 gzochid_application_event_handler handler, gpointer user_data)
{
  gzochid_application_event_handler_registration *registration = 
    malloc (sizeof (gzochid_application_event_handler_registration));

  registration->handler = handler;
  registration->user_data = user_data;

  source->handlers = g_list_append (source->handlers, registration);
}

void gzochid_application_event_dispatch
(gzochid_application_event_source *source, gzochid_application_event *event)
{
  gzochid_application_event_source *event_source = 
    (gzochid_application_event_source *) source;

  g_mutex_lock (event_source->mutex);
  g_queue_push_tail (event_source->events, event);
  g_mutex_unlock (event_source->mutex);
}
