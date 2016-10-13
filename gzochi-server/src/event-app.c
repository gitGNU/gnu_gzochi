/* event-app.c: gzochid-specific event type implementations
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "event.h"
#include "event-app.h"

struct _GzochidMetaServerEvent
{
  GzochidEvent parent_instance; /* The base struct, for casting. */

  char *connection_description; /* The connection description string. */
  
  /* The admin server base URL, or `NULL' if not available. */
  
  char *admin_server_base_url; 
};

G_DEFINE_TYPE (GzochidMetaServerEvent, gzochid_meta_server_event,
	       GZOCHID_TYPE_EVENT);

enum gzochid_meta_server_event_properties
  {
    PROP_META_SERVER_EVENT_CONNECTION_DESCRIPTION = 1,
    PROP_META_SERVER_EVENT_ADMIN_SERVER_BASE_URL,
    N_META_SERVER_EVENT_PROPERTIES
  };

static GParamSpec *
meta_server_event_properties[N_META_SERVER_EVENT_PROPERTIES] = { NULL };

static void
gzochid_meta_server_event_get_property (GObject *object, guint property_id,
					GValue *value, GParamSpec *pspec)
{
  GzochidMetaServerEvent *event = GZOCHID_META_SERVER_EVENT (object);

  switch (property_id)
    {
    case PROP_META_SERVER_EVENT_CONNECTION_DESCRIPTION:
      g_value_set_static_string (value, event->connection_description);
      break;
      
    case PROP_META_SERVER_EVENT_ADMIN_SERVER_BASE_URL:
      g_value_set_static_string (value, event->admin_server_base_url);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_meta_server_event_set_property (GObject *object, guint property_id,
					const GValue *value, GParamSpec *pspec)
{
  GzochidMetaServerEvent *event = GZOCHID_META_SERVER_EVENT (object);

  switch (property_id)
    {
    case PROP_META_SERVER_EVENT_CONNECTION_DESCRIPTION:
      if (event->connection_description != NULL)
	{
	  free (event->connection_description);
	  event->connection_description = NULL;
	}
      if (g_value_get_string (value) != NULL)	
	event->connection_description = strdup (g_value_get_string (value));
      break;
      
    case PROP_META_SERVER_EVENT_ADMIN_SERVER_BASE_URL:
      if (event->admin_server_base_url != NULL)
	{
	  free (event->admin_server_base_url);
	  event->admin_server_base_url = NULL;
	}
      if (g_value_get_string (value) != NULL)
	event->admin_server_base_url = strdup (g_value_get_string (value));
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
gzochid_meta_server_event_finalize (GObject *object)
{
  GzochidMetaServerEvent *self = GZOCHID_META_SERVER_EVENT (object);

  if (self->connection_description != NULL)
    free (self->connection_description);
  if (self->admin_server_base_url != NULL)
    free (self->admin_server_base_url);
}

static void
gzochid_meta_server_event_class_init (GzochidMetaServerEventClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->get_property = gzochid_meta_server_event_get_property;
  object_class->set_property = gzochid_meta_server_event_set_property;
  object_class->finalize = gzochid_meta_server_event_finalize;
  
  meta_server_event_properties[PROP_META_SERVER_EVENT_CONNECTION_DESCRIPTION] =
    g_param_spec_string ("connection-description", "address",
			 "The gzochi meta server address", NULL,
			 G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  meta_server_event_properties[PROP_META_SERVER_EVENT_ADMIN_SERVER_BASE_URL] =
    g_param_spec_string ("admin-server-base-url", "url",
			 "The gzochi meta server console URL", NULL,
			 G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  
  g_object_class_install_properties
    (object_class, N_META_SERVER_EVENT_PROPERTIES,
     meta_server_event_properties);
}

static void
gzochid_meta_server_event_init (GzochidMetaServerEvent *self)
{
}
