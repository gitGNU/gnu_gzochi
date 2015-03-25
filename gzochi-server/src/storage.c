/* storage.c: Storage engine management routines for gzochid
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

#include <glib.h>
#include <gmodule.h>
#include <stddef.h>
#include <stdlib.h>
#include <strings.h>

#include "game.h"
#include "storage.h"
#include "log.h"

#define ENGINE_INTERFACE_FUNCTION "gzochid_storage_init_engine"

#ifndef GZOCHID_STORAGE_ENGINE_LOCATION
#define GZOCHID_STORAGE_ENGINE_LOCATION "./storage"
#endif /* GZOCHID_STORAGE_ENGINE_LOCATION */

/* The initialization process for storage engine modules is similar to the
   process for loading authentication plugins, except that no probing is done.
   GLib's module load is used to open and load a named storage engine, which is
   then searched for a well-known, locally-scoped initialization function. That
   function's responsibility is to set the storage interface on the storage
   engine. If any of these steps fail, this function returns NULL. */

gzochid_storage_engine *
gzochid_storage_load_engine (const char *name)
{
  if (g_module_supported ())
    {
      gchar *path = g_strconcat 
	(GZOCHID_STORAGE_ENGINE_LOCATION, "/", name, NULL);
      GModule *engine_handle = g_module_open (path, G_MODULE_BIND_LOCAL);
      int (*engine_interface) (gzochid_storage_engine *);

      if (engine_handle == NULL)
	{
	  gzochid_warning 
	    ("Failed to load storage engine at '%s': %s", path, 
	     g_module_error ());

	  free (path);

	  return NULL;
	}

      /* Attempt to locate the storage engine's interface bootstrap function. */

      else if (!g_module_symbol (engine_handle, ENGINE_INTERFACE_FUNCTION, 
				 (gpointer *) &engine_interface))
	{
	  gzochid_warning
	    ("Missing engine info at '%s': %s", path, g_module_error ());

	  free (path);
	  g_module_close (engine_handle);

	  return NULL;
	}
      else
	{
	  gzochid_storage_engine *engine = 
	    malloc (sizeof (gzochid_storage_engine));

	  /* Invoke the interface bootstrap. */

	  if (engine_interface (engine) != 0)
	    {
	      gzochid_warning ("Failed to introspect engine at '%s'.", path);

	      free (path);
	      g_module_close (engine_handle);	  
	      free (engine);

	      return NULL;
	    }

	  engine->handle = engine_handle;
	      
	  gzochid_info ("Loaded storage engine '%s'", engine->interface->name);
	  return engine;
	}
    }
  else 
    {
      gzochid_info 
	("Plugins not supported; cannot load storage engine '%s'.", name);
      return NULL;
    }
}
