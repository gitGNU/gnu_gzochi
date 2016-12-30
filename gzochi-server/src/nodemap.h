/* nodemap.h: Generic interface for nodemap implementations
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

#ifndef GZOCHI_METAD_NODEMAP_H
#define GZOCHI_METAD_NODEMAP_H

#include <glib.h>

/*
  The following prototypes describe a mechanism for creating and consulting a 
  bi-directional mapping between gzochid application server nodes and the 
  application client sessions connected to them, for use in message routing
  and load balancing.

  This mapping is ephemeral with respect to application server nodes; it 
  represents (somewhat accurately) the state of all connections at the given
  instant.
*/

/* The base struct type for nodemap implementations. */

struct _gzochi_metad_nodemap
{
  struct _gzochi_metad_nodemap_iface *iface; /* The nodemap vtable. */
};

typedef struct _gzochi_metad_nodemap gzochi_metad_nodemap;

/* The nodemap functional interface, as a kind of poor man's vtable. */

struct _gzochi_metad_nodemap_iface
{
  /* Map the specified session id to the specified application server node id.
     Signals an error if the session is already mapped to a server. */
  
  void (*map_session) (gzochi_metad_nodemap *, char *, guint64, int,
		       GError **);

  /* Removes the mapping for the specified session id. Signals an error if the
     session is not currently mapped to a server. */
  
  void (*unmap_session) (gzochi_metad_nodemap *, char *, guint64, GError **);

  /* Returns the application server node id to which the specified session is 
     currently mapped. Signals an error if the session is not currently mapped
     to a server. */
  
  int (*lookup_session) (gzochi_metad_nodemap *, char *, guint64, GError **);

  /* Removes all client sessions mapped to the specified application server node
     id. */
  
  void (*unmap_all) (gzochi_metad_nodemap *, int);
};

typedef struct _gzochi_metad_nodemap_iface gzochi_metad_nodemap_iface;

/* Conveniene macro for extracting the functional interface from a given
   nodemap instance. */

#define GZOCHI_METAD_NODEMAP_IFACE(nodemap) \
  ((gzochi_metad_nodemap *) (nodemap))->iface;

enum
  {
    /* Indicates a failure resulting from an attempt to create a mapping for a
       session that is already mapped to an application server node id. */
    
    GZOCHI_METAD_NODEMAP_ERROR_ALREADY_MAPPED,

    /* Indicates a failure resulting from an attempt to look up the application
       server node id for a session that is not currently mapped to a server. */
    
    GZOCHI_METAD_NODEMAP_ERROR_NOT_MAPPED,

    /* Generic nodemap failure. */
    
    GZOCHI_METAD_NODEMAP_ERROR_FAILED
  };

#define GZOCHI_METAD_NODEMAP_ERROR gzochi_metad_nodemap_error_quark ()

GQuark gzochi_metad_nodemap_error_quark ();

#endif /* GZOCHI_METAD_NODEMAP_H */
