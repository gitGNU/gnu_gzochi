/* view.h: Declarations and prototypes for views.c
 * Copyright (C) 2012 Julian Graham
 *
 * This is free software: you can redistribute it and/or modify it
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

#ifndef MAZEWAR_DATA_VIEWS_H
#define MAZEWAR_DATA_VIEWS_H

/* Models an X, Y coordinate pair on the rendering surface. */

typedef struct _mazewar_vertex 
{ 
  unsigned int x; /* The x coordinate. */
  unsigned int y; /* The y coordinate. */
} mazewar_vertex;

/* Models an edge connecting two `mazewar_vertex' points. */

typedef struct _mazewar_edge 
{ 
  mazewar_vertex p1; /* The first point. */
  mazewar_vertex p2; /* The second point. */
} mazewar_edge;

mazewar_edge mazewar_view_edges[2880]; /* Pre-computed 3-D edge data table. */

#endif /* MAZEWAR_DATA_VIEWS_H */
