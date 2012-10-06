/* display.h: Prototypes and declarations for display.c and display-[driver].c
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

#ifndef MAZEWAR_DISPLAY_H
#define MAZEWAR_DISPLAY_H

#include <glib.h>

#include "mazewar.h"

#define	VIEW_X_DIM 200
#define	VIEW_Y_DIM 200

#define	VIEW_X_ORIGIN 50
#define	VIEW_Y_ORIGIN 25

#define	MAZE_X_ORIGIN 24
#define	MAZE_Y_ORIGIN 225

#define	SCORE_X_DIM 96
#define	SCORE_Y_DIM 104
#define	SCORE_X_ORIGIN 104
#define	SCORE_Y_ORIGIN 354

#define	MAZEWAR_X_DIM 304
#define	MAZEWAR_Y_DIM 458

/* The following functions are high level rendering functions that paint
   various portions of the rendering target. Their implementations can be
   found in display.c. */

/* Draw the overhead view of the maze. */
void mazewar_display_draw_maze (mazewar_context *);
/* Draw the first-person 3-D view of the local player's current location. */
void mazewar_display_draw_view (mazewar_context *);
/* Draw the scorecard for the current players. */
void mazewar_display_draw_scorecard (mazewar_context *);

/* Erase the arrow indicator from the specified location in the overhead view
   of the maze. */
void mazewar_display_clear_arrow (mazewar_context *, int, int);
/* Draw the arrow representing the local player at the specified location and
   orientation in the overhead view of the maze. */
void mazewar_display_draw_arrow_self (mazewar_context *, int, int, short);
/* Draw the arrow representing another player at the specified location and
   orientation in the overhead view of the maze. */
void mazewar_display_draw_arrow_other (mazewar_context *, int, int, short);

/* The following functions are rendering "primitives" that should be 
   implemented by an adapter for a specific rendering library. See 
   display-sdl.c for more information. */

/* Initialize the display, returning an opaque pointer representing the 
   "display context." */
gpointer mazewar_display_init (mazewar_context *);
/* Flush any pending updates to the rendering target. */
void mazewar_display_refresh (mazewar_context *);
/* Clear all marks from the specified region of the rendering target. */
void mazewar_display_clear_area (mazewar_context *, int, int, int, int);
/* Invert the specified region of the rendering target, changing all white
   pixels to black and black pixels to white. */
void mazewar_display_invert_area (mazewar_context *, int, int, int, int);
/* Draws a black line on the rendering target from the specified start point to
   the specified end point. */
void mazewar_display_render_line (mazewar_context *, int, int, int, int);
/* Write the text of the specified NULL-terminated string to the specified
   location on the rendering target. */
void mazewar_display_render_text (mazewar_context *, char *, int, int);
/* Copy a region of the specified bitmap to the specified location on the 
   rendering target .*/
void mazewar_display_render_bitmap 
(mazewar_context *, unsigned char *, int, int, int, int, int, int, int, int);

#endif /* MAZEWAR_DISPLAY_H */
