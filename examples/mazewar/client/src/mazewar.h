/* mazewar.h: Global declarations for gzochi mazewar example game
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

#ifndef MAZEWAR_H
#define MAZEWAR_H

#include <glib.h>
#include <libgzochi.h>

/* Constant definitions for the four cardinal directions. */

#define NORTH 0
#define SOUTH 1
#define EAST 2
#define WEST 3

/* Constant definitions for the four relative orientations. */

#define LEFT 0
#define RIGHT 1
#define REAR 2
#define FRONT 3

/* The mazewar player data structure. */

typedef struct _mazewar_player
{
  char *name; /* The player' name. */
  unsigned int score; /* The player's score. */

  /* If this is a non-local player, whether the player is currently visible to 
     the local player. */

  gboolean visible; 
  
  /* The coordinates and current orientation of the player within the maze. If
     this is a non-local, non-visible player, the values of these fields is
     unspecified. */

  short x;
  short y;
  short dir;
} mazewar_player;

/* The mazewar game context. Stores "global" information about the game state
   and the structure of the maze. */

typedef struct _mazewar_context {
  unsigned char *maze; /* The maze bitmap. */
  short width; /* The width of the maze in tiles. */
  short height; /* The height of the maze in tiles. */

  /* The display context is an opaque pointer to memory whose contents is
     determined by (and owned by) the code that implements the low-level
     display primitives. See display.h for more information. */

  gpointer display_context;

  gzochi_client_session *session; /* The gzochi client session. */
  mazewar_player *self; /* A pointer to the local player structure. */
  GList *players; /* The list of all players (including the local player). */
} mazewar_context;

/* Returns true (non-zero) if the tile at the specified coordinates is a wall
   (i.e., not a space for a player), false (zero) otherwise. */

gboolean mazewar_maze_is_wall (mazewar_context *, int, int);

#endif /* MAZEWAR_H */
