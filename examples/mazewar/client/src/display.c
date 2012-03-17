/* display.c: Generic display routines for gzochi mazewar example game
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

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "display.h"
#include "mazewar.h"
#include "data/avatars.h"
#include "data/font.h"
#include "data/views.h"

/* Bitmap data for the arrows to be drawn for the local player, with 8 bytes
   for each of the NORTH, SOUTH, EAST, and WEST-facing arrows. */

static unsigned char normal_arrows[4][8] =
  {
    { 0xf7, 0xe3, 0xc1, 0x80, 0xe3, 0xe3, 0xe3, 0xe3 },
    { 0xe3, 0xe3, 0xe3, 0xe3, 0x80, 0xc1, 0xe3, 0xf7 },
    { 0xff, 0xf7, 0xf3, 0x01, 0x00, 0x01, 0xf3, 0xf7 },
    { 0xff, 0xef, 0xcf, 0x80, 0x00, 0x80, 0xcf, 0xef }
  };

/* Bitmap data for the arrows to be drawn for non-local players, with 8 bytes
   for each of the NORTH, SOUTH, EAST, and WEST-facing arrows. */

static unsigned char other_arrows[4][8] =
  {
    { 0xf7, 0xe7, 0xdb, 0x88, 0xeb, 0xeb, 0xeb, 0xe3 },
    { 0xe3, 0xeb, 0xeb, 0xeb, 0x88, 0xdb, 0xe7, 0xf7 },
    { 0xff, 0xf7, 0xf3, 0x05, 0x7e, 0x05, 0xf3, 0xf7 },
    { 0xff, 0xef, 0xcf, 0xa0, 0x7e, 0xa0, 0xcf, 0xef }
  };

/* Scales the bitmap BITMAP of height HEIGHT bits and width WIDTH bits by the 
   specified horizontal dimension X_SCALE and vertical dimension Y_SCALE. A new
   bitmap, in the form of a pointer to unsigned chars, of the size 
   HEIGHT * WIDTH * X_SCALE * Y_SCALE, is allocated and returned. (It should be
   freed by the caller when no longer needed.

   This is used to create the overhead map view out of the maze bitmap in the
   mazewar context. */

static unsigned char *scale_bitmap 
(unsigned char *bitmap, int width, int height, int x_scale, int y_scale)
{
  int i = 0, bitmap_size = width * height, new_width = width * x_scale;

  /* Allocate space for the scaled bitmap. */

  unsigned char *scaled_bitmap = calloc 
    (bitmap_size * x_scale * y_scale, sizeof (unsigned char));
  
  /* Iterate over every bit of the original bitmap. */

  for (; i < bitmap_size; i++)
    {
      int x = i % width;
      int y = i / width;
      int projected_x = x * x_scale; /* The x bit in the target bitmap. */
      int projected_y = y * y_scale; /* The y bit in the target bitmap. */
      
      int j = 0;
      for (; j < x_scale; j++)
	{
	  int k = 0;
	  for (; k < y_scale; k++)
	    {
	      int src_byte = bitmap[i / 8]; /* The source byte. */
	      int bit_pos = 7 - (i % 8); /* The source bit position. */

	      /* Whether the source bit was 1 or 0. */
	      
	      int src_bit = (src_byte & (1 << bit_pos)) >> bit_pos;

	      /* The bit index in the target bitmap. */
	      
	      int projected_pos =
		((projected_y + k) * new_width) + projected_x + j;
	      int projected_byte = projected_pos / 8; /* The target byte. */
	      
	      /* Inclusive-or the source bit with contents of the target 
		 bitmap's byte. */

	      scaled_bitmap[projected_byte] |= 
		src_bit << (7 - (projected_pos % 8));
	    }
	}
    }

  return scaled_bitmap;
}

/* Writes the score for the player PLAYER at index IDX within the scorecard. */

static void write_score 
(mazewar_context *context, mazewar_player *player, int idx)
{
  char buf[64]; /* The text buffer for the rendered score. */
  int score_len, left_edge, score_top = SCORE_Y_ORIGIN + idx * 13;

  snprintf (buf, 64, "%d", player->score); /* Render the score as text. */
  score_len = strlen (buf); /* Calculate the text length of the score. */

  /* Find the target left boundary of the rendered score. */

  left_edge = SCORE_X_DIM - score_len * 8;

  /* Clear the score line. */
  
  mazewar_display_clear_area
    (context, SCORE_X_ORIGIN, score_top, SCORE_X_DIM, 13);

  /* Render the player's name to the score line. */

  mazewar_display_render_text 
    (context, player->name, SCORE_X_ORIGIN, score_top);

  /* Render the text form of the player's score to the score line. */
  
  mazewar_display_render_text 
    (context, buf, SCORE_X_ORIGIN + left_edge, score_top);

  /* If the player whose score is being rendered is not the local player and
     is visible, then invert the score line to indicate their identity. */

  if (player != context->self && player->visible)
    mazewar_display_invert_area 
      (context, SCORE_X_ORIGIN, score_top, SCORE_X_DIM, 13);
}

/* A lookup table of relative orientations keyed by absolute orientations. The
   first dimension index is the orientation of the local player; the second
   dimension index is the orientation of the target visible player; the value
   at these array dimensions is the relative orientation (see mazewar.h) of 
   that player, for the purposes of choosing a sprite. */

static short relative_orientations[4][4] = 
  { { 2, 3, 1, 0 }, { 3, 2, 0, 1 }, { 0, 1, 2, 3 }, { 1, 0, 3, 2 } };

/* Overlays an avatar image of the visible non-local player PLAYER on the 
   first-person 3-D view, at the proper distance and with the correct 
   orientation. 

   The image bitmap is taken from the copied from the avatar "sprite sheet"
   bitmap `mazewar_avatar_bits' (see data/avatar.c,h), which contains all of 
   the different sizes and orientations of the eyeball avatar image. */

static void draw_avatar (mazewar_context *context, mazewar_player *player)
{
  int size = 0; /* The size (length-wise) of the target avatar sprite. */

  /* The x and y offsets of the avatar image within the sprite sheet. */

  int src_x = 0, src_y = 0; 

  /* The target x, y coordinates for the avatar image on the screen. */ 

  int scrn_x = 0, scrn_y = 0;

  int distance = 0; /* The distance of PLAYER in maze tiles. */
  int edge_offset = 0;

  int relative = 0;

  if (player->x != context->self->x) {
    distance = abs (player->x - context->self->x);
  } else {
    distance = abs (player->y - context->self->y);
  }

  edge_offset = distance * 12;

  /* Calculate the target position of the sprite based on the length of the
     edges at the specified distance in the edge table. */
 
  scrn_x = (mazewar_view_edges[edge_offset + 3].p2.x 
	    + mazewar_view_edges[edge_offset + 10].p1.x) / 2;
  scrn_y = (mazewar_view_edges[edge_offset + 3].p1.y 
	    + mazewar_view_edges[edge_offset + 3].p2.y) / 2;

  /* Figure out which orientation of the other player's avatar to display,
     depending on the current orientation of the local player. */

  relative = relative_orientations[context->self->dir][player->dir];

  /* Use the distance of the other player to determine which avatar sprite
     from the sprite sheet to use for display, as well as where on the
     rendering target the sprite should appear. (Not every distinct value for
     distance has its own sprite / location.) */

  switch (distance) 
    {
    case 1:
      size = 64;
      src_y = 0;

      switch (relative)
	{
	case RIGHT: src_x = 0; break;
	case LEFT: src_x = 64; break;
	case REAR: src_x = 128; break;
	case FRONT: src_x = 192; break;
	}
      break;
    
    case 2:
      size = 32;
      
      switch (relative)
	{
	case RIGHT: src_x = 288; src_y = 0; break;
	case LEFT: src_x = 288; src_y = 32; break;
	case REAR: src_x = 256; src_y = 0; break;
	case FRONT: src_x = 256; src_y = 32; break;
	}
      break;
    
    case 3:
      size = 24;

      switch (relative)
	{
	case RIGHT: src_x = 360; src_y = 0; break;
	case LEFT: src_x = 360; src_y = 24; break;
	case REAR: src_x = 336; src_y = 0; break;
	case FRONT: src_x = 336; src_y = 24; break;	  
	}
      break;
    
    case 4:
    case 5:
      size = 16;
      src_x = 320;

      switch (relative)
	{
	case RIGHT: src_y = 0; break;
	case LEFT: src_y = 16; break;
	case REAR: src_y = 32; break;
	case FRONT: src_y = 48; break;	  
	}
      break;
    
    case 6:
    case 7:
    case 8:
      size = 9;
      src_y = 48;
      
      switch (relative)
	{
	case RIGHT: src_x = 375; break;
	case LEFT: src_x = 366; break;
	case REAR: src_x = 357; break;
	case FRONT: src_x = 348; break;	  
	}
      break;
    
    case 9:
    case 10:
    case 11:
    case 12:
      size = 6;
      src_y = 57;
      
      switch (relative)
	{
	case RIGHT: src_x = 378; break;
	case LEFT: src_x = 372; break;
	case REAR: src_x = 366; break;
	case FRONT: src_x = 360; break;	  
	}
      break;
    
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
      size = 4;
      src_y = 60;

      switch (relative)
	{
	case RIGHT: src_x = 356; break;
	case LEFT: src_x = 352; break;
	case REAR: src_x = 348; break;
	case FRONT: src_x = 344; break;	  
	}
      break;
    
    default:
      size = 3;
      src_y = 57;

      switch (relative)
	{
	case RIGHT: src_x = 357; break;
	case LEFT: src_x = 354; break;
	case REAR: src_x = 351; break;
	case FRONT: src_x = 348; break;	  
	}
    }

  /* Now that the bounds of the source bitmap have been established, copy that
     region from the avatar sprite sheet to the rendering target, centering the
     avatar sprite within its chosen position in the 3-D view. */
  
  mazewar_display_render_bitmap
    (context, mazewar_avatar_bits, 
     MAZEWAR_AVATAR_BITS_WIDTH, MAZEWAR_AVATAR_BITS_HEIGHT, 
     size, size, src_x, src_y, 
     VIEW_X_ORIGIN + scrn_x - size / 2, VIEW_Y_ORIGIN + scrn_y - size / 2);
}

/* A data structure to store book-keeping information used during the rendering
   process for the first-person 3-D view. */

typedef struct _mazewar_view_context
{
  /* A pointer to the next edge in the drawing sequence for the current 
     tile. */

  mazewar_edge *current_tile_pointer;

  gboolean right_occluded; /* Is the right-hand tile a wall? */

  /* If so, these are the top and bottom edges to draw */

  mazewar_edge right_occluded_edges[2]; 

  gboolean left_occluded; /* Is the left-hand tile a wall? */

  /* If so, these are the top and bottom edges to draw */

  mazewar_edge left_occluded_edges[2];
} mazewar_view_context;

/* The following arrays serve as lookup tables for the view-drawing functions
   below; their contents, keyed by orientation (NORTH, SOUTH, EAST, WEST) are
   x, y offsets for relatively-oriented tiles. For example, if the local player
   is facing NORTH, the `l1_delta' table shows that the left-hand wall is one
   tile WEST. */

/* Tile offsets for the current left-hand tile. */

static mazewar_vertex l1_delta[4] = 
  { { -1, 0 }, { 1, 0 }, { 0, -1 }, { 0, 1 } };

/* Tile offsets for the left-hand tile one tile ahead. */

static mazewar_vertex l2_delta[4] = 
  { { -1, -1 }, { 1, 1 }, { 1, -1 }, { -1, 1 } };

/* Tile offsets for the tile directly ahead. */

static mazewar_vertex c2_delta[4] = 
  { { 0, -1 }, { 0, 1 }, { 1, 0 }, { -1, 0 } };

/* Tile offsets for the current right-hand tile. */

static mazewar_vertex r1_delta[4] = 
  { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };

/* Tile offsets for the right-hand tile one tile ahead. */

static mazewar_vertex r2_delta[4] = 
  { { 1, -1 }, { -1, 1 }, { 1, 1 }, { -1, -1 } };

/* Draws a line connecting the two vertices given by the edge E. */

static void draw_edge (mazewar_context *context, mazewar_edge *e)
{
  mazewar_display_render_line 
    (context, 
     VIEW_X_ORIGIN + e->p1.x, VIEW_Y_ORIGIN + e->p1.y, 
     VIEW_X_ORIGIN + e->p2.x, VIEW_Y_ORIGIN + e->p2.y);
}

/* The inner drawing loop for the first-person 3-D view renderer. 

   This function is adapted from the `hidden' function in the HP-UX Mazewar
   distribution by Christopher A. Kent. */

static void draw_view_inner
(mazewar_context *context, int x, int y, short dir, 
 mazewar_view_context *view_context)
{
  int l1x = x + l1_delta[dir].x; /* The x coordinate of the left-hand tile. */
  int l1y = y + l1_delta[dir].y; /* The y coordinate of the left-hand tile. */

  /* The x coordinate of the left-hand-plus-one tile. */

  int l2x = x + l2_delta[dir].x;

  /* The y coordinate of the left-hand-plus-one tile. */
  
  int l2y = y + l2_delta[dir].y;
  int r1x = x + r1_delta[dir].x; /* The x coordinate of the right-hand tile. */
  int r1y = y + r1_delta[dir].y; /* The y coordinate of the right-hand tile. */

  /* The x coordinate of the right-hand-plus-one tile. */

  int r2x = x + r2_delta[dir].x;

  /* The y coordinate of the right-hand-plus-one tile. */

  int r2y = y + r2_delta[dir].y;
  int c2x = x + c2_delta[dir].x; /* The x coordinate of the next tile. */
  int c2y = y + c2_delta[dir].y; /* The y coordinate of the next tile. */

  /* Is the next tile a wall? */
  
  gboolean edge2 = mazewar_maze_is_wall (context, c2x, c2y);

  /* Is the left-hand tile a wall? */

  gboolean edge3 = mazewar_maze_is_wall (context, l1x, l1y);
  gboolean edge4 = !edge3;

  /* Is the right-hand tile a wall? */

  gboolean edge7 = mazewar_maze_is_wall (context, r1x, r1y); 
  gboolean edge6 = !edge7;

  /* Should we draw an unbroken edge along the "ceiling" from the left-hand
     tile to the left-hand-plus-one tile? */
  
  gboolean edge1 = 
    (edge3 && (edge2 || !mazewar_maze_is_wall (context, l2x, l2y)))
    || (!edge2 && edge4);

  /* Should we draw an unbroken edge along the "ceiling" from the right-hand
     tile to the right-hand-plus-one tile? */

  gboolean edge5 = 
    (edge7 && (edge2 || !mazewar_maze_is_wall (context, r2x, r2y)))
    || (!edge2 && edge6);

  /* Draw the left-hand ceiling edge if necessary. */

  if (edge1)
    draw_edge (context, view_context->current_tile_pointer);
  view_context->current_tile_pointer++;
  
  if (edge2)
    {
      draw_edge (context, view_context->current_tile_pointer); /* Top. */

      /* Bottom. */

      draw_edge (context, view_context->current_tile_pointer + 1); 
    }
  view_context->current_tile_pointer += 2;

  if (edge3) 
    {
      if (view_context->right_occluded) 
	{
	  view_context->right_occluded_edges[0].p2 = 
	    (view_context->current_tile_pointer++)->p2;
	  view_context->right_occluded_edges[1].p2 = 
	    view_context->current_tile_pointer->p2;
	} 
      else 
	{
	  view_context->right_occluded_edges[0] = 
	    *view_context->current_tile_pointer++;
	  view_context->right_occluded_edges[1] = 
	    *view_context->current_tile_pointer;
	  view_context->right_occluded = TRUE;
	}
      view_context->current_tile_pointer++;
    } 
  else 
    {
      if (view_context->right_occluded) 
	{
	  draw_edge (context, &view_context->right_occluded_edges[0]);
	  draw_edge (context, &view_context->right_occluded_edges[1]);
	  view_context->right_occluded = FALSE;
	}
      view_context->current_tile_pointer += 2;
    }

  /* Draw the left-hand opposing wall's top and bottom segments, if the
     left-hand tile is not a wall. */
  
  if (edge4)
    {
      draw_edge (context, view_context->current_tile_pointer);
      draw_edge (context, view_context->current_tile_pointer + 1);
    }
  view_context->current_tile_pointer += 2;

  /* Draw the right-hand ceiling edge if necessary. */

  if (edge5)
    draw_edge (context, view_context->current_tile_pointer);
  view_context->current_tile_pointer++;

  /* Draw the right-hand opposing wall's top and bottom segments, if the
     right-hand tile is not a wall. */
  
  if (edge6)
    {
      draw_edge (context, view_context->current_tile_pointer);
      draw_edge (context, view_context->current_tile_pointer + 1);
    }
  view_context->current_tile_pointer += 2;

  if (edge7) 
    {
      if (view_context->left_occluded) 
	{
	  view_context->left_occluded_edges[0].p1 = 
	    (view_context->current_tile_pointer++)->p1;
	  view_context->left_occluded_edges[1].p1 = 
	    view_context->current_tile_pointer->p1;
	} 
      else 
	{
	  view_context->left_occluded_edges[0] = 
	    *view_context->current_tile_pointer++;
	  view_context->left_occluded_edges[1] = 
	    *view_context->current_tile_pointer;
	  view_context->left_occluded = TRUE;
	}
      view_context->current_tile_pointer++;
    } 
  else 
    {
      if (view_context->left_occluded) 
	{
	  draw_edge (context, &view_context->left_occluded_edges[0]);
	  draw_edge (context, &view_context->left_occluded_edges[1]);
	  view_context->left_occluded = FALSE;
	}
      view_context->current_tile_pointer += 2;
    }
}

/* Draws the first-person 3-D view of the maze from the point of view of the
   local player, based on their current location and orientation within the
   maze. 

   This procedure simulates a camera that starts at the local player's current
   tile and advances through the maze relative to the player's orientation
   until it hits a wall. */

void mazewar_display_draw_view (mazewar_context *context)
{
  mazewar_view_context view_context; /* The view-drawing context. */

  GList *players = context->players;
  int x = context->self->x; /* The starting x position for the "camera." */
  int y = context->self->y; /* The starting y position for the "camera." */
  int dir = context->self->dir; /* The orientation of the "camera." */
  
  /* Clear the current first-person view. */
  
  mazewar_display_clear_area 
    (context, VIEW_X_ORIGIN, VIEW_Y_ORIGIN, VIEW_X_DIM, VIEW_Y_DIM);

  /* Initialize the view-drawing context with some initial values. */
  
  view_context.current_tile_pointer = mazewar_view_edges;
  view_context.right_occluded = FALSE;
  view_context.left_occluded = FALSE;

  /* Iteratively draw further and further tiles by advancing the coordinates
     appropriately and calling the inner drawing function until a wall tile is 
     encountered. */

  while (!mazewar_maze_is_wall (context, x, y))
    {
      /* Draw the camera's current tile. */

      draw_view_inner (context, x, y, dir, &view_context);

      /* Advance the camera in the appropriate direction. */

      switch (dir) 
	{
	case NORTH: y--; break;
	case SOUTH: y++; break;
	case EAST: x++; break;
	case WEST: x--; break;
	}
    }

  /* Perform some last-iteration drawing fix-ups for top and bottom edges that
     need to be drawn in for the final tile. */

  if (view_context.right_occluded)
    {
      draw_edge (context, &view_context.right_occluded_edges[0]); /* Top. */
      draw_edge (context, &view_context.right_occluded_edges[1]); /* Bottom. */
    }
  if (view_context.left_occluded)
    {
      draw_edge (context, &view_context.left_occluded_edges[0]); /* Top. */
      draw_edge (context, &view_context.left_occluded_edges[1]); /* Bottom. */
    }

  /* Iterate over the players to see if any need to be drawn into the view. */

  while (players != NULL)
    {
      mazewar_player *player = (mazewar_player *) players->data;

      /* If the player is not the local player and is visible, then draw their
	 avatar sprite on top of the 3-D view. */

      if (player != context->self && player->visible)
	draw_avatar (context, player);

      players = players->next;
    }
}

/* Draw the arrow representing the current orientation DIR of the local player
   on the overhead view of the maze at the specified X, Y position within the 
   maze. */

void mazewar_display_draw_arrow_self
(mazewar_context *context, int x, int y, short dir)
{
  /* Transfer the orientation-specific bitmap from the array of "local player"
     arrow bitmaps to the rendering target. */

  mazewar_display_render_bitmap 
    (context, normal_arrows[dir], 
     8, 8, 8, 8, 0, 0, MAZE_X_ORIGIN + x * 8, MAZE_Y_ORIGIN + y * 8);
}

/* Draw the arrow representing the current orientation DIR of another player on
   the overhead view of the maze at the specified X, Y position within the 
   maze. */

void mazewar_display_draw_arrow_other 
(mazewar_context *context, int x, int y, short dir)
{
  /* Transfer the orientation-specific bitmap from the array of "other player"
     arrow bitmaps to the rendering target. */
  
  mazewar_display_render_bitmap 
    (context, other_arrows[dir], 
     8, 8, 8, 8, 0, 0, MAZE_X_ORIGIN + x * 8, MAZE_Y_ORIGIN + y * 8);
}

/* Erase the arrow indicator from the overhead view of the maze using the
   specified X, Y position within the maze. */

void mazewar_display_clear_arrow (mazewar_context *context, int x, int y)
{
  /* A blank white square to be transferred to the rendering target instead of
     an arrow bitmap. */

  static unsigned char empty[8] = 
    { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };

  mazewar_display_render_bitmap 
    (context, empty, 
     8, 8, 8, 8, 0, 0, MAZE_X_ORIGIN + x * 8, MAZE_Y_ORIGIN + y * 8);
}

/* Draw the scorecard for the current players. */

void mazewar_display_draw_scorecard (mazewar_context *context)
{
  GList *players = context->players; /* The player list. */
  int idx = 0; /* The position of each player within the list. */

  /* Erase the entire score area in preparation for redraw. */

  mazewar_display_clear_area 
    (context, SCORE_X_ORIGIN, SCORE_Y_ORIGIN, SCORE_X_DIM, SCORE_Y_DIM);
  
  while (players != NULL)
    {
      mazewar_player *player = (mazewar_player *) players->data;

      /* Draw the score at the appropriate line. */

      write_score (context, player, idx++); 
      players = players->next;
    }
}

/* Draw the overhead view of the maze by scaling the maze bitmap from the
   mazewar context by a factor of 8, and then transferring it to the rendering
   target. This may not be the most efficient way to paint the maze, but should
   only need to happen once. */

void mazewar_display_draw_maze (mazewar_context *context)
{
  /* The scaled maze bitmap. */

  unsigned char *maze = scale_bitmap 
    (context->maze, context->width, context->height, 8, 8);

  int w = context->width * 8; /* The width of the scaled bitmap. */
  int h = context->height * 8; /* The height of the scaled bitmap. */

  mazewar_display_render_bitmap 
    (context, maze, w, h, w, h, 0, 0, MAZE_X_ORIGIN, MAZE_Y_ORIGIN);  

  free (maze); /* Free the scaled bitmap. */
}
