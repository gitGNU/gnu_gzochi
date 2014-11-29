/* display-sdl.c: SDL-based rendering layer for gzochi mazewar example game
 * Copyright (C) 2014 Julian Graham
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

#include <math.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/time.h>

#include <SDL/SDL.h>

#include "display.h"
#include "mazewar.h"

#include "data/avatars.h"
#include "data/font.h"

/* The renderer-specific display context type. In the case of SDL, just wraps a
   window surface. */

typedef struct _mazewar_sdl_display_context {
  SDL_Surface *window; /* The main window. */
} mazewar_sdl_display_context;

/* Draws a single character C at the specified pixel location X, Y by copying
   the bitmap region for the character from the font bitmap. */

static void draw_char (mazewar_context *context, int x, int y, unsigned char c)
{
  mazewar_display_render_bitmap
    (context, mazewar_font_8x13_bytes, 8, 2912, 8, 13, 0, c * 13, x, y);
}

/* Sets a pixel on the surface SURFACE at the specified X, Y coordinates to
   the RGBA color COLOR. */

static void set_pixel (SDL_Surface *surface, int x, int y, Uint32 color)
{
 /* Assume a 32-bit pixel format. */

  Uint32 *pixels = (Uint32 *) surface->pixels;
  pixels[y * surface->w + x] = color;
}

/* Draws a black line on the rendering target from the point X1, Y1 to the
   point X2, Y2. */

void mazewar_display_render_line 
(mazewar_context *context, int x1, int y1, int x2, int y2)
{
  mazewar_sdl_display_context *display_context = 
    (mazewar_sdl_display_context *) context->display_context;

  int i = 0;
  double x = x2 - x1; /* The x distance. */
  double y = y2 - y1; /* The y distance. */
  double length = sqrt (x * x + y * y); /* The Pythagorean distance formula. */
  
  double addx = x / length; /* The size of each horizontal line subsegment. */
  double addy = y / length; /* The size of each vertical line subsegment. */

  Uint32 color = SDL_MapRGB (display_context->window->format, 0x00, 0x00, 0x00);
  
  /* Start at X1, Y1. */

  x = x1;
  y = y1;
  
  SDL_LockSurface (display_context->window);

  for (; i < length; i++)
    {
      /* Draw a pixel. */
      
      set_pixel (display_context->window, (int) x, (int) y, color);

      /* Increment the x and y coordinates. */

      x += addx;
      y += addy;
    }

  SDL_UnlockSurface (display_context->window);
}

/* Copy the region of the bitmap DATA originating at SRC_X and SRC_Y and 
   extending SRC_WIDTH pixels horizontally and SRC_HEIGHT pixels vertically to
   the location given by DST_X and DST_Y on the rendering target.
   
   The bmp_width and bmp_height parameters give the dimensions of the bitmap,
   which are neessary for extracting the pixel data. */

void mazewar_display_render_bitmap 
(mazewar_context *context, unsigned char *data,
 int bmp_width, int bmp_height, 
 int src_width, int src_height, 
 int src_x, int src_y, 
 int dst_x, int dst_y)
{
  mazewar_sdl_display_context *display_context = 
    (mazewar_sdl_display_context *) context->display_context;

  /* Calculate and cache black and white pixel values. */

  int black = SDL_MapRGB (display_context->window->format, 0x00, 0x00, 0x00);
  int white = SDL_MapRGB (display_context->window->format, 0xff, 0xff, 0xff);

  int i = 0, j = 0;
  
  SDL_LockSurface (display_context->window);

  /* Iterate over the two-dimensional extent of the region. */

  for (; i < src_height; i++)
    {
      for (; j < src_width; j++)
	{
	  /* Find the pixel offset in the source bitmap. */

	  int pixel = (src_y + i) * bmp_width + src_x + j;
	  int byte = pixel / 8; /* Which byte contains the pixel? */

	  /* Extract the pixel value from the source byte. */

	  int bit = data[byte] & (1 << (7 - (pixel % 8)));

	  /* Set the pixel value on the destination surface. */

	  set_pixel (display_context->window, 
		     dst_x + j, dst_y + i, bit ? white : black);
	}
      j = 0;
    }

  SDL_UnlockSurface (display_context->window);
}

/* Write the characters of the NULL-terminated string TEXT to the rendering
   target at the specified X, Y coordinates. */

void mazewar_display_render_text 
(mazewar_context *context, char *text, int x, int y)
{
  int i = 0, text_len = strlen (text); /* The number of character to write. */

  /* Draw each character in the string at 8-pixel horizontal intervals. */
  
  for (; i < text_len; i++)
    draw_char (context, x + i * 8, y, text[i]); 
}

/* Flush any pending updates to the rendering target. */

void mazewar_display_refresh (mazewar_context *context)
{
  mazewar_sdl_display_context *display_context = 
    (mazewar_sdl_display_context *) context->display_context;

  SDL_Flip (display_context->window); /* Delegate to SDL for refresh. */
}

/* Clear all marks from the specified region originating at position X, Y of
   the rendering target and extending W pixels horizontally and H pixels 
   vertically. All pixels in this region will be set to white. */

void mazewar_display_clear_area 
(mazewar_context *context, int x, int y, int w, int h)
{
  mazewar_sdl_display_context *display_context = 
    (mazewar_sdl_display_context *) context->display_context;
  SDL_Rect rect; /* The target region, as an SDL_Rect. */

  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;

  /* Have SDL fill the rectangle with white. */

  SDL_FillRect (display_context->window, &rect, 
		SDL_MapRGB (display_context->window->format, 0xff, 0xff, 0xff));
}

/* Invert the region of the rendering target originating at X, Y and extending
   W pixels horizontally and H pixels vertically, changing all white pixels to
   black and black pixels to white. */

void mazewar_display_invert_area
(mazewar_context *context, int x, int y, int w, int h)
{
  mazewar_sdl_display_context *display_context = 
    (mazewar_sdl_display_context *) context->display_context;
  int black = SDL_MapRGB (display_context->window->format, 0x00, 0x00, 0x00);
  int white = SDL_MapRGB (display_context->window->format, 0xff, 0xff, 0xff);
  int i = 0, j = 0;
  
  SDL_LockSurface (display_context->window);

  /* Iterate over the two-dimensional extent of the region. */

  for (; i < h; i++)
    {
      for (; j < w; j++)
	{
	  /* Calculate the offset of the current pixel in the pixel array. */

	  int pixel = ((y + i) * display_context->window->w) + x + j;
	  Uint32 p = ((Uint32 *) display_context->window->pixels)[pixel];

	  /* If P is black, change it to white, and vice versa. */

	  ((Uint32 *) display_context->window->pixels)[pixel] = 
	    p ? black : white;
	}
      j = 0;
    }

  SDL_UnlockSurface (display_context->window);
}

/* Initialize the display, returning an opaque pointer representing the
   "display context." */

gpointer mazewar_display_init (mazewar_context *context)
{
  mazewar_sdl_display_context *display_context = 
    malloc (sizeof (mazewar_sdl_display_context));
  Uint32 color = 0;

  /* Create the window as a 32-bit-per-pixel SDL surface. Try to take advantage
     of video memory and double-buffering if available. */

  display_context->window = 
    SDL_SetVideoMode (MAZEWAR_X_DIM, MAZEWAR_Y_DIM, 32, 
		      SDL_HWSURFACE | SDL_DOUBLEBUF);

  /* Fill the window with white to start with. */
  
  color = SDL_MapRGB (display_context->window->format, 0xff, 0xff, 0xff);
  SDL_FillRect (display_context->window, NULL, color);
 
  return display_context;
}
