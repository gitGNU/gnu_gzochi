/* input-sdl.c: SDL-based input implementation for gzochi mazewar example game
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

#include <SDL/SDL.h>

#include "input.h"

/* Initializes the input sub-system. At the moment, this is a no-op. */

void mazewar_input_init ()
{
}

/* Waits for and returns the next input event from the user. */

int mazewar_input_next_event ()
{
  SDL_Event event;
  
  while (1)
    {
      /* Wait unil a new event has been fired by SDL's input library. If it can
	 be handled and return, do so; otherwise wait for another event, and so
	 on. */
      
      SDL_WaitEvent (&event);

      if (event.type == SDL_KEYDOWN) /* Was it keyboard event? */
	{
	  /* Translate from the set of possible SDL keyboard events to the
	     implementation-agnostic set of Mazewar keyboard event types. */

	  switch (event.key.keysym.sym)
	    {
	    case SDLK_s: return MAZEWAR_INPUT_KEY_S;
	    case SDLK_LEFT: return MAZEWAR_INPUT_KEY_LEFT;
	    case SDLK_f: return MAZEWAR_INPUT_KEY_F;
	    case SDLK_RIGHT: return MAZEWAR_INPUT_KEY_RIGHT;
	    case SDLK_d: return MAZEWAR_INPUT_KEY_D;
	    case SDLK_UP: return MAZEWAR_INPUT_KEY_UP;
	    case SDLK_RCTRL: return MAZEWAR_INPUT_KEY_RCTRL;
	    case SDLK_LCTRL: return MAZEWAR_INPUT_KEY_LCTRL;
	    default:
	      break;
	    }	  
	}
      else if (event.type == SDL_QUIT) /* Was it the `quit' event? */
	return MAZEWAR_INPUT_EVENT_QUIT;
    }

  return 0;
}
