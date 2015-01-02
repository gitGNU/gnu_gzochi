/* abermud.h: Global declarations for gzochi abermud example game
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

#ifndef ABERMUD_H
#define ABERMUD_H

#include <libgzochi-glib.h>

#include "console.h"

/* The abermud game context. Stores local information about game state. */

typedef struct _abermud_context
{
  int allow_input; /* 1 if input is currently allowed; otherwise 0. */
  int echo; /* 1 if typed characters are echoed to the screen; otherwise 0. */

  gzochi_glib_client_session *session; /* The gzochi client session. */

  /* This input buffer is shared between this context object and its embedded
     console state to make information about the state of the input area
     visible both to the game code in abermud.c and to the lower-level console
     rendering implementation. */
       
  abermud_input_buffer *input_buffer;

  /* The local console state for this game. See console.h. */

  abermud_console_state *console_state;
} abermud_context;

#endif /* ABERMUD_H */
