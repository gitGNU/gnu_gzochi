/* input.h: Prototypes and declarations for input-[driver].c
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

#ifndef MAZEWAR_INPUT_H
#define MAZEWAR_INPUT_H

/* The set of input event described below is an attempt to offer compatibility 
   with the "classic" Mazewar controls while also supporting a key layout that 
   will be familiar to contemporary players. */

#define MAZEWAR_INPUT_KEY_S 115 /* The 's' key was pressed. */
#define MAZEWAR_INPUT_KEY_LEFT 276 /* The left directional key was pressed. */
#define MAZEWAR_INPUT_KEY_F 102 /* The 'f' key was pressed. */

/* The right directional key was pressed. */

#define MAZEWAR_INPUT_KEY_RIGHT 275 

#define MAZEWAR_INPUT_KEY_D 100 /* The 'd' key was pressed. */
#define MAZEWAR_INPUT_KEY_UP 273 /* The up directional key was pressed. */
#define MAZEWAR_INPUT_KEY_RCTRL 305 /* The right ctrl key was pressed. */
#define MAZEWAR_INPUT_KEY_LCTRL 306 /* The left ctrl key was pressed. */

#define MAZEWAR_INPUT_EVENT_QUIT 0 /* The user signaled a quit event. */

void mazewar_input_init (void); /* Initializes the input sub-system. */

/* Waits for and returns the next input event from the user. */

int mazewar_input_next_event (void);

#endif /* MAZEWAR_INPUT_H */
