/* console.h: Prototypes and declarations for console-curses.c
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

#ifndef ABERMUD_CONSOLE_H
#define ABERMUD_CONSOLE_H

#include <curses.h>

#define ABERMUD_KEY_LEFT KEY_LEFT
#define ABERMUD_KEY_RIGHT KEY_RIGHT
#define ABERMUD_KEY_BACKSPACE KEY_BACKSPACE

#define ABERMUD_INPUT_BUFFER_SIZE 100

/* The console's input buffer stores the contents of the input region along
   with the current offset of the cursor within that region. */

typedef struct _abermud_input_buffer
{
  char data[ABERMUD_INPUT_BUFFER_SIZE];
  int pos;
} abermud_input_buffer;

typedef struct _abermud_console_state 
{
  /* The title bar. After login, this window contains a short description of 
     the current room. */

  WINDOW *title;

  WINDOW *output; /* The output region. Displays messages from the server. */
  WINDOW *input; /* The input region. Displays characters typed locally. */
 
  /* The text of the prompt displayed at the front of the input region. The
     prompt cannot be modified by the user's edits and is not sent to the
     server when the player submits a command. */

  char *prompt;

  /* The shared input buffer. See abermud,h. */

  abermud_input_buffer *input_buffer;
} abermud_console_state;

/* Initialize the console, returning a pointer to a new console state object
   configured with the specified input buffer. */
abermud_console_state *abermud_console_init (abermud_input_buffer *);

/* Append server output to the console, scrolling the existing content up as 
   necessary. */
void abermud_console_append_output (abermud_console_state *, char *);
/* Set the prompt for the console's input area, redrawing the input buffer's
   contents as necessary. */
void abermud_console_set_prompt (abermud_console_state *, char *);
/* Set the console's title text. */
void abermud_console_set_title (abermud_console_state *, char *);

/* Read and return a character from the console. */
int abermud_console_getch (abermud_console_state *);
/* Send the console's cursor to the specified offset in the input area,
   relative to the current prompt. */
void abermud_console_input_cursor_to (abermud_console_state *, int);
/* Renders the contents of the console's input area. This function should be
   called whenever the state of the console's input buffer changes. */
void abermud_console_draw_input (abermud_console_state *);

/* Shuts down the console, freeing all the resources (such as curses WINDOWs) 
   that have been allocated to it. */
void abermud_console_shutdown (abermud_console_state *);

#endif /* ABERMUD_CONSOLE_H */
