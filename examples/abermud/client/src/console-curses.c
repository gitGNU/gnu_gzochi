/* console-curses.c: Curses integration for gzochi abermud example game
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

#include <curses.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "abermud.h"
#include "console.h"

#define OUTPUT_HEIGHT LINES - 7
#define INPUT_HEIGHT 6

/* 
   Initialize the console and return a pointer to a new console state object
   that shares input state with the game context via the abermud_input_buffer 
   INPUT_BUFFER. 
 */

abermud_console_state *
abermud_console_init (abermud_input_buffer *input_buffer)
{
  abermud_console_state *state = malloc (sizeof (abermud_console_state));
  
  initscr ();

  state->title = newwin (1, COLS, 0, 0);
  state->output = newwin (OUTPUT_HEIGHT, COLS, 1, 0);
  state->input = newwin (INPUT_HEIGHT, COLS, OUTPUT_HEIGHT + 1, 0);

  state->input_buffer = input_buffer;
  state->prompt = NULL;

  scrollok (state->output, TRUE);
  idlok (state->title, TRUE);
  idlok (state->output, TRUE);
  idlok (state->input, TRUE);
  keypad (state->input, TRUE);
  noecho (); 
  nodelay (state->input, TRUE);

  return state;
}

/*
  Appends the content of OUTPUT to the console's server output window. The
  output window will be scrolled up as necessary to accommodate the lines in
  OUTPUT. 
 */

void 
abermud_console_append_output (abermud_console_state *state, char *output)
{
  int i = 0, len = strlen (output);
  
  scroll (state->output);
  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
  
  for (; i < len; i++) 
    {
      if (output [i] == '\n')
	{
	  scroll (state->output);
	  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
	  wclrtoeol (state->output);
	  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
	}
      else waddch (state->output, output[i]);
    }

  wrefresh (state->output);

  /* The cursor will be in the output window after writing to it, so send it
     back to the current offset within the input area. */

  abermud_console_input_cursor_to (state, state->input_buffer->pos);
}

/*
  Sets the console's input prompt to the contents of PROMPT and redraws the 
  input window, including the current contents of the input buffer. The 
  console stores a copy of PROMPT to allow the input area to be redrawn as 
  necessary. 
 */

void 
abermud_console_set_prompt (abermud_console_state *state, char *prompt)
{
  /* Free any existing prompt data. */

  if (state->prompt != NULL)
    free (state->prompt);

  state->prompt = strndup (prompt, strlen (prompt));

  werase (state->input);
  wprintw (state->input, "%s", prompt);
  wmove (state->input, 0, strlen (prompt));
  wprintw (state->input, "%s", state->input_buffer->data);
  abermud_console_input_cursor_to (state, state->input_buffer->pos);
  wrefresh (state->input);
}

/*
  Set the console's title text (i.e., the content of the title window) to the
  contents of TITLE. 
 */

void 
abermud_console_set_title (abermud_console_state *state, char *title)
{
  int i = 0, len = strlen (title);
  
  wattron (state->title, A_REVERSE);
  wclrtoeol (state->title);
  mvwaddnstr (state->title, 0, 0, title, len);
  
  for (; i < COLS - len; i++) 
    waddch (state->title, ' ');

  wattroff (state->title, A_REVERSE);
  wrefresh (state->title);
}

/*
  Renders the contents of the console's input area (i.e., everything following
  the prompt) based on the state of the shared input buffer. This function 
  should be called whenever the state of the input buffer changes. 

  This function currently does its work directly (instead of enqueueing it)
  because it is only called from the Ncurses thread.
*/

void 
abermud_console_draw_input (abermud_console_state *state)
{
  wmove (state->input, 0, strlen (state->prompt));
  wclrtoeol (state->input);
  wprintw (state->input, "%s", state->input_buffer->data);
  abermud_console_input_cursor_to (state, state->input_buffer->pos);
  wrefresh (state->input);
}

/*
  Send the console's cursor to the specified offset OFFSET within the input 
  area, relative to the current prompt. 
*/

void 
abermud_console_input_cursor_to (abermud_console_state *state, int offset)
{
  int x, y;
  int pos = offset;
  
  /* If a prompt has been set, take its length into account when finding the
     window offset. */

  if (state->prompt != NULL)
    pos += strlen (state->prompt);

  x = pos % COLS;
  y = pos / COLS;
  
  wmove (state->input, y, x);
  wrefresh (state->input);
}

/*
  Shuts down the console, freeing all the resources (such as curses WINDOWs) 
  that have been allocated to it. `abermud_console_init' must be called before
  further console operations can be performed. 
 */

void 
abermud_console_shutdown (abermud_console_state *state)
{
  endwin ();
  free (state);
}

/*
  Reads and returns a character from the input area (i.e., the input window)
  of the console. Since the Ncurses input system is set to wait for input for a
  maximum of 100ms (see above), dispatch any waiting console events in between
  waiting for input.
 */

int 
abermud_console_getch (abermud_console_state *state)
{
  int ret = wgetch (state->input);
  return ret == ERR ? 0 : ret;
}
