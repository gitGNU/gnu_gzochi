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
#include <pthread.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "abermud.h"
#include "console.h"

#define OUTPUT_HEIGHT LINES - 7
#define INPUT_HEIGHT 6

/*
  A simple mechanism for enqueuing requests to update the console. Ncurses is
  not (easily) thread-safe, so this ensures that all calls to the Ncurses API
  and which may mutate Ncurses global state originate from the same thread.
 */

typedef enum 
  {
    ABERMUD_CONSOLE_NONE,
    ABERMUD_CONSOLE_APPEND_OUTPUT,
    ABERMUD_CONSOLE_SET_TITLE,
    ABERMUD_CONSOLE_SET_PROMPT
  }
  abermud_console_event_type;

static pthread_mutex_t console_event_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t console_event_cond = PTHREAD_COND_INITIALIZER;

static abermud_console_event_type console_event_type = ABERMUD_CONSOLE_NONE;
static char *console_event_text = NULL;

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

  /* Limit the amount of time Ncurses will wait for input to 1 tenth of a 
     second. */

  halfdelay (1);

  return state;
}

/*
  Appends the content of OUTPUT to the console's server output window. The
  output window will be scrolled up as necessary to accommodate the lines in
  OUTPUT. 
 */

static void 
append_output (abermud_console_state *state)
{
  int i = 0, len = strlen (console_event_text);
  
  scroll (state->output);
  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
  
  for (; i < len; i++) 
    {
      if (console_event_text [i] == '\n')
	{
	  scroll (state->output);
	  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
	  wclrtoeol (state->output);
	  wmove (state->output, OUTPUT_HEIGHT - 1, 0);
	}
      else waddch (state->output, console_event_text[i]);
    }

  wrefresh (state->output);

  /* The cursor will be in the output window after writing to it, so send it
     back to the current offset within the input area. */

  abermud_console_input_cursor_to (state, state->input_buffer->pos);
}

/*
  Enqueue a request to add output to the console.
 */

void 
abermud_console_append_output (abermud_console_state *state, char *output)
{
  pthread_mutex_lock (&console_event_lock);
  
  while (console_event_type != ABERMUD_CONSOLE_NONE)
    pthread_cond_wait (&console_event_cond, &console_event_lock);

  console_event_type = ABERMUD_CONSOLE_APPEND_OUTPUT;
  console_event_text = strdup (output);

  pthread_mutex_unlock (&console_event_lock);
}

/*
  Sets the console's input prompt to the contents of PROMPT and redraws the 
  input window, including the current contents of the input buffer. The 
  console stores a copy of PROMPT to allow the input area to be redrawn as 
  necessary. 
 */

static void 
set_prompt (abermud_console_state *state)
{
  werase (state->input);
  wprintw (state->input, "%s", state->prompt);
  wmove (state->input, 0, strlen (state->prompt));
  wprintw (state->input, "%s", state->input_buffer->data);
  abermud_console_input_cursor_to (state, state->input_buffer->pos);
  wrefresh (state->input);
}

/*
  Enqueue a request to set the console prompt.
 */

void 
abermud_console_set_prompt (abermud_console_state *state, char *prompt)
{
  pthread_mutex_lock (&console_event_lock);
  
  /* Free any existing prompt data. */

  if (state->prompt != NULL)
    free (state->prompt);

  state->prompt = strndup (prompt, strlen (prompt));

  while (console_event_type != ABERMUD_CONSOLE_NONE)
    pthread_cond_wait (&console_event_cond, &console_event_lock);

  console_event_type = ABERMUD_CONSOLE_SET_PROMPT;
  pthread_mutex_unlock (&console_event_lock);
}

/*
  Set the console's title text (i.e., the content of the title window) to the
  contents of TITLE. 
 */

static void 
set_title (abermud_console_state *state)
{
  int i = 0, len = strlen (console_event_text);
  
  wattron (state->title, A_REVERSE);
  wclrtoeol (state->title);
  mvwaddnstr (state->title, 0, 0, console_event_text, len);
  
  for (; i < COLS - len; i++) 
    waddch (state->title, ' ');

  wattroff (state->title, A_REVERSE);
  wrefresh (state->title);
}

/*
  Enqueue a request to set the title.
 */

void 
abermud_console_set_title (abermud_console_state *state, char *title)
{
  pthread_mutex_lock (&console_event_lock);
  
  while (console_event_type != ABERMUD_CONSOLE_NONE)
    pthread_cond_wait (&console_event_cond, &console_event_lock);

  console_event_type = ABERMUD_CONSOLE_SET_TITLE;
  console_event_text = strdup (title);

  pthread_mutex_unlock (&console_event_lock);  
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
  Empty the console event state to indicate that a new event may be enqueued.
 */

static void 
clear_console_event (void)
{
  console_event_type = ABERMUD_CONSOLE_NONE;

  if (console_event_text != NULL)
    {
      free (console_event_text);
      console_event_text = NULL;
    }
}

/*
  Dispatch the enqueued event, if any.
 */

static void 
dispatch_console_event (abermud_console_state *state)
{
  pthread_mutex_lock (&console_event_lock);

  switch (console_event_type)
    {
    case ABERMUD_CONSOLE_APPEND_OUTPUT: append_output (state); break;
    case ABERMUD_CONSOLE_SET_TITLE: set_title (state); break;
    case ABERMUD_CONSOLE_SET_PROMPT: set_prompt (state); break;
    default: break;
    }

  clear_console_event ();

  pthread_cond_signal (&console_event_cond);
  pthread_mutex_unlock (&console_event_lock);
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
  while (TRUE)
    {
      int ret = wgetch (state->input);

      if (ret != ERR)
	return ret;

      dispatch_console_event (state);
    }
}
