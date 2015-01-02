/* abermud: An example implementation of AberMUD for gzochi
 * abermud.c: Main loop implementation for gzochi abermud example game
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

#include <glib.h>
#include <libgzochi-glib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "abermud.h"
#include "console.h"

#define INPUT_POLL_INTERVAL_MS 10 /* The delay between polls for input. */

/* Opcode constants for packets originating on the server. */

#define ABERMUD_PACKET_TYPE_ECHO_ON 0x05 /* Switch console echo on. */
#define ABERMUD_PACKET_TYPE_ECHO_OFF 0x06 /* Switch console echo off. */
#define ABERMUD_PACKET_TYPE_INPUT_ON 0x07 /* Allow input from the console. */
/* Disallow input from the console. */
#define ABERMUD_PACKET_TYPE_INPUT_OFF 0x08 
#define ABERMUD_PACKET_TYPE_SHUTDOWN 0x51 /* Terminate the game session. */
#define ABERMUD_PACKET_TYPE_SET_PROMPT 0x83 /* Set the console input prompt. */
/* New console output from the server. */
#define ABERMUD_PACKET_TYPE_OUTPUT 0x81 
#define ABERMUD_PACKET_TYPE_SET_TITLE 0xf0 /* Set the console title text. */

/* Creates and returns a new abermud game context. A new shared input buffer
   will be created and used to construct a new console state object. */ 

static abermud_context *
make_abermud_context ()
{
  abermud_context *context = malloc (sizeof (abermud_context));
  abermud_input_buffer *input_buffer = 
    calloc (1, sizeof (abermud_input_buffer));

  context->echo = FALSE;
  context->allow_input = FALSE;
  context->input_buffer = input_buffer;
  context->console_state = abermud_console_init (input_buffer);

  return context;
}

/* Adds a character to the specified context's input buffer at the buffer's
   current cursor position. The portion of the input buffer that comes after
   the cursor will be shifted over to accomodate the new character. */

static void 
char_insert (abermud_context *context, char c)
{
  char *x = context->input_buffer->data + context->input_buffer->pos;
  int ct = strlen (x);
  
  while (ct >= 0)
    {
      x[ct + 1] = x[ct];
      ct--;
    }

  context->input_buffer->data[context->input_buffer->pos++] = c;
}

/* Shuts down the game context, including the console, and exits with the
   code given by EXIT_CODE. */

static void 
shutdown (abermud_context *context, int exit_code)
{
  abermud_console_shutdown (context->console_state);
  free (context->input_buffer);
  free (context);
  exit (exit_code);
}

/* Sends the contents of MESSAGE to the game server. */

static void 
send_message (abermud_context *context, char *message)
{
  gzochi_glib_client_send 
    (context->session, (unsigned char *) message, strlen (message));
}

/* Reads an input event from the console and dispatches to the appropriate 
   handling logic. Alphanumeric, punctuation, and space characters are stored
   in the shared input buffer and the position of the cursor is advanced. The
   newline character causes the contents of the input buffer to be submitted to
   the server. As per the original AberMUD client code, some limited 
   line-editing capabilities are available: on compatible keyboards, the LEFT 
   and RIGHT keys will move the cursor left and right within the input area,
   and the BACKSPACE key will delete the preceding character. */

static void 
handle_input (abermud_context *context)
{
  int ch = abermud_console_getch (context->console_state);

  switch (ch)
    {
    case '\n':

      /* Send the contents of the buffer to the server as a command. The input
	 buffer is cleared and input is disabled until explicitly re-enabled by
	 a packet from the server. */

      context->allow_input = FALSE;
      send_message (context, context->input_buffer->data);
      memset (context->input_buffer->data, 0, ABERMUD_INPUT_BUFFER_SIZE);
      context->input_buffer->pos = 0;

      abermud_console_draw_input (context->console_state);

      break;

    case ABERMUD_KEY_LEFT:

      /* Move the cursor left. */
      
      if (context->input_buffer->pos > 0)
	{
	  context->input_buffer->pos--;
	  if (context->echo)
	    abermud_console_input_cursor_to
	      (context->console_state, context->input_buffer->pos);
	}
      break;

    case ABERMUD_KEY_RIGHT:

      /* Move the cursor right. */

      if (context->input_buffer->pos < strlen (context->input_buffer->data))
	{
	  context->input_buffer->pos++;
	  if (context->echo)
	    abermud_console_input_cursor_to
	      (context->console_state, context->input_buffer->pos);
	}
      break;

    case ABERMUD_KEY_BACKSPACE:

      /* Delete the previous character, shifting the subsequent characters in
	 the buffer left to fill the gap. */

      if (context->input_buffer->pos > 0)
	{
	  context->input_buffer->pos--;
	  strncpy
	    (context->input_buffer->data + context->input_buffer->pos, 
	     context->input_buffer->data + context->input_buffer->pos + 1, 
	     strlen (context->input_buffer->data) - 
	     context->input_buffer->pos + 1);

	  if (context->echo)
	    abermud_console_draw_input (context->console_state);
	}
      break;

    case ABERMUD_KEY_NONE: break;

    default:
      
      /* If the buffer's at capacity or the typed character isn't valid, don't
	 update the buffer. */

      if (context->input_buffer->pos == ABERMUD_INPUT_BUFFER_SIZE 
	  || ch < 32 
	  || ch > 127)
	beep ();
      else
	{
	  /* Add the character to the buffer, drawing it to the console if 
	     echo is on. */

	  char_insert (context, ch);
	  if (context->echo)
	    abermud_console_draw_input (context->console_state);
	}
    }
}

/* This is the primary input handling thread for the game. It initializes the
   input system and then loops forever, passing events to the handle_input 
   procedure. The context object is passed as the void pointer arg. */

static gboolean
input_timeout_function (gpointer data)
{
  abermud_context *context = data;
  handle_input (context);
  return TRUE;
}

/* The client `disconnected' callback. Called when the client is disconnected
   by the server, either explicitly or as the result of a network error. The
   user_data pointer is the one specified at registration time (see below). */

static void 
disconnected (gzochi_glib_client_session *session, void *user_data)
{
  abermud_context *context = user_data;
  shutdown (context, 0);
}

/* Updates the game state to allow characters to be typed in the input area. */

static void 
allow_input (abermud_context *context)
{
  context->allow_input = TRUE;
}

/* Updates the game state to either show or hide characters that are typed in
   the input area, depending on the boolean value FLAG. */

static void 
set_echo (abermud_context *context, int flag)
{
  context->echo = flag;
}

/* The `received_message' callback. Called when the client receives a packet
   MSG from the server. This implementation dispatches to a more specific 
   handler depending on the packet type, which is expected to be stored in the
   first byte of MSG. The user_data pointer is the one specified at
   registration time (see below) and is cast to the game context object. */

static void 
received_message
(gzochi_glib_client_session *session, unsigned char *msg, short len, 
 void *user_data)
{
  abermud_context *context = user_data;
  char *body = NULL;

  /* Packets from the server that include text content beyond the opcode in
     byte 0 may not be NULL-terminated; to treat them safely as C strings, we
     use strndup with the LEN argument. */

  if (len > 1)
    body = strndup ((char *) msg + 1, len - 1);

  switch (msg[0])
    {
    case ABERMUD_PACKET_TYPE_ECHO_ON: set_echo (context, TRUE); break;
    case ABERMUD_PACKET_TYPE_ECHO_OFF: set_echo (context, FALSE); break;
    case ABERMUD_PACKET_TYPE_INPUT_ON: allow_input (context); break;
    case ABERMUD_PACKET_TYPE_SHUTDOWN: shutdown (context, 0); break;

      /* Each of the following case blocks checks to see if body is NULL; the
	 server may occasionally send 0-length prompt, title, or output
	 packets. */

    case ABERMUD_PACKET_TYPE_SET_PROMPT: 
      if (body != NULL)
	abermud_console_set_prompt (context->console_state, body);
      break;

    case ABERMUD_PACKET_TYPE_OUTPUT:
      if (body != NULL)
	abermud_console_append_output (context->console_state, body);
      break;

    case ABERMUD_PACKET_TYPE_SET_TITLE:
      if (body != NULL)
	abermud_console_set_title (context->console_state, body);
      break;
    }

  /* Free any body content that has been allocated. */
  
  if (body != NULL)
    free (body);
}

/* The main function for the abermud game client application. Two command-line 
   arguments are required: the game server hostname and port. This function 
   parses the arguments, registers the `disconnected' and `received_message' 
   callbacks, establishes the connection to the server, and then enters the 
   server message listener loop. All abermud clients connect to the server as
   the `abermud' user; authentication is handled by the game's server 
   component. */

int 
main (int argc, char *argv[])
{
  /* Create the GLib main loop and main context. */
  
  GMainLoop *main_loop = g_main_loop_new (NULL, FALSE);
  GMainContext *main_context = g_main_loop_get_context (main_loop); 

  int port = 0;

  char *hostname = NULL;
  char *player = "abermud";

  gzochi_glib_client_session *session = NULL; /* The client session. */

  if (argc != 3)
    {
      fprintf (stderr, "%s <host> <port>\n", argv[0]);
      exit (1);
    }
  
  if (argc == 3)
    port = atoi (argv[2]);
  if (argc > 1)
    hostname = argv[1];
  
  /* Attempt to connect to the server. */

  session = gzochi_glib_client_connect 
    (hostname, port, "abermud", (unsigned char *) player, strlen (player));

  if (session != NULL)
    {
      /* If the connection is successful, initialize the game context and add 
	 the local player to it. */

      abermud_context *context = make_abermud_context ();
      context->session = session;

      /* Register the `disconnected' and `received_message' callbacks for the
	 new client session, passing the game context as "user data." */

      gzochi_glib_client_session_set_disconnected_callback 
	(session, disconnected, context);
      gzochi_glib_client_session_set_received_message_callback 
	(session, received_message, context);

      /* Attach the context session to the main context and start the GLib main
	 loop. This function will synchronously read and dispatch events to the
	 appropriate callbacks until the client is disconnected. */

      g_source_attach ((GSource *) gzochi_source_new (session), main_context);

      /* Plug some input handling into the main loop. */
      
      g_timeout_add (INPUT_POLL_INTERVAL_MS, input_timeout_function, context);

      g_main_loop_run (main_loop);
    }

  /* Otherwise log the failure and exit. */

  else printf ("Failed to connect to %s:%d/abermud.\n", hostname, port);
  return 0;
}
