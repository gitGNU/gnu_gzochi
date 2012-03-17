/* mazewar: A client-server implementation of mazewar for gzochi
 * mazewar.c: Main loop implementation for gzochi mazewar example game
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

#include <assert.h>
#include <glib.h>
#include <libgzochi.h>
#include <pthread.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "display.h"
#include "input.h"
#include "mazewar.h"
#include "message.h"

#define MAZE_HEIGHT 16
#define MAZE_WIDTH 32

/* Processes the input event INPUT. The original mazewar controls are
   supported, as well as a more modern set of key bindings. Each keystroke
   generates a message to be sent to the server. */

static void handle_event (mazewar_context *context, int input)
{
  short len = 0; /* The length of the outgoing message. */
  unsigned char *msg = NULL; /* The message bytes. */
  gzochi_client_session *session = context->session;

  switch (input)
    {
    case MAZEWAR_INPUT_KEY_S:
    case MAZEWAR_INPUT_KEY_LEFT: /* The player wants to turn left. */
      msg = mazewar_message_encode_turn_left (&len); break;
    case MAZEWAR_INPUT_KEY_F:
    case MAZEWAR_INPUT_KEY_RIGHT: /* The player wants to turn right. */
      msg = mazewar_message_encode_turn_right (&len); break;
    case MAZEWAR_INPUT_KEY_D:
    case MAZEWAR_INPUT_KEY_UP: /* The player wants to step forward. */
      msg = mazewar_message_encode_step (&len); break;
    case MAZEWAR_INPUT_KEY_RCTRL:
    case MAZEWAR_INPUT_KEY_LCTRL: /* The player wants to fire a missile. */
      msg = mazewar_message_encode_shoot (&len); break;
    case MAZEWAR_INPUT_EVENT_QUIT: /* The player wans to quit the game. */
      exit (0);
    default: break;
    }

  if (msg != NULL)
    {
      /* Send the generated message to the server. */

      gzochi_client_send (session, msg, len);
      free (msg);
    }
}

/* This is the primary input handling thread for the game. It initializes the
   input system and then loops forever, passing events to the handle_event 
   procedure. The context object is passed as the void pointer arg. */

static void *play (void *arg)
{
  mazewar_context *context = (mazewar_context *) arg;

  mazewar_input_init ();

  while (1)
    handle_event (context, mazewar_input_next_event ());

  return NULL;
}

/* The client `disconnected' callback. Called when the client is disconnected
   by the server, either explicitly or as the result of a network error. The
   user_data pointer is the one specified at registration time (see below),
   although it is ignored here. */

static void disconnected (gzochi_client_session *session, void *user_data)
{
  printf ("Disconnected.\n");
  exit (0);
}

/* A `GCompareFunc' implementation to be used for looking up `mazewar_player'
   objects in the global player list by player name: The name string is
   compared with the `player_name' field of the `mazewar_player' under
   comparison using `strcmp'. */

static gint player_name_compare (gconstpointer a, gconstpointer b)
{
  mazewar_player *player = (mazewar_player *) a; /* The player in the list. */
  char *player_name = (char *) b; /* The player name to look up. */

  return strcmp (player->name, player_name);
}

/* Retrieves a player by name from the mazewar global context CONTEXT. */

static mazewar_player *get_player 
(mazewar_context *context, char *player_name)
{
  /* Find the `GList' link using `player_name_compare' with the specified
     name PLAYER_NAME. */

  GList *player = g_list_find_custom 
    (context->players, player_name, player_name_compare);

  /* If the link is found, dereference it for its data; otherwise, return 
     NULL. */

  return player == NULL ? NULL : (mazewar_player *) player->data;
}

/* Return the global list of players connected to the mazewar context CONTEXT, 
   as a `GList' of `mazewar_player' objects. */

GList *mazewar_get_players (mazewar_context *context)
{
  return context->players;
}

/* Returns TRUE (non-zero) if the specified coordinates in the maze for the
   mazewar context CONTEXT is a wall tile, FALSE (zero) otherwise. */

gboolean mazewar_maze_is_wall (mazewar_context *context, int x, int y)
{
  int bit = y * MAZE_WIDTH + x; /* The bit within the maze bitmap. */
  unsigned char byte = context->maze[bit / 8]; /* The maze bitmap byte. */

  return !(byte & (1 << (7 - (bit % 8))));
}

/* Adds a new player structure to the mazewar game context CONTEXT with the
   specified characteristics, and returns a pointer to it. The new player will
   be created at the specified x-y coordinates and with the specified 
   orientation DIR and score SCORE. */ 

static mazewar_player *add_player 
(mazewar_context *context, char *player_name, int x, int y, int dir, 
 gboolean visible, int score)
{
  mazewar_player *player = malloc (sizeof (mazewar_player));

  player->name = strdup (player_name);

  player->x = x;
  player->y = y;
  player->dir = dir;

  player->visible = FALSE; /* Set the player to be invisible initially. */
  player->score = score;

  /* Add the new player to the player list. */
  
  context->players = g_list_append (context->players, player);
  return player;
}

/* Removes the player with the name PLAYER_NAME from the set of players in the
   game represented by the mazewar context CONTEXT. The memory used by the 
   player structure is freed. */

static void remove_player
(mazewar_context *context, char *player_name)
{
  mazewar_player *player = get_player (context, player_name);

  if (player != NULL)
    {
      context->players = g_list_remove (context->players, player);
      free (player);
    }
}

/* Called when a new player joins the game. Adds the player described by 
   JOIN_MSG to the game context CONTEXT and updates the display. */

static void player_joined 
(mazewar_context *context, mazewar_server_player_message *join_msg)
{
  /* Add the player to the game. */

  add_player (context, join_msg->player_name, 0, 0, 0, FALSE, 0); 

  mazewar_display_draw_scorecard (context); /* Update the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */
}

/* Called when an existing player leaves the game. Removes the player described
   by LEFT_MESSAGE from the game context CONTEXT and updates the display. */

static void player_left 
(mazewar_context *context, mazewar_server_player_message *left_message)
{
  /* Remove the player from the game. */

  remove_player (context, left_message->player_name);

  mazewar_display_draw_scorecard (context); /* Update the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */
}

/* Called when a player's position in the game changes. May be called for the
   client's associated player or a player that has just become visible to the
   client or a visible client that has moved to a new position. Updates the
   book-keeping for the player specified by POS_MSG in the game context CONTEXT 
   updates the display accordingly. */

static void update_position 
(mazewar_context *context, mazewar_server_player_position_message *pos_msg)
{
  mazewar_player *player = 
    get_player (context, ((mazewar_server_player_message *) pos_msg)
		->player_name);

  /* If the player was not previously hidden, then there must have been an
     arrow for them on the overhead map, so clear their previous location. */

  if (player->x != 0 && player->y != 0)
    mazewar_display_clear_arrow (context, player->x, player->y);
  
  player->x = pos_msg->x; /* Set the new x position. */
  player->y = pos_msg->y; /* Set the new y position. */
  player->visible = TRUE; /* Mark the player visibile until further notice. */

  /* Set the player's orientation. */

  switch (pos_msg->orientation)
    {
    case 0x0: player->dir = NORTH; break;
    case 0x1: player->dir = SOUTH; break;
    case 0x2: player->dir = EAST; break;
    case 0x3: player->dir = WEST;
    default: break;
    }

  mazewar_display_draw_view (context); /* Draw the 3-D view. */
  
  /* The client's player and the other players have different arrows, so figure
     out which one should be drawn on the overhead map at the new location. */
  
  if (player == context->self)
    mazewar_display_draw_arrow_self 
      (context, player->x, player->y, player->dir);
  else mazewar_display_draw_arrow_other
	 (context, player->x, player->y, player->dir);

  mazewar_display_draw_scorecard (context); /* Update the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */
}

/* Called when a player's score changes. Updates the score for the player
   specified by SCORE_MSG in the game context CONTEXT and updates the display
   accordingly. Score updates are "global" - they may be received for players
   that are currnently invisible to the client. */

static void update_score 
(mazewar_context *context, mazewar_server_player_score_message *score_msg)
{
  mazewar_player *player = 
    get_player (context, ((mazewar_server_player_message *) score_msg)
		->player_name);

  player->score = score_msg->score; /* Set the new score. */

  mazewar_display_draw_scorecard (context); /* Update the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */
}

/* Called when a previously-visible player becomes insvisible to the client,
   usually as a result of that player or the client's player moving out of 
   visual range. Updates the book-keeping for the player specified by 
   HIDDEN_MSG in the game context CONTEXT and updates the display 
   accordingly. */

static void hide_position 
(mazewar_context *context, mazewar_server_player_message *hidden_msg)
{
  mazewar_player *player = get_player (context, hidden_msg->player_name);

  /* Clear the last known location of the player on the overhead map. */

  if (player->x != 0 && player->y != 0)
    mazewar_display_clear_arrow (context, player->x, player->y);
  
  /* The assumption here is that the coordinates (0, 0) will never be an 
     accessible space on the map, and can thus serve as a signal value for
     "hidden." */

  player->x = 0;
  player->y = 0;

  /* Make the player invisible until further notice. */
  
  player->visible = FALSE; 

  mazewar_display_draw_view (context); /* Update the 3-D view. */
  mazewar_display_draw_scorecard (context); /* Update the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */
}

/* Completes the initializes the mazewar game context with the first message 
   from the server (MSG), which includes the maze dimensions, the maze bitmap
   data, and an initial list of player name and score information. */

static void bootstrap_maze 
(mazewar_context *context, unsigned char *msg, short len)
{
  pthread_t play_thread; /* The input event handler thread. */
  int i = 0, num_players = 0, maze_bytes_len = MAZE_WIDTH * MAZE_HEIGHT / 8;

  /* Allocate space for the maze bitmap. */

  unsigned char *maze_bytes = malloc (sizeof (unsigned char) * maze_bytes_len);
  
  /* The first two bytes of MSG give the maze dimensions, which are, for the
     moment, hard-coded. */

  assert (msg[0] == MAZE_WIDTH);
  assert (msg[1] == MAZE_HEIGHT);

  context->width = msg[0];
  context->height = msg[1];
  
  msg += 2;
  maze_bytes = memcpy (maze_bytes, msg, maze_bytes_len);
  context->maze = maze_bytes;
  msg += maze_bytes_len;

  /* Initialize the display context. */

  context->display_context = mazewar_display_init (context);

  num_players = msg[0]; /* The number of players is the next byte. */

  /* For each of NUM_PLAYERS, MSG will contain a length-prefixed sequence of 
     bytes giving the player's name, followed by four bytes giving the player's
     score in little-endian encoding. */

  msg += 1;
  for (; i < num_players; i++)
    {
      int player_name_len = msg[0];
      char *player_name = strndup ((char *) ++msg, player_name_len);
      int score = (msg[player_name_len] << 24)
	+ (msg[player_name_len + 1] << 16)
	+ (msg[player_name_len + 2] << 8)
	+ (msg[player_name_len + 3]);

      /* Add the new player to the list of players. */

      add_player (context, player_name, 0, 0, 0, FALSE, score);
      msg += player_name_len + 4;
    }

  mazewar_display_draw_maze (context); /* Draw the maze. */
  mazewar_display_draw_scorecard (context); /* Draw the scorecard. */
  mazewar_display_refresh (context); /* Refresh the view. */

  /* Launch the input handling thread, passing the game context as the user 
     data argument. */

  pthread_create (&play_thread, NULL, play, context);

  return;
}

/* The `received_message' callback. Called when the client receives a message
   MSG from the server. This implementation does a decoding pass on the message
   contents and then dispatches to a more specific handler depending on the
   decoded message type. The user_data pointer is the one specified at 
   registration time (see below) and is cast to the game context object. */

static void received_message
(gzochi_client_session *session, unsigned char *msg, short len, void *user_data)
{
  mazewar_context *context = (mazewar_context *) msg;
  mazewar_server_message *message = NULL;

  /* If the maze bitmap is NULL, this must be the first message received from
     the server, so assume that it is the state bootstrapping message and use
     it to bootstrap the game context. */

  if (context->maze == NULL)
    {
      bootstrap_maze (context, msg, len);
      return;
    }
  
  /* Otherwise, decode the message bytes and dispatch. */
  
  message = mazewar_message_decode (msg, len);
  switch (message->type)
    {
    case MAZEWAR_MESSAGE_TYPE_PLAYER_JOINED: /* A player joined the game. */ 
      player_joined (context, (mazewar_server_player_message *) message); break;
    case MAZEWAR_MESSAGE_TYPE_PLAYER_LEFT: /* A player left the game. */
      player_left (context, (mazewar_server_player_message *) message); break;
    case MAZEWAR_MESSAGE_TYPE_PLAYER_SCORE: /* A player's score changed. */
      update_score (context, (mazewar_server_player_score_message *) message); 
      break;
    case MAZEWAR_MESSAGE_TYPE_PLAYER_POSITION: 
      
      /* A visible player's position changed. */
      
      update_position 
	(context, (mazewar_server_player_position_message *) message); 
      break;
    case MAZEWAR_MESSAGE_TYPE_PLAYER_HIDDEN:

      /* A player became invisible. */

      hide_position (context, (mazewar_server_player_message *) message); 
    default: break;
    }
}

/* Allocates and returns a new, empty mazewar game context object. */

static mazewar_context *make_mazewar_context ()
{
  return calloc (1, sizeof (mazewar_context));
}

/* The main function for the mazewar game client application. Three 
   comamnd-line arguments are required, providing, respectively, the game 
   server hostname, game server port, and client username. This function parses
   the argument, registers the `disconnected' and `received_message' callbacks,
   establishes the connection to the server, and then enters the server message
   listener loop. */

int main (int argc, char *argv[])
{
  gzochi_client_session *session = NULL; /* The client session. */
  char *hostname = NULL, *player = NULL; /* The hostname and player name. */
  int port = 0; /* The port number. */

  assert (argc == 4);

  hostname = argv[1];
  port = atoi (argv[2]); /* Parse the port number. */
  player = argv[3];

  /* Attempt to connect to the server. */

  session = gzochi_client_connect 
    (argv[1], port, "mazewar", (unsigned char *) player, strlen (player));

  if (session != NULL)
    {
      /* If the connection is successful, initialize the game context and add 
	 the local player to it. */

      mazewar_context *context = make_mazewar_context ();

      context->self = add_player (context, player, 0, 0, 0, TRUE, 0);
      context->session = session;

      /* Register the `disconnected' and `received_message' callbacks for the
	 new client session, passing the game context as "user data." */

      gzochi_client_session_set_disconnected_callback 
	(session, disconnected, context);
      gzochi_client_session_set_received_message_callback 
	(session, received_message, context);

      /* Start the gzochi client event loop. This function will synchronously 
	 read and dispatch events to the appropriate callbacks until the client
	 is disconnected. */

      gzochi_client_run (session);
    }

  /* Otherwise log the failure and exit. */

  else printf ("Failed to connect to %s:%d/mazewar.\n", hostname, port);

  return 0;
}
