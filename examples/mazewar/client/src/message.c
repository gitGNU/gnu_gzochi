/* message.c: Message serialization functions for gzochi mazewar example game
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

#include <stdlib.h>
#include <string.h>

#include "message.h"

/* Decodes the payload of a "player joined" message from the server. The format
   of this message is: One byte giving the opcode; followed by LEN - 1 bytes
   encoding the name of the joining player. */

static mazewar_server_player_message *decode_player_joined 
(unsigned char *msg, short len)
{
  mazewar_server_player_message *player_message = 
    malloc (sizeof (mazewar_server_player_message));

  ((mazewar_server_message *) player_message)->type = 
    MAZEWAR_MESSAGE_TYPE_PLAYER_JOINED;

  /* Take the remainder of the message as the player's name. */

  player_message->player_name = strndup ((char *) msg + 1, len - 1);

  return player_message;
}

/* Decodes the payload of a "player left" message from the server. The format
   of this message is: One byte giving the opcode; followed by LEN - 1 bytes
   encoding the name of the leaving player. */

static mazewar_server_player_message *decode_player_left
(unsigned char *msg, short len)
{
  mazewar_server_player_message *player_message = 
    malloc (sizeof (mazewar_server_player_message));

  ((mazewar_server_message *) player_message)->type = 
    MAZEWAR_MESSAGE_TYPE_PLAYER_LEFT;

  /* Take the remainder of the message as the player's name. */

  player_message->player_name = strndup ((char *) msg + 1, len - 1);

  return player_message;
}

/* Decodes the payload of a "player hidden" message from the server. The format
   of this message is: One byte giving the opcode; followed by LEN - 1 bytes
   encoding the name of the player no longer visible. */

static mazewar_server_player_message *decode_player_hidden
(unsigned char *msg, short len)
{
  mazewar_server_player_message *player_message = 
    malloc (sizeof (mazewar_server_player_message));

  ((mazewar_server_message *) player_message)->type = 
    MAZEWAR_MESSAGE_TYPE_PLAYER_HIDDEN;

  /* Take the remainder of the message as the player's name. */

  player_message->player_name = strndup ((char *) msg + 1, len - 1);

  return player_message;
}

/* Decodes the payload of a "player score" message from the server. The format
   of this message is: One byte giving the opcode; followed by one byte giving
   the number of bytes (n) in the name of the player whose score changed; 
   followed by n bytes encoding the player's name; followed by four bytes
   giving the little-endian encoding of the player's new score. */

static mazewar_server_player_score_message *decode_player_score
(unsigned char *msg, short len)
{
  mazewar_server_player_score_message *player_message = 
    malloc (sizeof (mazewar_server_player_score_message));
  unsigned char player_name_len = msg[1];

  ((mazewar_server_message *) player_message)->type = 
    MAZEWAR_MESSAGE_TYPE_PLAYER_SCORE;

  /* Take the next PLAYER_NAME_LEN bytes as the player's name. */

  ((mazewar_server_player_message *) player_message)->player_name = 
    strndup ((char *) msg + 2, player_name_len);

  /* Shift each subsequent byte into the right position within the target
     four-byte integer value, least significant bit first. */

  player_message->score = msg[player_name_len + 2]
    + (msg[player_name_len + 3] << 8)
    + (msg[player_name_len + 4] << 16)
    + (msg[player_name_len + 5] << 24);

  return player_message;
}

/* Decodes the payload of a "player position" message from the server. The 
   format of this message is: One byte giving the opcode; followed by one byte
   giving the number of bytes (n) in the name of the player whose position 
   changed; followed by n bytes encoding the player's name; followed by one
   byte each giving, respectively, the x position, y position, and orientation
   of the player within the maze. */

static mazewar_server_player_position_message *decode_player_position
(unsigned char *msg, short len)
{
  mazewar_server_player_position_message *player_message = 
    malloc (sizeof (mazewar_server_player_position_message));
  unsigned char player_name_len = msg[1];

  ((mazewar_server_message *) player_message)->type = 
    MAZEWAR_MESSAGE_TYPE_PLAYER_POSITION;

  /* Take the next PLAYER_NAME_LEN bytes as the player's name. */

  ((mazewar_server_player_message *) player_message)->player_name = 
    strndup ((char *) msg + 2, player_name_len);

  player_message->x = msg[2 + player_name_len]; /* Set the x position. */
  player_message->y = msg[3 + player_name_len]; /* Set the y position. */

  /* Set the orientation. */

  player_message->orientation = msg[4 + player_name_len];

  return player_message;
}

/* The main message decoding dispatch function. Reads the first byte addressed
   by MSG and dispatches to a more specific decoder function. */

mazewar_server_message *mazewar_message_decode (unsigned char *msg, short len)
{
  switch (msg[0])
    {
    case MAZEWAR_MESSAGE_TYPE_PLAYER_JOINED: 
      return (mazewar_server_message *) decode_player_joined (msg, len);
    case MAZEWAR_MESSAGE_TYPE_PLAYER_LEFT:
      return (mazewar_server_message *) decode_player_left (msg, len);
    case MAZEWAR_MESSAGE_TYPE_PLAYER_SCORE:
      return (mazewar_server_message *) decode_player_score (msg, len);
    case MAZEWAR_MESSAGE_TYPE_PLAYER_POSITION:
      return (mazewar_server_message *) decode_player_position (msg, len);
    case MAZEWAR_MESSAGE_TYPE_PLAYER_HIDDEN:
      return (mazewar_server_message *) decode_player_hidden (msg, len);
    default:
      return NULL;
    }
}

/* Frees the memory used by the server message MESSAGE, performing some 
   type-specific freeing as necessary. */

void mazewar_message_free (mazewar_server_message *message)
{
  switch (message->type)
    {
    case MAZEWAR_MESSAGE_TYPE_PLAYER_JOINED: 
    case MAZEWAR_MESSAGE_TYPE_PLAYER_LEFT:
    case MAZEWAR_MESSAGE_TYPE_PLAYER_SCORE:
    case MAZEWAR_MESSAGE_TYPE_PLAYER_POSITION:
    case MAZEWAR_MESSAGE_TYPE_PLAYER_HIDDEN:

      /* All of these message types include a player name string, so free that
	 data explicity before freeing the outer message structure. */

      free (((mazewar_server_player_message *) message)->player_name);
    default: break;
    }

  free (message); /* Free the message itself. */
}

/* Returns a byte encoding of a "turn left" message suitable for sending to the
   server. If the short pointer LEN is non-NULL, its addressable memory will be
   set to the number of bytes in the encoded message. */

unsigned char *mazewar_message_encode_turn_left (short *len)
{
  unsigned char *msg = malloc (sizeof (unsigned char));
  
  *msg = MAZEWAR_MESSAGE_TYPE_CLIENT_TURN_LEFT;

  if (len != NULL)
    *len = 1;

  return msg;
}

/* Returns a byte encoding of a "turn right" message suitable for sending to 
   the server. If the short pointer LEN is non-NULL, its addressable memory 
   will be set to the number of bytes in the encoded message. */

unsigned char *mazewar_message_encode_turn_right (short *len)
{
  unsigned char *msg = malloc (sizeof (unsigned char));
  
  *msg = MAZEWAR_MESSAGE_TYPE_CLIENT_TURN_RIGHT;

  if (len != NULL)
    *len = 1;

  return msg;
}

/* Returns a byte encoding of a "step forward" message suitable for sending to 
   the server. If the short pointer LEN is non-NULL, its addressable memory 
   will be set to the number of bytes in the encoded message. */

unsigned char *mazewar_message_encode_step (short *len)
{
  unsigned char *msg = malloc (sizeof (unsigned char));
  
  *msg = MAZEWAR_MESSAGE_TYPE_CLIENT_STEP;

  if (len != NULL)
    *len = 1;

  return msg;
}

/* Returns a byte encoding of a "shoot" message suitable for sending to the
   server. If the short pointer LEN is non-NULL, its addressable memory will be
   set to the number of bytes in the encoded message. */

unsigned char *mazewar_message_encode_shoot (short *len)
{
  unsigned char *msg = malloc (sizeof (unsigned char));
  
  *msg = MAZEWAR_MESSAGE_TYPE_CLIENT_SHOOT;

  if (len != NULL)
    *len = 1;

  return msg;
}
