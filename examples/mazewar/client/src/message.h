/* message.h: Prototypes and declarations for message.c
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

#ifndef MAZEWAR_MESSAGE_H
#define MAZEWAR_MESSAGE_H

/* Opcode constants for messages originating on the server. */

#define MAZEWAR_MESSAGE_TYPE_PLAYER_JOINED 0x01 /* A new player joined. */
#define MAZEWAR_MESSAGE_TYPE_PLAYER_LEFT 0x02 /* An existing player left. */
#define MAZEWAR_MESSAGE_TYPE_PLAYER_SCORE 0x03 /* A player's score changed. */
/* A player became visible or a visible player's position changed. */
#define MAZEWAR_MESSAGE_TYPE_PLAYER_POSITION 0x04 
/* A visible player became invisible. */
#define MAZEWAR_MESSAGE_TYPE_PLAYER_HIDDEN 0x05 

/* Opcode constants for messages originating on the client. */

/* The local player turned left. */
#define MAZEWAR_MESSAGE_TYPE_CLIENT_TURN_LEFT 0x01
/* The local player turned right. */
#define MAZEWAR_MESSAGE_TYPE_CLIENT_TURN_RIGHT 0x02
/* The local player stepped forward. */
#define MAZEWAR_MESSAGE_TYPE_CLIENT_STEP 0x03
/* The local player fired a missile. */
#define MAZEWAR_MESSAGE_TYPE_CLIENT_SHOOT 0x04

/* The "base" type for messages sent by the server. */

typedef struct _mazewar_server_message
{
  int type; /* The message opcode. */
} mazewar_server_message;

/* The "base" type for messages sent by the server about a player. */

typedef struct _mazewar_server_player_message
{
  mazewar_server_message base; /* The "parent" message data. */
  char *player_name; /* The player's name. */
} mazewar_server_player_message;

/* The position message type. */

typedef struct _mazewar_server_player_position_message
{
  mazewar_server_player_message base; /* The "parent" message data. */

  int x; /* The player's new x coordinate. */
  int y; /* The player's new y coordinate. */
  int orientation; /* The player's new orientation. */
} mazewar_server_player_position_message;

/* The score message type. */

typedef struct _mazewar_server_player_score_message
{
  mazewar_server_player_message base; /* The "parent" message data. */

  int score; /* The player's new score. */
} mazewar_server_player_score_message;

/* Decodes the specified bytes (of specified length LEN) into a 
   mazewar_server_message. */

mazewar_server_message *mazewar_message_decode (unsigned char *, short);

/* Returns a byte encoding of a "turn left" message suitable for sending to the
   server. If the short pointer LEN is non-NULL, its addressable memory will be
   set to the number of bytes in the encoded message. */
unsigned char *mazewar_message_encode_turn_left (short *);
/* Returns a byte encoding of a "turn right" message suitable for sending to 
   the server. If the short pointer LEN is non-NULL, its addressable memory 
   will be set to the number of bytes in the encoded message. */
unsigned char *mazewar_message_encode_turn_right (short *);
/* Returns a byte encoding of a "step forward" message suitable for sending to 
   the server. If the short pointer LEN is non-NULL, its addressable memory 
   will be set to the number of bytes in the encoded message. */
unsigned char *mazewar_message_encode_step (short *);
/* Returns a byte encoding of a "shoot" message suitable for sending to the
   server. If the short pointer LEN is non-NULL, its addressable memory will be
   set to the number of bytes in the encoded message. */
unsigned char *mazewar_message_encode_shoot (short *);

#endif /* MAZEWAR_MESSAGE_H */
