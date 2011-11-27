/* session.h: Prototypes and declarations for session.c
 * Copyright (C) 2011 Julian Graham
 *
 * gzochi is free software: you can redistribute it and/or modify it
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

#ifndef GZOCHI_SESSION_H
#define GZOCHI_SESSION_H

#define GZOCHI_CLIENT_MAX_BUFFER_SIZE 65538

typedef struct _gzochi_client_channel 
{
  char *name;
  struct _gzochi_client_session *session;

  int connected;
  
  void (*disconnected_callback) (struct _gzochi_client_channel *);
  void (*received_message_callback)
  (struct _gzochi_client_channel *, unsigned char *, short);
} gzochi_client_channel;

typedef struct _gzochi_client_session
{
  char *hostname;
  char *endpoint;
  int port;

  int connected;

  int socket;
  unsigned char buffer[GZOCHI_CLIENT_MAX_BUFFER_SIZE];
  int buffer_length;
  
  gzochi_client_channel **channels;
  int channels_length;

  void (*disconnected_callback) (struct _gzochi_client_session *);
  void (*received_message_callback)
  (struct _gzochi_client_session *, unsigned char *, short);
  void (*joined_channel_callback) (struct _gzochi_client_channel *);
} gzochi_client_session;

gzochi_client_session *gzochi_client_session_new (void);
void gzochi_client_session_free (gzochi_client_session *);
void gzochi_client_session_disconnect (gzochi_client_session *);

#endif /* GZOCHI_SESSION_H */
