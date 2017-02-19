/* session.h: Prototypes and declarations for session.c
 * Copyright (C) 2017 Julian Graham
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

#ifndef LIBGZOCHI_GLIB_SESSION_H
#define LIBGZOCHI_GLIB_SESSION_H

#define GZOCHI_CLIENT_MAX_BUFFER_SIZE 65538

struct _gzochi_client_common_session
{
  char *hostname;
  char *endpoint;
  int port;

  int connected;
  int disconnect_acknowledged;

  int socket;
  unsigned char buffer[GZOCHI_CLIENT_MAX_BUFFER_SIZE];
  int buffer_length;
  
  void (*disconnected_callback) (struct _gzochi_client_common_session *, 
				 void *);
  void *disconnected_user_data;
  void (*received_message_callback) (struct _gzochi_client_common_session *, 
				     unsigned char *, unsigned short, void *);
  void *received_message_user_data;
};

typedef struct _gzochi_client_common_session gzochi_client_common_session;

gzochi_client_common_session *gzochi_client_common_session_new (void);
void gzochi_client_common_session_free (gzochi_client_common_session *);
void gzochi_client_common_session_disconnect (gzochi_client_common_session *);

char *gzochi_client_common_session_endpoint (gzochi_client_common_session *);
char *gzochi_client_common_session_hostname (gzochi_client_common_session *);
int gzochi_client_common_session_port (gzochi_client_common_session *);

int gzochi_client_common_session_is_dispatchable 
(gzochi_client_common_session *);

typedef void (*gzochi_client_common_session_disconnected_callback) 
(gzochi_client_common_session *, void *);

typedef void (*gzochi_client_common_session_received_message_callback) 
(gzochi_client_common_session *, unsigned char *, unsigned short, void *);

void gzochi_client_common_session_set_disconnected_callback
(gzochi_client_common_session *,
 gzochi_client_common_session_disconnected_callback,
 void *);

void gzochi_client_common_session_set_received_message_callback
(gzochi_client_common_session *,
 gzochi_client_common_session_received_message_callback,
 void *);

#endif /* LIBGZOCHI_GLIB_SESSION_H */
