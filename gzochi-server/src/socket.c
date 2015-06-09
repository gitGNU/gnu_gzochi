/* socket.c: Application socket server implementation for gzochid
 * Copyright (C) 2015 Julian Graham
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

#include <glib.h>
#include <gzochi-common.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "context.h"
#include "fsm.h"
#include "game.h"
#include "log.h"
#include "protocol.h"
#include "socket.h"
#include "threads.h"

struct _gzochid_client_socket
{
  gzochid_socket_server_context *context;
  gzochid_protocol_client *client;

  GMutex sock_mutex;

  GArray *send_buffer;
  GArray *recv_buffer;
  
  GIOChannel *channel;
  char *connection_description;
  GSource *write_source;
};

gzochid_socket_server_context *
gzochid_socket_server_context_new (void)
{
  gzochid_socket_server_context *context = calloc 
    (1, sizeof (gzochid_socket_server_context));

  context->main_context = g_main_context_new ();
  context->main_loop = g_main_loop_new (context->main_context, FALSE);

  return context;  
}

void 
gzochid_socket_server_context_free (gzochid_socket_server_context *context)
{
  gzochid_context_free ((gzochid_context *) context);
}

static gboolean
dispatch_client_write (GIOChannel *channel, GIOCondition cond, gpointer data)
{
  GIOStatus status;
  gzochid_client_socket *sock = data;

  g_mutex_lock (&sock->sock_mutex);

  do
    {
      gsize written = 0;
      GError *error = NULL;
      
      status = g_io_channel_write_chars 
	(sock->channel, sock->send_buffer->data, sock->send_buffer->len, 
	 &written, &error);

      if (written > 0)
	g_array_remove_range (sock->send_buffer, 0, written);

      if (error != NULL)
	{
	  g_error_free (error);
	  return FALSE;
	}
    }
  while (status == G_IO_STATUS_NORMAL && sock->send_buffer->len > 0);

  if (sock->send_buffer->len == 0)
    {
      g_source_destroy (sock->write_source);
      sock->write_source = NULL;
    }

  g_mutex_unlock (&sock->sock_mutex);
  return TRUE;
}

static gboolean
dispatch_client_error (gzochid_client_socket *sock)
{
  gzochid_debug ("Socket disconnected.");
  gzochid_protocol_client_disconnected (sock->client);
  gzochid_protocol_client_free (sock->client);

  return FALSE;
}

static gboolean
fill_recv_buffer (gzochid_client_socket *sock)
{
  GIOStatus status;

  do
    {
      gchar buf[1024];
      gsize bytes_read = 0;
      GError *error = NULL;

      status = g_io_channel_read_chars 
	(sock->channel, buf, 1024, &bytes_read, &error);

      if (bytes_read > 0)
	g_array_append_vals (sock->recv_buffer, buf, bytes_read);

      if (error != NULL)
	{
	  g_error_free (error);
	  return FALSE;
	}
    }
  while (status == G_IO_STATUS_NORMAL);
  return status == G_IO_STATUS_EOF ? dispatch_client_error (sock) : TRUE;
}

static gboolean
dispatch_recv_buffer (gzochid_client_socket *sock)
{
  int offset = 0, total = 0;
  int remaining = sock->recv_buffer->len;

  while (remaining >= 3)
    {
      short len = gzochi_common_io_read_short 
	((unsigned char *) sock->recv_buffer->data, offset);
      
      if (++len > remaining - 2)
	break;
      
      offset += 2;
      
      gzochid_protocol_client_dispatch 
	(sock->client, (unsigned char *) sock->recv_buffer->data + offset, len);
      
      offset += len;
      remaining -= len + 2;
      total += len + 2;
    }
  
  if (total > 0)
    g_array_remove_range (sock->recv_buffer, 0, total);

  return TRUE;
}

static gboolean
dispatch_client_read (gzochid_client_socket *sock)
{
  return fill_recv_buffer (sock) && dispatch_recv_buffer (sock);
}

static gboolean
dispatch_client (GIOChannel *channel, GIOCondition condition, gpointer data)
{
  gzochid_client_socket *sock = data;

  switch (condition)
    {
    case G_IO_IN:
    case G_IO_PRI: return dispatch_client_read (sock);
    case G_IO_ERR:
    case G_IO_HUP: return dispatch_client_error (sock);
    default: return FALSE;
    }
}

static gzochid_client_socket *
create_client_socket (GIOChannel *channel)
{
  gzochid_client_socket *sock = malloc (sizeof (gzochid_client_socket));

  sock->channel = channel;
  sock->write_source = NULL;

  g_mutex_init (&sock->sock_mutex);
  sock->recv_buffer = g_array_new (FALSE, FALSE, sizeof (unsigned char));
  sock->send_buffer = g_array_new (FALSE, FALSE, sizeof (unsigned char));

  return sock;
}

static gboolean
dispatch_accept (GIOChannel *channel, GIOCondition cond, gpointer data)
{
  gzochid_socket_server_context *server_context = data;
  gint client_fd, server_fd = g_io_channel_unix_get_fd (channel);
  struct sockaddr_in client_addr;
  socklen_t client_addr_len = sizeof (struct sockaddr_in);
  int one = 1;

  while ((client_fd = accept
	  (server_fd, (struct sockaddr *) &client_addr, &client_addr_len)) >= 0)
    {
      GIOChannel *channel = g_io_channel_unix_new (client_fd);
      gzochid_client_socket *sock = create_client_socket (channel);
      gzochid_protocol_client *client =
	gzochid_protocol_client_accept (sock);
      GSource *client_read_source = g_io_create_watch 
	(channel, G_IO_IN | G_IO_PRI | G_IO_ERR | G_IO_HUP);

      setsockopt (client_fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof (int));

      g_io_channel_set_encoding (channel, NULL, NULL);
      g_io_channel_set_flags (channel, G_IO_FLAG_NONBLOCK, NULL);
      g_io_channel_set_buffered (channel, FALSE);

      sock->client = client;
      sock->connection_description = g_strdup_printf 
	("%d.%d.%d.%d:%d",
	 client_addr.sin_addr.s_addr & 0xff,
	 (client_addr.sin_addr.s_addr & 0xff00) >> 8,
	 (client_addr.sin_addr.s_addr & 0xff0000) >> 16,
	 (client_addr.sin_addr.s_addr & 0xff000000) >> 24,
	 client_addr.sin_port);

      sock->context = server_context;
      
      g_source_set_callback
	(client_read_source, (GSourceFunc) dispatch_client, sock, NULL);
      g_source_attach (client_read_source, server_context->main_context);
    }

  return TRUE;
}

static void 
initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_socket_server_context *server_context = 
    (gzochid_socket_server_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gint fd = socket (PF_INET, SOCK_STREAM, 0);
  GIOChannel *server_sock = g_io_channel_unix_new (fd);
  GSource *server_source = g_io_create_watch (server_sock, G_IO_IN);
  struct sockaddr_in addr;
  int one = 1;

  addr.sin_family = AF_INET;
  addr.sin_port = htons (game_context->port);
  addr.sin_addr.s_addr = htonl (INADDR_ANY);

  setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof (int));
  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof (int));

  g_io_channel_set_flags (server_sock, G_IO_FLAG_NONBLOCK, NULL);

  if (bind (fd, (struct sockaddr *) &addr, sizeof (struct sockaddr_in)) != 0)
    gzochid_err ("Failed to bind server to port %d", game_context->port);
  if (listen (fd, 128) != 0)
    gzochid_err ("Failed to listen on port %d", game_context->port);
  else
    {
      /* To enable the server to bind to an arbitrary port - i.e., to allow
	 game_context->port to be zero - we need a way to figure out which port
	 was actually selected by the network subsystem. Call `getsockname' on 
	 the server socket after a successful `listen' to figure this out. */
      
      struct sockaddr_in *bound_addr = NULL;
      
      server_context->addr = malloc (sizeof (struct sockaddr_in));
      server_context->addrlen = sizeof (struct sockaddr_in);

      bound_addr = (struct sockaddr_in *) server_context->addr;

      if (getsockname (fd, server_context->addr, &server_context->addrlen) != 0)
	gzochid_err ("Failed to get name of bound socket");
      else gzochid_notice
	     ("Game server listening on port %d", ntohs (bound_addr->sin_port));
    }

  g_source_set_callback 
    (server_source, (GSourceFunc) dispatch_accept, server_context, NULL);
  g_source_attach (server_source, server_context->main_context);
  
  gzochid_fsm_to_state (context->fsm, GZOCHID_SOCKET_SERVER_STATE_RUNNING);
}

static gpointer 
run_async (gpointer data)
{
  gzochid_socket_server_context *server = data;
  g_main_loop_run (server->main_loop);
  return NULL;
}

static void 
run (int from_state, int to_state, gpointer user_data)
{
  g_thread_new ("socket-server", run_async, user_data);
}

static void 
stop (int from_state, int to_state, gpointer user_data)
{
  gzochid_socket_server_context *server = user_data;
  g_main_loop_quit (server->main_loop);
}

void 
gzochid_socket_server_context_init 
(gzochid_socket_server_context *context, gzochid_context *parent, int port)
{
  gzochid_fsm *fsm = gzochid_fsm_new
    ("socket-server", GZOCHID_SOCKET_SERVER_STATE_INITIALIZING, "INITIALIZING");
  
  gzochid_fsm_add_state (fsm, GZOCHID_SOCKET_SERVER_STATE_RUNNING, "RUNNING");
  gzochid_fsm_add_state (fsm, GZOCHID_SOCKET_SERVER_STATE_STOPPED, "STOPPED");
  
  gzochid_fsm_add_transition
    (fsm, GZOCHID_SOCKET_SERVER_STATE_INITIALIZING,
     GZOCHID_SOCKET_SERVER_STATE_RUNNING);
  gzochid_fsm_add_transition
    (fsm, GZOCHID_SOCKET_SERVER_STATE_RUNNING,
     GZOCHID_SOCKET_SERVER_STATE_STOPPED);

  gzochid_fsm_on_enter
    (fsm, GZOCHID_SOCKET_SERVER_STATE_INITIALIZING, initialize, context);
  gzochid_fsm_on_enter (fsm, GZOCHID_SOCKET_SERVER_STATE_RUNNING, run, context);
  gzochid_fsm_on_enter
    (fsm, GZOCHID_SOCKET_SERVER_STATE_STOPPED, stop, context);

  gzochid_context_init ((gzochid_context *) context, parent, fsm);
}

gzochid_socket_server_context *
gzochid_socket_get_server_context (gzochid_client_socket *sock)
{
  return sock->context;
}

char *
gzochid_socket_get_connection_description (gzochid_client_socket *sock)
{
  return sock->connection_description;
}

void
gzochid_client_socket_write 
(gzochid_client_socket *sock, unsigned char *data, size_t len)
{
  g_mutex_lock (&sock->sock_mutex);
  g_array_append_vals (sock->send_buffer, data, len);
  if (sock->write_source == NULL)
    {
      sock->write_source = g_io_create_watch (sock->channel, G_IO_OUT);
      g_source_set_callback
	(sock->write_source, (GSourceFunc) dispatch_client_write, sock, NULL);
      g_source_attach (sock->write_source, sock->context->main_context);
    }
  g_mutex_unlock (&sock->sock_mutex);
}

void
gzochid_client_socket_free (gzochid_client_socket *sock)
{
  g_io_channel_unref (sock->channel);
  g_mutex_clear (&sock->sock_mutex);
  g_array_unref (sock->recv_buffer);
  g_array_unref (sock->send_buffer);
  g_free (sock->connection_description);
  free (sock);
}
