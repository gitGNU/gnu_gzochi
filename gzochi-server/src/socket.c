/* socket.c: Application socket server implementation for gzochid
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

#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "protocol.h"
#include "socket.h"

G_DEFINE_TYPE (GzochidSocketServer, gzochid_socket_server, G_TYPE_OBJECT);

/* Lifecycle functions for the `GzochidSocketServer' object. */

static void
gzochid_socket_server_finalize (GObject *gobject)
{
  GzochidSocketServer *server = GZOCHID_SOCKET_SERVER (gobject);

  g_main_context_unref (server->main_context);
  g_main_loop_unref (server->main_loop);

  G_OBJECT_CLASS (gzochid_socket_server_parent_class)->finalize (gobject);
}

static void
gzochid_socket_server_class_init (GzochidSocketServerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->finalize = gzochid_socket_server_finalize;
}

static void
gzochid_socket_server_init (GzochidSocketServer *self)
{
  self->main_context = g_main_context_new ();
  self->main_loop = g_main_loop_new (self->main_context, FALSE);
}

struct _gzochid_server_socket
{
  GzochidSocketServer *server; /* A ref to the socket server. */

  char *name; /* The server socket name, for logging. */
  GIOChannel *channel; /* The server socket IO channel. */
  struct sockaddr *addr; /* The actual address of the server socket. */
  socklen_t addrlen; /* The length of `addr'. */

  gzochid_server_protocol protocol; /* The server protocol. */
  gpointer protocol_data; /* Callback data for the server protocol. */
};

struct _gzochid_client_socket
{
  GzochidSocketServer *server; /* A ref to the socket server. */
  
  GMutex sock_mutex; /* A mutex to synchronize access to the send buffer. */

  GByteArray *send_buffer; /* The outgoing data buffer. */
  GByteArray *recv_buffer; /* The incoming data buffer. */
  
  GIOChannel *channel; /* The client socket IO channel. */
  char *connection_description; /* Description of the connection, for logs. */
  GSource *read_source; /* The client's read source. */
  GSource *write_source; /* The client's write source. */

  gzochid_client_protocol protocol; /* The client protocol. */
  gpointer protocol_data; /* Callback data for the client protocol. */

  gint ref_count; /* The socket's reference count. */
};

gzochid_server_socket *
gzochid_server_socket_new (char *name, gzochid_server_protocol protocol,
			   gpointer protocol_data)
{
  gzochid_server_socket *server_socket =
    calloc (1, sizeof (gzochid_server_socket));

  server_socket->name = strdup (name);
  server_socket->protocol = protocol;
  server_socket->protocol_data = protocol_data;

  return server_socket;
}

static gboolean
dispatch_client_write (GIOChannel *channel, GIOCondition cond, gpointer data)
{
  GIOStatus status;
  gzochid_client_socket *sock = data;

  /* It's possible that a dangling write may be dispatched to a destroyed 
     source, which may already have been finalized. So check the destroyed
     status before doing anything else. */
  
  if (g_source_is_destroyed (g_main_current_source ()))
    return FALSE;
  
  g_mutex_lock (&sock->sock_mutex);

  do
    {
      gsize written = 0;
      GError *error = NULL;
      
      status = g_io_channel_write_chars 
	(sock->channel, (const gchar *) sock->send_buffer->data,
	 sock->send_buffer->len, &written, &error);

      if (written > 0)
	g_byte_array_remove_range (sock->send_buffer, 0, written);

      if (error != NULL)
	{
	  g_error_free (error);
	  g_mutex_unlock (&sock->sock_mutex);
	  return FALSE;
	}
    }
  while (status == G_IO_STATUS_NORMAL && sock->send_buffer->len > 0);

  if (sock->send_buffer->len == 0)
    {
      g_source_destroy (sock->write_source);
      g_source_unref (sock->write_source);

      sock->write_source = NULL;
    }

  g_mutex_unlock (&sock->sock_mutex);
  return TRUE;
}

static gboolean
dispatch_client_error (gzochid_client_socket *sock)
{
  g_debug ("Socket %s disconnected.",
	   gzochid_client_socket_get_connection_description (sock));
  sock->protocol.error (sock->protocol_data);
  return FALSE;
}

static gboolean
fill_recv_buffer (gzochid_client_socket *sock)
{
  GIOStatus status;

  do
    {
      guint8 buf[1024];
      gsize bytes_read = 0;
      GError *error = NULL;

      status = g_io_channel_read_chars 
	(sock->channel, (gchar *) buf, 1024, &bytes_read, &error);

      if (bytes_read > 0)
	g_byte_array_append (sock->recv_buffer, buf, bytes_read);

      if (error != NULL)
	{
	  g_warning ("Encountered error while reading from socket: %s",
		     error->message);
	  g_error_free (error);
	  return FALSE;
	}
    }
  while (status == G_IO_STATUS_NORMAL);
  return status == G_IO_STATUS_EOF ? dispatch_client_error (sock) : TRUE;
}

static gboolean
dispatch_client_read (gzochid_client_socket *sock)
{
  if (fill_recv_buffer (sock))
    {      
      while (sock->recv_buffer->len > 0 && sock->protocol.can_dispatch
	     (sock->recv_buffer, sock->protocol_data))
	{
	  unsigned int num = sock->protocol.dispatch
	    (sock->recv_buffer, sock->protocol_data);

	  if (num > 0)
	    sock->recv_buffer = g_byte_array_remove_range
	      (sock->recv_buffer, 0, num);
	  else break;
	}
      return TRUE;
    }
  else return FALSE;
}

static gboolean
dispatch_client (GIOChannel *channel, GIOCondition condition, gpointer data)
{
  gzochid_client_socket *sock = data;

  /* It's possible that a dangling read or error may be dispatched to a 
     destroyed source, which may already have been finalized. So check the 
     destroyed status before doing anything else. */
  
  if (g_source_is_destroyed (g_main_current_source ()))
    return FALSE;

  switch (condition)
    {
    case G_IO_IN:
    case G_IO_PRI: return dispatch_client_read (sock);
    case G_IO_ERR:
    case G_IO_HUP: return dispatch_client_error (sock);
    default: return FALSE;
    }
}

gzochid_client_socket *
gzochid_client_socket_new (GIOChannel *channel,
			   const char *connection_description,
			   gzochid_client_protocol protocol,
			   gpointer protocol_data)
{
  gzochid_client_socket *sock = malloc (sizeof (gzochid_client_socket));

  sock->server = NULL;
  sock->channel = channel;
  sock->connection_description = strdup (connection_description);

  sock->protocol = protocol;
  sock->protocol_data = protocol_data;
  
  sock->read_source = NULL;
  sock->write_source = NULL;

  g_mutex_init (&sock->sock_mutex);
  sock->recv_buffer = g_byte_array_new ();
  sock->send_buffer = g_byte_array_new ();

  sock->ref_count = 1;
  
  return sock;
}

void gzochid_server_socket_free (gzochid_server_socket *sock)
{
  free (sock->name);
  free (sock);
}

static void
free_server_socket (gpointer data)
{
  gzochid_server_socket *sock = data;

  g_io_channel_unref (sock->channel);
  free (sock->addr);
  
  gzochid_server_socket_free (sock);
}

/* Add the specified client to the socket server in the form of a watch. */

static void
add_client (GzochidSocketServer *server, gzochid_client_socket *sock)
{
  sock->read_source = g_io_create_watch 
    (sock->channel, G_IO_IN | G_IO_PRI | G_IO_ERR | G_IO_HUP);

  /* Set the read callback for the socket. `free_socket' is used as a
     `GDestroyNotify' callback for the socket when this source is 
     destroyed. */
  
  g_source_set_callback
    (sock->read_source, (GSourceFunc) dispatch_client,
     gzochid_client_socket_ref (sock),
     (GDestroyNotify) gzochid_client_socket_unref);
  g_source_attach (sock->read_source, server->main_context);
  
  sock->server = server;  
}

static gboolean
dispatch_accept (GIOChannel *channel, GIOCondition cond, gpointer data)
{
  gzochid_server_socket *server_socket = data;
  gint client_fd, server_fd = g_io_channel_unix_get_fd (channel);
  struct sockaddr_in client_addr;
  socklen_t client_addr_len = sizeof (struct sockaddr_in);
  int one = 1;

  while ((client_fd = accept
	  (server_fd, (struct sockaddr *) &client_addr, &client_addr_len)) >= 0)
    {
      GIOChannel *channel = g_io_channel_unix_new (client_fd);
      char *connection_description = g_strdup_printf 
	("%d.%d.%d.%d:%d",
	 client_addr.sin_addr.s_addr & 0xff,
	 (client_addr.sin_addr.s_addr & 0xff00) >> 8,
	 (client_addr.sin_addr.s_addr & 0xff0000) >> 16,
	 (client_addr.sin_addr.s_addr & 0xff000000) >> 24,
	 client_addr.sin_port);
      
      gzochid_client_socket *sock = server_socket->protocol.accept
	(channel, connection_description, server_socket->protocol_data);

      g_free (connection_description);
      
      setsockopt (client_fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof (int));

      g_io_channel_set_encoding (channel, NULL, NULL);
      g_io_channel_set_flags (channel, G_IO_FLAG_NONBLOCK, NULL);
      g_io_channel_set_buffered (channel, FALSE);

      add_client (server_socket->server, sock);
      gzochid_client_socket_unref (sock);
    }

  return TRUE;
}

void
gzochid_client_socket_listen (GzochidSocketServer *server,
			      gzochid_client_socket *sock)
{
  add_client (server, sock);
}

void gzochid_server_socket_listen
(GzochidSocketServer *server, gzochid_server_socket *sock, gint port)
{
  gint fd = socket (PF_INET, SOCK_STREAM, 0);
  GSource *server_source = NULL;
  struct sockaddr_in addr;
  int one = 1;

  sock->channel = g_io_channel_unix_new (fd);
  server_source = g_io_create_watch (sock->channel, G_IO_IN);
  
  assert (sock->server == NULL);
  sock->server = server;
  
  addr.sin_family = AF_INET;
  addr.sin_port = htons (port);
  addr.sin_addr.s_addr = htonl (INADDR_ANY);

  setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof (int));
  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof (int));

  g_io_channel_set_flags (sock->channel, G_IO_FLAG_NONBLOCK, NULL);

  if (bind (fd, (struct sockaddr *) &addr, sizeof (struct sockaddr_in)) != 0)
    g_critical ("Failed to bind server to port %d", port);
  if (listen (fd, 128) != 0)
    g_critical ("Failed to listen on port %d", port);
  else
    {
      /* To enable the server to bind to an arbitrary port - i.e., to allow
	 game_context->port to be zero - we need a way to figure out which port
	 was actually selected by the network subsystem. Call `getsockname' on 
	 the server socket after a successful `listen' to figure this out. */
      
      struct sockaddr_in *bound_addr = NULL;
      
      sock->addr = malloc (sizeof (struct sockaddr_in));
      sock->addrlen = sizeof (struct sockaddr_in);

      bound_addr = (struct sockaddr_in *) sock->addr;

      if (getsockname (fd, sock->addr, &sock->addrlen) != 0)
	g_critical ("Failed to get name of bound socket");
      else g_message ("%s listening on port %d", sock->name,
		      ntohs (bound_addr->sin_port));
    }

  g_source_set_callback
    (server_source, (GSourceFunc) dispatch_accept, sock, free_server_socket);
  g_source_attach (server_source, server->main_context);
  g_source_unref (server_source);
}

static gpointer 
run_async (gpointer data)
{
  GzochidSocketServer *server = data;
  g_main_loop_run (server->main_loop);
  return NULL;
}

void 
gzochid_socket_server_start (GzochidSocketServer *server)
{
  g_thread_new ("socket-server", run_async, server);
}
  
void 
gzochid_socket_server_stop (GzochidSocketServer *server)
{
  g_main_loop_quit (server->main_loop);
}

GzochidSocketServer *
gzochid_client_socket_get_server (gzochid_client_socket *sock)
{
  return sock->server;
}

const char *
gzochid_client_socket_get_connection_description (gzochid_client_socket *sock)
{
  return sock->connection_description;
}

void
_gzochid_server_socket_getsockname (gzochid_server_socket *sock,
				    struct sockaddr *addr, size_t *addrlen)
{
  /* Ensure the socket is listening. */

  assert (sock->addr != NULL);
  
  memcpy (addr, sock->addr, MIN(*addrlen, sock->addrlen));
}

gzochid_client_protocol
_gzochid_client_socket_get_protocol (gzochid_client_socket *sock)
{
  return sock->protocol;
}

gpointer
_gzochid_client_socket_get_protocol_data (gzochid_client_socket *sock)
{
  return sock->protocol_data;
}

void
gzochid_client_socket_write (gzochid_client_socket *sock,
			     const unsigned char *data, size_t len)
{
  assert (sock->server != NULL);
  
  g_mutex_lock (&sock->sock_mutex);
  g_byte_array_append (sock->send_buffer, data, len);

  if (sock->write_source == NULL)
    {
      sock->write_source = g_io_create_watch (sock->channel, G_IO_OUT);
      g_source_set_callback
	(sock->write_source, (GSourceFunc) dispatch_client_write, sock, NULL);
      g_source_attach (sock->write_source, sock->server->main_context);
    }

  g_mutex_unlock (&sock->sock_mutex);
}

gzochid_client_socket *
gzochid_client_socket_ref (gzochid_client_socket *sock)
{
  g_atomic_int_inc (&sock->ref_count);
  return sock;
}

void
gzochid_client_socket_unref (gzochid_client_socket *sock)
{
  if (g_atomic_int_dec_and_test (&sock->ref_count))
    {
      /* It's not safe to free the socket synchronously if it's been added to a
	 main loop, because another thread may be in the process of dispatching
	 a read or a write and the caller has no way of knowing that. Instead, 
	 unref both sources and let the `GDestroyNotify' attached to the read 
	 source take care of the cleanup. */

      /* Need hold the socket mutex in case we're in the middle of a write. */
      
      g_mutex_lock (&sock->sock_mutex);
      
      if (sock->write_source != NULL)
	{
	  g_source_destroy (sock->write_source);
	  g_source_unref (sock->write_source);
	}
      
      g_mutex_unlock (&sock->sock_mutex);
      
      if (sock->read_source != NULL)
	{
	  if (! g_source_is_destroyed (sock->read_source))
	    g_source_destroy (sock->read_source);
	  g_source_unref (sock->read_source);
	}
      
      sock->protocol.free (sock->protocol_data);
      
      g_io_channel_unref (sock->channel);
      g_mutex_clear (&sock->sock_mutex);

      g_byte_array_unref (sock->recv_buffer);
      g_byte_array_unref (sock->send_buffer);

      g_free (sock->connection_description);

      free (sock);
    }
}

/* A pair of lifecycle callbacks for a `gzochid_reconnectable_socket'. */

struct _gzochid_reconnectable_socket_listener
{
  /* The "connected" callback. */

  gzochid_reconnectable_socket_connected connected; 

  gpointer connected_user_data; /* Closure data for the "conected" callback. */

  /* The "connected" callback. */

  gzochid_reconnectable_socket_disconnected disconnected;

  /* Closure data for the "disconnected" callback. */

  gpointer disconnected_user_data; 
};

typedef struct _gzochid_reconnectable_socket_listener
gzochid_reconnectable_socket_listener;

/* The reconnectable socket struct. */

struct _gzochid_reconnectable_socket
{
  gzochid_client_socket *socket; /* The underlying connection, or `NULL'. */
  GByteArray *outbound_messages; /* The outbound message buffer. */
  GList *listeners; /* List of `gzochid_reconnectable_socket_listener'. */
  
  /* Protects the relationship between the `socket' and `outbound_messages'
     fields. This is a recursive mutex to allow lifecycle listeners to call
     `gzochid_reconnectable_socket_write without blocking. */

  GRecMutex mutex;
};

gzochid_reconnectable_socket *
gzochid_reconnectable_socket_new ()
{
  gzochid_reconnectable_socket *sock =
    malloc (sizeof (gzochid_reconnectable_socket));

  sock->socket = NULL;
  sock->outbound_messages = g_byte_array_new ();
  sock->listeners = NULL;
  
  g_rec_mutex_init (&sock->mutex);
  
  return sock;
}

void
gzochid_reconnectable_socket_free (gzochid_reconnectable_socket *sock)
{
  if (sock->socket != NULL)
    gzochid_client_socket_unref (sock->socket);
    
  g_byte_array_unref (sock->outbound_messages);
  g_list_free_full (sock->listeners, free);
  
  g_rec_mutex_clear (&sock->mutex);
  
  free (sock);
}

void
gzochid_reconnectable_socket_listen
(gzochid_reconnectable_socket *sock,
 gzochid_reconnectable_socket_connected connected, gpointer connected_user_data,
 gzochid_reconnectable_socket_disconnected disconnected,
 gpointer disconnected_user_data)
{
  gzochid_reconnectable_socket_listener *listener =
    malloc (sizeof (gzochid_reconnectable_socket_listener));

  listener->connected = connected;
  listener->connected_user_data = connected_user_data;
  listener->disconnected = disconnected;
  listener->disconnected_user_data = disconnected_user_data;
  
  g_rec_mutex_lock (&sock->mutex);

  sock->listeners = g_list_append (sock->listeners, listener);
  
  g_rec_mutex_unlock (&sock->mutex);
}

void
gzochid_reconnectable_socket_write (gzochid_reconnectable_socket *sock,
				    unsigned char *buf, size_t len)
{
  g_rec_mutex_lock (&sock->mutex);

  if (sock->socket != NULL)

    /* If the socket's connected, delegate to the regular socket API. */
    
    gzochid_client_socket_write (sock->socket, buf, len);

  /* Otherise, enqueue it in the outbound buffer. */
  
  else g_byte_array_append (sock->outbound_messages, buf, len);
  
  g_rec_mutex_unlock (&sock->mutex);
}

void
gzochid_reconnectable_socket_connect (gzochid_reconnectable_socket *sock,
				      gzochid_client_socket *client_socket)
{
  GList *listener_ptr = NULL;
  
  g_rec_mutex_lock (&sock->mutex);

  assert (sock->socket == NULL);
  sock->socket = gzochid_client_socket_ref (client_socket);

  listener_ptr = sock->listeners;
  
  while (listener_ptr != NULL)
    {
      gzochid_reconnectable_socket_listener *listener = listener_ptr->data;

      listener->connected (listener->connected_user_data);      
      listener_ptr = listener_ptr->next;
    }

  if (sock->outbound_messages->len > 0)
    {
      /* Flush the outbound message buffer. */
      
      gzochid_client_socket_write
	(sock->socket, sock->outbound_messages->data,
	 sock->outbound_messages->len);

      g_byte_array_remove_range
	(sock->outbound_messages, 0, sock->outbound_messages->len);
    }
  
  g_rec_mutex_unlock (&sock->mutex);
}

void
gzochid_reconnectable_socket_disconnect (gzochid_reconnectable_socket *sock)
{
  GList *listener_ptr = NULL;

  g_rec_mutex_lock (&sock->mutex);

  assert (sock->socket != NULL);
  gzochid_client_socket_unref (sock->socket);
  sock->socket = NULL;

  listener_ptr = sock->listeners;
  
  while (listener_ptr != NULL)
    {
      gzochid_reconnectable_socket_listener *listener = listener_ptr->data;

      listener->disconnected (listener->disconnected_user_data);      
      listener_ptr = listener_ptr->next;
    }

  g_rec_mutex_unlock (&sock->mutex);
}
