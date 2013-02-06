/* socket.c: Application socket server implementation for gzochid
 * Copyright (C) 2012 Julian Graham
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
#include <libserveez.h>
#include <stdlib.h>

#include "context.h"
#include "fsm.h"
#include "game.h"
#include "log.h"
#include "protocol.h"
#include "socket.h"
#include "threads.h"

static int global_init (svz_servertype_t *servertype)
{
  return 0;
}

static int init (svz_server_t *server)
{
  return 0;
}

static int detect_proto (svz_server_t *server, svz_socket_t *sock)
{
  return 1;
}

static int disconnected_socket (svz_socket_t *sock)
{
  gzochid_socket_server_context *context = 
    (gzochid_socket_server_context *) sock->data;
  gzochid_protocol_client *client = 
    g_hash_table_lookup (context->clients, sock);
  
  gzochid_debug ("Socket disconnected.");
  gzochid_protocol_client_disconnected (client);

  g_hash_table_remove (context->clients, sock);
  gzochid_protocol_client_free (client);

  return 0;
}

static int check_request (svz_socket_t *sock)
{
  if (sock->recv_buffer_fill > 0)
    {
      int offset = 0, total = 0;
      int remaining = sock->recv_buffer_fill;
      
      while (remaining >= 3)
	{
	  short len = gzochi_common_io_read_short 
	    ((unsigned char *) sock->recv_buffer, offset);
	  
	  if (++len > remaining - 2)
	    break;
	  
	  offset += 2;
	  if (sock->handle_request)
	    { 
	      if (sock->handle_request 
		  (sock, sock->recv_buffer + offset, len))
		return -1;
	    }
	  else break;
	  
	  offset += len;
	  remaining -= len + 2;
	  total += len + 2;
	}
      
      svz_sock_reduce_recv (sock, total);
    }

  return 0;
}

static int connect_socket (svz_server_t *server, svz_socket_t *sock)
{
  gzochid_socket_server_context *context = 
    (gzochid_socket_server_context *) server->data;

  sock->data = context;
  sock->disconnected_socket = disconnected_socket;
  sock->check_request = check_request;
  sock->handle_request = server->handle_request;
  
  gzochid_debug ("Received socket connection."); 

  g_hash_table_insert
    (context->clients, sock, gzochid_protocol_client_accept (sock));
  return 0;
}

static int finalize (svz_server_t *server)
{
  return 0;
}

static int global_finalize (svz_servertype_t *servertype)
{
  return 0;
}

static char *info_client (svz_server_t *server, svz_socket_t *sock)
{
  return "unknown";
}

static char *info_server (svz_server_t *server)
{
  return "gzochi";
}

static int notify (svz_server_t *server)
{
  return 0;
}

static int reset (svz_server_t *server)
{
  return 0;
}

static int handle_request (svz_socket_t *sock, char *msg, int len)
{
  gzochid_socket_server_context *context = 
    (gzochid_socket_server_context *) sock->data;
  gzochid_protocol_client *client = 
    g_hash_table_lookup (context->clients, sock);

  gzochid_protocol_client_dispatch (client, (unsigned char *) msg, len);

  return 0;
}

static struct svz_servertype servertype =
  {
    "gzochi game server",
    "gzochi",
        
    global_init,     /* global_init */
    init,            /* init */
    detect_proto,    /* detect_proto */
    connect_socket,  /* connect_socket */
    finalize,        /* finalize */
    global_finalize, /* global_definition */
    info_client,     /* info_client */
    info_server,     /* info_server */
    notify,          /* notify */
    reset,           /* reset */
    handle_request   /* handle request */
  };

static void initialize_async (gpointer data, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) data;
  gzochid_fsm_to_state (context->fsm, GZOCHID_SOCKET_SERVER_STATE_RUNNING);
}

static void initialize (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_socket_server_context *server_context = 
    (gzochid_socket_server_context *) context;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;
  
  svz_portcfg_t *portcfg = NULL;
  svz_server_t *server = NULL;
  
  svz_boot ("gzochid");

  portcfg = svz_portcfg_create ();
  portcfg->name = "default";
  portcfg->proto = SVZ_PROTO_TCP;
  portcfg->flags = 0x0002;
  portcfg->protocol.tcp.port = game_context->port;
  portcfg->protocol.tcp.ipaddr = "*";

  if (svz_portcfg_mkaddr (portcfg) != 0)
    gzochid_err ("Failed to set port configuration for %s, %d", "*", 
		 game_context->port);
  if (svz_portcfg_add ("default", portcfg) == NULL)
    gzochid_err ("Failed to add port configuration for %s, %d", "*", 
		 game_context->port);

  svz_servertype_add (&servertype);
  svz_config_type_instantiate 
    ("server", "gzochi", "game-default", NULL, NULL, 0, NULL);

  server = svz_server_get ("game-default");
  server->data = server_context;

  svz_updn_all_coservers (-1);
  if (svz_updn_all_servers (1) < 0)
    gzochid_err ("Server failed to initialize");

  if (svz_server_bind (server, portcfg) != 0)
    gzochid_err ("Failed to bind server to port %d", game_context->port);

  gzochid_notice ("Game server listening on port %d", game_context->port);

  gzochid_thread_pool_push 
    (game_context->pool, initialize_async, context, NULL);  
}

static void run_async (gpointer data, gpointer user_data)
{
  svz_loop ();
}

static void run (int from_state, int to_state, gpointer user_data)
{
  gzochid_context *context = (gzochid_context *) user_data;
  gzochid_game_context *game_context = (gzochid_game_context *) context->parent;

  gzochid_thread_pool_push
    (game_context->pool, run_async, user_data, NULL);
}

static void stop (int from_state, int to_state, gpointer user_data)
{
  svz_updn_all_servers (0);
  svz_halt ();
}

gzochid_socket_server_context *gzochid_socket_server_context_new (void)
{
  gzochid_socket_server_context *context = calloc 
    (1, sizeof (gzochid_socket_server_context));

  context->clients = g_hash_table_new (g_direct_hash, g_direct_equal);

  return context;  
}

void gzochid_socket_server_context_free (gzochid_socket_server_context *context)
{
  g_hash_table_destroy (context->clients);
  gzochid_context_free ((gzochid_context *) context);
}

void gzochid_socket_server_context_init 
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
