/* oids-dataclient.c: Dataclient-based oid block allocation strategy for gzochid
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

#include <glib.h>
#include <glib-object.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "dataclient.h"
#include "oids-dataclient.h"
#include "tx.h"

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif /* G_LOG_DOMAIN */
#define G_LOG_DOMAIN "gzochid.oids-dataclient"

/* Contextual data needed by the allocation strategy. */

struct _gzochid_oid_dataclient_context
{
  GzochidDataClient *dataclient; /* The data client. */
  char *app; /* The name of requesting gzochi game application. */
};

typedef struct _gzochid_oid_dataclient_context gzochid_oid_dataclient_context;

/* Captures the state of a block reservation request pending response, with
   synchronization primitives for coordinated access. */

struct _oids_pending_response
{
  /* A block to fill out with oid block reservation data. */

  gzochid_data_oids_block block; 

  gboolean complete; /* To distinguish spurious wakeups. */
  
  GMutex mutex; /* Mutex for synchronization access to response condition. */
  GCond cond; /* Condition variable for coordinating access to the response. */

  /* Reference count to allow callbacks and cleanup to occur asynchronously 
     with allocation request. */

  int ref_count; 
};

typedef struct _oids_pending_response oids_pending_response;

/* A `GDestroyNotify' to clean up resources used by the allocation strategy. */

static void
cleanup_context (gpointer user_data)
{
  gzochid_oid_dataclient_context *context = user_data;

  g_object_unref (context->dataclient);
  free (context->app);
  free (context);
}

/* Create and return a new pending response object. The reference count on the
   object is initialized to 1. */

static oids_pending_response *
create_oids_pending_response ()
{
  oids_pending_response *pending_response =
    malloc (sizeof (oids_pending_response));
  
  pending_response->block = (gzochid_data_oids_block) { 0 };
  pending_response->complete = FALSE;
  pending_response->ref_count = 1;
  
  g_mutex_init (&pending_response->mutex);
  g_cond_init (&pending_response->cond);

  return pending_response;
}

/* Increase the reference count of the specified `oids_pending_response' 
   object and return the object. */

static oids_pending_response *
oids_pending_response_ref (oids_pending_response *pending_response)
{
  pending_response->ref_count++;
  return pending_response;
}

/* Decrease the referennce count of the specified `oids_pending_response' 
   object. If the reference count reaches zero, the memory used by the object 
   is freed. */

static void
oids_pending_response_unref (oids_pending_response *pending_response)
{
  if (--pending_response->ref_count == 0)
    {
      g_mutex_clear (&pending_response->mutex);
      g_cond_clear (&pending_response->cond);
      free (pending_response);
    }
}

/* Callback function for `gzochid_dataclient_reserve_oids. */

static void
oids_callback (gzochid_data_oids_block block, gpointer user_data)
{
  oids_pending_response *response = user_data;

  g_mutex_lock (&response->mutex);

  /* Set the completion flag to distinguish from a spurious wakeup. */
  
  response->complete = TRUE;
  
  /* Copy the block metadata to the callback object. */

  response->block = block;

  /* Let the caller know the response is complete. */
  
  g_cond_signal (&response->cond);
  g_mutex_unlock (&response->mutex);

  /* Unref the response object. */
  
  oids_pending_response_unref (response);
}

/* The allocation function implementation. */

static gboolean
allocate (gpointer user_data, gzochid_data_oids_block *block, GError **err)
{
  gboolean complete = FALSE;
  gzochid_oid_dataclient_context *context = user_data;
  oids_pending_response *pending_response = NULL;

  if (gzochid_transaction_active () && gzochid_transaction_rollback_only ())
    {
      g_set_error
	(err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	 "Transaction marked for rollback.");
      return FALSE;
    }
  
  /* Create and initialize the pending response structure. */

  pending_response = create_oids_pending_response ();

  /* Take the lock and submit the request for oids. */
  
  g_mutex_lock (&pending_response->mutex);
  
  gzochid_dataclient_reserve_oids
    (context->dataclient, context->app, oids_callback,
     oids_pending_response_ref (pending_response));

  if (gzochid_transaction_active () && gzochid_transaction_timed ())

    /* Wait on the pending response condition in a loop to handle spurious 
       wakeups. */

    while (!pending_response->complete && !gzochid_transaction_timed_out ())
      {
	struct timeval remaining = gzochid_transaction_time_remaining ();
	gint64 end_timestamp = g_get_monotonic_time () + remaining.tv_sec * 1000
	  + remaining.tv_usec;
	
	if (!g_cond_wait_until
	    (&pending_response->cond, &pending_response->mutex, end_timestamp))
	  {
	    g_debug ("Transaction timed out while waiting for oid block.");
	    g_set_error
	      (err, GZOCHID_OIDS_ERROR, GZOCHID_OIDS_ERROR_TRANSACTION,
	       "Transaction timed out while waiting for oid block.");
	  }
      }
  else while (!pending_response->complete)
	 g_cond_wait (&pending_response->cond, &pending_response->mutex);

  if (pending_response->complete)
    {
      block->block_start = pending_response->block.block_start;
      block->block_size = pending_response->block.block_size;
      
      complete = TRUE;
    }
  
  g_mutex_unlock (&pending_response->mutex);

  /* Unref the response object. */

  oids_pending_response_unref (pending_response);
  
  return complete;
}

gzochid_oid_allocation_strategy *
gzochid_dataclient_oid_strategy_new (GzochidDataClient *dataclient, char *app)
{
  gzochid_oid_dataclient_context *context =
    malloc (sizeof (gzochid_oid_dataclient_context));

  context->dataclient = g_object_ref (dataclient);
  context->app = strdup (app);
  
  return gzochid_oid_allocation_strategy_new
    (allocate, context, cleanup_context);
}
