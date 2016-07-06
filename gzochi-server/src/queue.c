/* queue.c: Simple durable FIFO queue implementation for gzochid
 * Copyright (C) 2016 Julian Graham
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
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>

#include "data.h"
#include "io.h"
#include "queue.h"
#include "util.h"

/* Architecturally, the queue is a wrapper around a linked list with head and 
   tail pointers used for remove and append operations, respectively. */

/* A link node in the queue. */

struct _gzochid_durable_queue_element
{
  mpz_t oid; /* The id of the object stored at this node. */

  /* A pointer to the next node, or `NULL' if this is the end of the queue. */
  
  gzochid_data_managed_reference *next; 
};

typedef struct _gzochid_durable_queue_element gzochid_durable_queue_element;

/* The outer queue structure. */

struct _gzochid_durable_queue
{
  /* The gzochi application context to which the queue belongs. */

  gzochid_application_context *app_context; 
  
  /* The a reference to the first `gzochid_durable_queue_element' in the queue, 
     or `NULL' if the queue is empty. */

  gzochid_data_managed_reference *head; 

  /* The a reference to the last `gzochid_durable_queue_element' in the queue, 
     or `NULL' if the queue is empty. */

  gzochid_data_managed_reference *tail;
};

/* Create and return a new `gzochid_durable_queue_element'. The memory allocated
   for this object should be freed via `gzochid_durable_queue_element_free' when
   no longer in use. */

static gzochid_durable_queue_element *
gzochid_durable_queue_element_new ()
{
  gzochid_durable_queue_element *elt = calloc
    (1, sizeof (gzochid_durable_queue_element));

  mpz_init (elt->oid);
  
  return elt;
}

/* Frees the memory associated with the specified 
   `gzochid_durable_queue_element'. */

static void
gzochid_durable_queue_element_free (gzochid_durable_queue_element *elt)
{
  mpz_clear (elt->oid);
  free (elt);
}

/* Serializer implementation for `gzochid_durable_queue_element' objects. Writes
   the oid of the referenced object, followed, optionally, by the oid of the
   next link in the queue. */

static void
serialize_element (gzochid_application_context *app_context, gpointer data,
		   GString *out, GError **err)
{
  gzochid_durable_queue_element *elt = data;
  
  gzochid_util_serialize_mpz (elt->oid, out);

  /* Is there a next link? */
  
  if (elt->next != NULL)
    {
      /* If so, write its oid. */
      
      gzochid_util_serialize_boolean (TRUE, out);
      gzochid_util_serialize_mpz (elt->next->oid, out);
    }
  else gzochid_util_serialize_boolean (FALSE, out);  
}

/* Forward declaration for the serialization, since the deserializer needs 
   it. */

static gzochid_io_serialization gzochid_durable_queue_element_serialization;

/* Deserializer implementation for `gzochid_durable_queue_element' objects. 
   Reads the oid of the referenced object, followed, optionally, by the oid of 
   the next link in the queue. */

static gpointer
deserialize_element (gzochid_application_context *app_context, GString *in,
		     GError **err)
{
  gzochid_durable_queue_element *elt = gzochid_durable_queue_element_new ();
  
  gzochid_util_deserialize_mpz (in, elt->oid);

  /* Is there a next link? */
  
  if (gzochid_util_deserialize_boolean (in))
    {
      mpz_t next_oid;

      /* If so, deserialize it. */

      mpz_init (next_oid);
      gzochid_util_deserialize_mpz (in, next_oid);

      elt->next = gzochid_data_create_reference_to_oid
	(app_context, &gzochid_durable_queue_element_serialization, next_oid);

      mpz_clear (next_oid);
    }
  
  return elt;
}

/* Finalizer implementation for `gzochid_durable_queue_element' objects. */

static void
finalize_element (gzochid_application_context *app_context, gpointer data)
{
  gzochid_durable_queue_element_free (data);
}

/* The serializer struct for `gzochid_durable_queue_element'. */

static gzochid_io_serialization gzochid_durable_queue_element_serialization =
  { serialize_element, deserialize_element, finalize_element };

/* Serializer implementation for `gzochid_durable_queue' objects. Writes the
   oids of the head and tail pointers, if they exist. */

static void
serialize_queue (gzochid_application_context *app_context, gpointer data,
		 GString *out, GError **err)
{
  gzochid_durable_queue *queue = data;

  /* Is the queue non-empty? */
  
  if (queue->head != NULL)
    {
      /* If so, write the head and tail oids. */
      
      gzochid_util_serialize_boolean (TRUE, out);
      gzochid_util_serialize_mpz (queue->head->oid, out);
      gzochid_util_serialize_mpz (queue->tail->oid, out);
    }
  else gzochid_util_serialize_boolean (FALSE, out);
}

/* Deserializer implementation for `gzochid_durable_queue' objects. Reads the
   oids of the head and tail pointers, if they exist. */

static gpointer
deserialize_queue (gzochid_application_context *app_context, GString *in,
		   GError **err)
{
  gzochid_durable_queue *queue = gzochid_durable_queue_new (app_context);

  /* Is the queue non-empty? */

  if (gzochid_util_deserialize_boolean (in))
    {
      mpz_t oid;

      mpz_init (oid);

      /* If so, read the head and tail oids. */
      
      gzochid_util_deserialize_mpz (in, oid);
      queue->head = gzochid_data_create_reference_to_oid
	(app_context, &gzochid_durable_queue_element_serialization, oid);
      gzochid_util_deserialize_mpz (in, oid);
      queue->tail = gzochid_data_create_reference_to_oid
	(app_context, &gzochid_durable_queue_element_serialization, oid);

      mpz_clear (oid);
    }
  
  return queue;
}

/* Finalizer implementation for `gzochid_durable_queue' objects. */

static void
finalize_queue (gzochid_application_context *app_context, gpointer data)
{
  gzochid_durable_queue_free (data);
}

/* The serializer struct for `gzochid_durable_queue'. */

gzochid_io_serialization gzochid_durable_queue_serialization =
  { serialize_queue, deserialize_queue, finalize_queue };

gzochid_durable_queue *
gzochid_durable_queue_new (gzochid_application_context *app_context)
{
  gzochid_durable_queue *queue = calloc (1, sizeof (gzochid_durable_queue));

  queue->app_context = app_context;
  
  return queue;
}

void
gzochid_durable_queue_free (gzochid_durable_queue *queue)
{
  free (queue);
}

void
gzochid_durable_queue_offer (gzochid_durable_queue *queue,
			     gzochid_io_serialization *serialization,
			     gpointer data, GError **err)
{
  GError *local_err = NULL;
  gzochid_durable_queue_element *elt = gzochid_durable_queue_element_new ();
  gzochid_data_managed_reference *elt_ref = NULL; 
  gzochid_data_managed_reference *data_ref = NULL; 

  elt_ref = gzochid_data_create_reference
    (queue->app_context, &gzochid_durable_queue_element_serialization, elt,
     &local_err);
  
  if (local_err == NULL)
    data_ref = gzochid_data_create_reference
      (queue->app_context, serialization, data, &local_err);
  else gzochid_durable_queue_element_free (elt);

  /* Creating the reference to the new element wrapper or to the element itself
     may fail if the transaction isn't healthy. If that happens, propagate the
     error and bail out. */
  
  if (local_err != NULL)
    {
      g_propagate_error (err, local_err);
      return;
    }
  
  mpz_set (elt->oid, data_ref->oid);
  elt->next = NULL;

  if (queue->head == NULL)
    {
      /* If the queue is empty, the new link is the head and the tail. */
      
      queue->head = elt_ref;
      queue->tail = elt_ref;
    }
  else
    {
      gzochid_data_dereference (queue->tail, &local_err);

      if (local_err == NULL)
	{
	  /* Otherwise, make the new link the "next" of the current tail, and
	     set the tail to be the new link. */
	  
	  gzochid_durable_queue_element *tail_elt = queue->tail->obj;
	  
	  if (local_err != NULL)
	    {
	      g_propagate_error (err, local_err);
	      return;
	    }
	  
	  tail_elt->next = elt_ref;
	  queue->tail = elt_ref;

	  /* Mark the old tail for modification. */
	  
	  gzochid_data_mark
	    (queue->app_context, &gzochid_durable_queue_element_serialization,
	     tail_elt, &local_err);
	}

      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);
	  return;
	}
    }

  /* Mark the entire queue for modification. */
  
  gzochid_data_mark
    (queue->app_context, &gzochid_durable_queue_serialization, queue, err);
}

gpointer
gzochid_durable_queue_peek (gzochid_durable_queue *queue,
			    gzochid_io_serialization *serialization,
			    GError **err)
{
  gzochid_data_managed_reference *head_ref = queue->head;

  if (head_ref != NULL)
    {
      GError *local_err = NULL;
      gzochid_durable_queue_element *elt =
	gzochid_data_dereference (head_ref, &local_err);

      if (local_err == NULL)
	{
	  gzochid_data_managed_reference *data_ref =
	    gzochid_data_create_reference_to_oid
	    (queue->app_context, serialization, elt->oid);

	  return gzochid_data_dereference (data_ref, err);
	}
      else g_propagate_error (err, local_err);
    }

  return NULL;
}

gpointer
gzochid_durable_queue_pop (gzochid_durable_queue *queue,
			   gzochid_io_serialization *serialization,
			   GError **err)
{
  gzochid_data_managed_reference *head_ref = queue->head;

  if (head_ref != NULL)
    {
      GError *local_err = NULL;
      gzochid_durable_queue_element *elt = gzochid_data_dereference_for_update
	(head_ref, &local_err);

      if (local_err == NULL)
	{
	  gpointer data = NULL;
	  gzochid_data_managed_reference *data_ref = NULL;
	  
	  queue->head = elt->next;

	  /* If the head of the queue is now `NULL', that must mean the queue is
	     empty. Set the tail to `NULL' as well. */
	  
	  if (queue->head == NULL)
	    queue->tail = NULL;

	  data_ref = gzochid_data_create_reference_to_oid
	    (queue->app_context, serialization, elt->oid);
	  data = gzochid_data_dereference (data_ref, &local_err);

	  if (local_err == NULL)
	    {
	      /* Remove the old head link from the data store. */
	      
	      gzochid_data_remove_object (head_ref, &local_err);

	      if (local_err == NULL)

		/* Mark the queue for modification. */
		
		gzochid_data_mark
		  (queue->app_context, &gzochid_durable_queue_serialization,
		   queue, &local_err);
	      
	      if (local_err == NULL)
		return data;
	    }
	}

      g_propagate_error (err, local_err);
    }

  return NULL;
}
