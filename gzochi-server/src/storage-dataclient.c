/* storage-dataclient.c: Dataclient-based caching storage engine for gzochid
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
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "data-protocol.h"
#include "dataclient.h"
#include "gzochid-storage.h"
#include "itree.h"
#include "lock.h"
#include "storage-mem.h"
#include "util.h"

/*
  The following data structures and functions form an implementation of a
  gzochi storage engine as a cache against a persistent store of game 
  application data managed by a gzochi meta server instance. The concepts and
  behaviors are adapted from the paper "Scalable Data Storage in Project
  Darkstar" by Tim Blackman and Jim Waldo (2009).

  Intentional locking is performed via communication with the meta server, which
  transmits keys and values in response as appropriate. Once the appropriate 
  lock is established, the keys and values are temporarily stored in a B+tree
  managed by the built-in "mem" storage engine, where they can be manipulated as
  usual by application transactions. Upon transaction commit, any changes to the
  store are transmitted to the meta server for durable persistence.
*/

/* Combines a key with the store to which it belongs, to disambiguate it in
   contexts in which keys are not otherwise partitioned. */

struct _dataclient_qualified_key
{
  char *store; /* The name of the store to which the key belongs. */
  GBytes *key; /* The key bytes. */
};

typedef struct _dataclient_qualified_key dataclient_qualified_key;

/* Combines a key range with the store to which it belongs, to disambiguate it
   in contexts in which keys are not otherwise partitioned. */

struct _dataclient_qualified_key_range
{
  char *store; /* The name of the store to which the key range belongs. */
  GBytes *from; /* The start key, which may be `NULL'. */
  GBytes *to; /* The end key, which may be `NULL'. */
};

typedef struct _dataclient_qualified_key_range dataclient_qualified_key_range;

/* A point lock on a key in a store managed by the meta server, and currently
   held by this gzochi game application server. */

struct _dataclient_lock
{
  dataclient_qualified_key *key; /* The subject of the lock. */

  /* Whether the lock is exclusive (for write) or non-exclusive (for read). */
  
  gboolean for_write; 

  int ref_count; /* The count of references to this lock. */
};

typedef struct _dataclient_lock dataclient_lock;

/*
  A range lock on the key space of a store managed by the meta server, and 
  currently held by this gzochi game application server.

  Range locks are (currently) exclusive.
*/

struct _dataclient_range_lock
{
  dataclient_qualified_key_range *key_range; /* The subject of the lock. */

  int ref_count; /* The count of references to this range lock. */
};

typedef struct _dataclient_range_lock dataclient_range_lock;

/*
  A request for a point lock (i.e., a `dataclient_lock') on the specified key
  with the specified intention.

  This structure is reference-counted to allow it to be shared by multiple 
  threads / transactions with different timings, and to be re-used across 
  multiple requests for the same key in the event of a failure / backoff 
  response from the meta server.
*/

struct _dataclient_lock_request
{
  dataclient_qualified_key *key; /* The target key. */
  gboolean for_write; /* Whether the key should be locked for write. */

  /* Protects the condition variable, requested indicator, and next request
     timestamp. */

  GMutex mutex; 

  /* If `next_request_time' is in the past, indicates whether a request for the
     lock has been sent to the meta server. */

  gboolean requested;

  /* The time after which the next request to the meta server may be made. */
  
  guint64 next_request_time;
  
  /* A condition for interested threads to wait on. Will be signaled when a 
     response is received. */

  GCond cond; 

  int ref_count; /* The count of references to this request. */
};

typedef struct _dataclient_lock_request dataclient_lock_request;

/*
  A request for a range lock (i.e., a `dataclient_range_lock') on the specified
  key with the specified intention.

  This structure is reference-counted to allow it to be shared by multiple 
  threads / transactions with different timings, and to be re-used across 
  multiple requests for the same key range in the event of a failure / backoff 
  response from the meta server.
*/

struct _dataclient_range_lock_request
{
  dataclient_qualified_key_range *key_range; /* The target key range. */
  
  /* Protects the condition variable, requested indicator, and next request
     timestamp. */

  GMutex mutex;

  /* If `next_request_time' is in the past, indicates whether a request for the
     range lock has been sent to the meta server. */

  gboolean requested;

  /* The time after which the next request to the meta server may be made. */

  guint64 next_request_time;

  /* A condition for interested threads to wait on. Will be signaled when a 
     response is received. */

  GCond cond;

  int ref_count; /* The count of references to this request. */
};

typedef struct _dataclient_range_lock_request dataclient_range_lock_request;

/*
  Represents a key for which a release has been requested but which has not yet
  actually been released via `gzochid_dataclient_release_key'. 

  Locks on keys in this state are unavailable to transactions that aren't 
  already using them, yet cannot safely be re-requested (via 
  `gzochid_dataclient_request_value') until the last active user releases its 
  reference. As such, this structure also serves as a "buffer" for requests for
  locks on keys currently being evicted.
*/

struct _dataclient_evicted_key
{
  dataclient_qualified_key key; /* The key being evicted. */

  /* The pending lock request, or `NULL'. */
  
  dataclient_lock_request *lock_request; 
};

typedef struct _dataclient_evicted_key dataclient_evicted_key;

/*
  Represents a key range for which a release has been requested but which has 
  not yet actually been released via `gzochid_dataclient_release_key_range'. 

  Locks on key ranges in this state are unavailable to transactions that aren't 
  already using them, yet cannot safely be re-requested (via 
  `gzochid_dataclient_request_next_key') until the last active user releases 
  its reference. As such, this structure also serves as a "buffer" for requests
  for locks on key ranges currently being evicted.
*/

struct _dataclient_evicted_key_range
{
  dataclient_qualified_key_range key_range; /* The key range being evicted. */

  /* The pending range lock request, or `NULL'. */

  dataclient_range_lock_request *range_lock_request;
};

typedef struct _dataclient_evicted_key_range dataclient_evicted_key_range;

/* The storage environment structure for the dataclient storage engine. */

struct _dataclient_environment
{
  char *app_name; /* The application name. */

  /* The storage engine interface for the delegate storage environment. This is
     by default the built-in memory storage engine. */

  gzochid_storage_engine_interface *delegate_iface; 

  gzochid_storage_context *delegate_context; /* The delegate storage context. */

  /* The data client. Must be injected via 
     `gzochid_dataclient_storage_context_set_dataclient'. */
  
  GzochidDataClient *client;

  GList *evicted_keys; /* List of `dataclient_evicted_key' structs. */

  /* List of `dataclient_evicted_key_range' structs. */
  
  GList *evicted_key_ranges; 
  
  GMutex mutex; /* Protects the eviction lists. */
  GMutex lock_table_mutex; /* Protects the lock tables and value cache. */

  /* `dataclient_qualified_key' keys to `dataclient_locks'. */

  GHashTable *locks; 

  /* `dataclient_qualified_key' keys to `dataclient_lock_requests'. */
  
  GHashTable *read_lock_requests; 

  /* `dataclient_qualified_key' keys to `dataclient_lock_requests'. */
  
  GHashTable *write_lock_requests; 

  /* A read-through cache of `dataclient_qualified_key' keys to `GBytes' 
     values. */

  GHashTable *value_cache;  
};

typedef struct _dataclient_environment dataclient_environment;
  
/* The store structure; encapsulates the set of locks on the target store 
   currently held by this application server, the state of all pending lock 
   requests, and a pointer to the delegate store where the actual keys and 
   values are cached. */

struct _dataclient_database
{
  char *name; /* The store name. */
  
  GMutex mutex; /* Protects the range lock and range lock request tables. */
  
  gzochid_itree *range_locks; /* `GBytes' bounds to `dataclient_range_locks'. */
  
  /* `GBytes' bounds to `dataclient_range_lock_requests'. */
  
  gzochid_itree *range_lock_requests;

  gzochid_storage_store *delegate_store; /* The delegate mem store. */
};

typedef struct _dataclient_database dataclient_database;

/* The transaction proxy structure. Includes transaction-specific point and
   range lock tables, as well as a pointer to a delegate memory storage engine
   transaction. */

struct _dataclient_transaction
{
  gint64 end_time; /* Transaction end time, in milliseconds since the epoch. */
  
  /* `GBytes' keys to `dataclient_locks'; the subset of point locks held by the
     application server that are in use by this transaction. */

  GHashTable *locks;

  /* `GBytes' bounds to `dataclient_range_lock_requests'; the subset of range
     locks held by the application server that are in use by this 
     transaction. */  
  
  gzochid_itree *range_locks;

  /* An array of `gzochid_data_change' for each put or delete executed in this 
     transaction. */

  GArray *changeset; 

  /* Tracks deleted keys, which need to be purged from the value cache if the 
     transaction commits. */

  GList *deleted_keys; 
  
  gzochid_storage_transaction *delegate_tx; /* The delegate transaction. */
};

typedef struct _dataclient_transaction dataclient_transaction;

/* Closure data for the success and failure callback functions for point and
   range lock requests. */

struct _dataclient_callback_data
{
  gzochid_storage_store *store; /* The target store. */

  /* In the case of point lock requests, the target key; for range lock 
     requests, the lower bound of the requested range, or `NULL' if the first
     key in the keyspace was requested. */

  GBytes *key;

  /* Whether an exclusive (write) lock was requested. In the case of a range 
     lock request, this field is ignored. */

  gboolean for_write;
};

typedef struct _dataclient_callback_data dataclient_callback_data;

void
gzochid_dataclient_storage_context_set_dataclient
(gzochid_storage_context *context, GzochidDataClient *client)
{
  dataclient_environment *environment = context->environment;

  assert (environment->client == NULL); /* This should only be set once. */

  environment->client = g_object_ref (client);
}

/* A hash function for `dataclient_qualified_key' structures. Combines the hash
   codes produced by delegating to `g_str_hash' (for the store name) and
   `g_bytes_hash' (for the key bytes). */

static guint
dataclient_qualified_key_hash (gconstpointer v)
{
  const dataclient_qualified_key *k = v;

  return g_str_hash (k->store) * 31 + g_bytes_hash (k->key);
}

/* An equality function for `dataclient_qualified_key' structures. Two qualified
   keys are equal if their store names are equal (via `strcmp') and their key
   byte buffers are equal (via `g_bytes_equal'). */

static gboolean
dataclient_qualified_key_equal (gconstpointer v1, gconstpointer v2)
{
  const dataclient_qualified_key *k1 = v1;
  const dataclient_qualified_key *k2 = v2;

  return strcmp (k1->store, k2->store) == 0 && g_bytes_equal (k1->key, k2->key);
}

/* Create and return a new `dataclient_qualified_key' structure with the 
   specifed store name and key bytes. The memory used by this structure
   should be freed via `dataclient_qualified_key_free' when no longer in use. */

static dataclient_qualified_key *
dataclient_qualified_key_new (const char *store, GBytes *key)
{
  dataclient_qualified_key *k = malloc (sizeof (dataclient_qualified_key));

  k->store = strdup (store);
  k->key = g_bytes_ref (key);
  
  return k;  
}

/* Frees the specified `dataclient_qualified_key'. Can be used as a 
   `GDestroyNotify' where appropriate. */

static void
dataclient_qualified_key_free (gpointer data)
{
  dataclient_qualified_key *key = data;

  free (key->store);
  g_bytes_unref (key->key);
  
  free (key);
}

/* Create and return a new `dataclient_qualified_key' structure with a store 
   name and key bytes equal to those of the specified qualified key. */

static dataclient_qualified_key *
dataclient_qualified_key_copy (dataclient_qualified_key *k)
{
  return dataclient_qualified_key_new (k->store, k->key);
}

/* A `GCompareFunc' implementation for `dataclient_qualified_key' objects. */

static gint dataclient_qualified_key_compare (gconstpointer a, gconstpointer b)
{
  const dataclient_qualified_key *key_a = a;
  const dataclient_qualified_key *key_b = b;

  gint ret = strcmp (key_a->store, key_b->store);

  return ret == 0 ? g_bytes_compare (key_a->key, key_b->key) : ret;
}

/* Create and return a new `dataclient_qualified_key_range' structure with the 
   specifed store name and key range bytes. The memory used by this structure
   should be freed via `dataclient_qualified_key_range_free' when no longer in
   use. */

static dataclient_qualified_key_range *
dataclient_qualified_key_range_new (const char *store, GBytes *from, GBytes *to)
{
  dataclient_qualified_key_range *k =
    malloc (sizeof (dataclient_qualified_key_range));

  k->store = strdup (store);
  k->from = from == NULL ? NULL : g_bytes_ref (from);
  k->to = to == NULL ? NULL : g_bytes_ref (to);
  
  return k;  
}

/* Frees the specified `dataclient_qualified_key'. Can be used as a 
   `GDestroyNotify' where appropriate. */

static void
dataclient_qualified_key_range_free (gpointer data)
{
  dataclient_qualified_key_range *key_range = data;
  
  free (key_range->store);

  if (key_range->from != NULL)
    g_bytes_unref (key_range->from);
  if (key_range->to != NULL)
    g_bytes_unref (key_range->to);
  
  free (key_range);
}

/* Create and return a new `dataclient_qualified_key_range' structure with a 
   store name and from / to key bytes equal to those of the specified qualified
   key range. */

static dataclient_qualified_key_range *
dataclient_qualified_key_range_copy (dataclient_qualified_key_range *k)
{
  return dataclient_qualified_key_range_new (k->store, k->from, k->to);
}

/* A `GCompareFunc' implementation for `dataclient_qualified_key_range' 
   objects. */

static gint dataclient_qualified_key_range_compare (gconstpointer a,
						    gconstpointer b)
{
  const dataclient_qualified_key_range *key_range_a = a;
  const dataclient_qualified_key_range *key_range_b = b;

  gint ret = strcmp (key_range_a->store, key_range_b->store);

  if (ret == 0)
    ret = gzochid_util_bytes_compare_null_first
      (key_range_a->from, key_range_b->from);

  return ret == 0
    ? gzochid_util_bytes_compare_null_last (key_range_a->to, key_range_b->to)
    : ret;
}

/* Create and return a new `dataclient_evicted_key' structure with the specified
   store name and key. The memory used by this structure should be freed via 
   `dataclient_evicted_key_free' when no longer in use. */

static dataclient_evicted_key *
dataclient_evicted_key_new (const char *store, GBytes *key)
{
  dataclient_evicted_key *k = malloc (sizeof (dataclient_evicted_key));

  k->key.store = strdup (store);
  k->key.key = g_bytes_ref (key);
  k->lock_request = NULL;

  return k;
}

/* Frees the specified `dataclient_evicted_key'. */

static void
dataclient_evicted_key_free (dataclient_evicted_key *key)
{
  dataclient_qualified_key_free (&key->key);
}

/* Create and return a new `dataclient_evicted_key_range' structure with the 
   specified store name and key. The memory used by this structure should be 
   freed via `dataclient_evicted_key_range_free' when no longer in use. */

static dataclient_evicted_key_range *
dataclient_evicted_key_range_new (const char *store, GBytes *from, GBytes *to)
{
  dataclient_evicted_key_range *k =
    malloc (sizeof (dataclient_evicted_key_range));

  k->key_range.store = strdup (store);
  k->key_range.from = from == NULL ? NULL : g_bytes_ref (from);
  k->key_range.to = to == NULL ? NULL : g_bytes_ref (to);
  k->range_lock_request = NULL;
  
  return k;
}

/* Frees the specified `dataclient_evicted_key_range'. */

static void
dataclient_evicted_key_range_free (dataclient_evicted_key_range *key_range)
{
  dataclient_qualified_key_range_free (&key_range->key_range);
}

/* Create and return a new point lock structure for the specified qualified key
   and with the specified access level. The memory used by this structure 
   should be freed via `free_lock' when no longer in use. */

static dataclient_lock *
lock_new (dataclient_qualified_key *qualified_key, gboolean for_write)
{
  dataclient_lock *lock = malloc (sizeof (dataclient_lock));
     
  lock->ref_count = 1;
  lock->key = dataclient_qualified_key_copy (qualified_key);
  lock->for_write = for_write;

  return lock;
}

/* Free the specified point lock structure. This function should only be called
   after the lock has been released by all users; i.e., all R/W locks have been
   released and the lock is no longer present in any lock tables. */

static void
free_lock (dataclient_lock *lock)
{
  dataclient_qualified_key_free (lock->key);
  free (lock);
}

/* Increases the reference count of the specified lock by one, and returns
   the lock. This function should not be called on locks that have been removed
   from their store's lock table. */

static dataclient_lock *
lock_ref (dataclient_lock *lock)
{
  lock->ref_count++;
  return lock;
}

/* Returns the evicted key record for the specified qualified key in the target
   environment, or `NULL' if no such record exists. The environment's mutex 
   must be held by the caller. */

static dataclient_evicted_key *
find_evicted_key (dataclient_environment *environment,
		  dataclient_qualified_key *qualified_key)
{
  GList *evicted_key_link = g_list_find_custom
    (environment->evicted_keys, qualified_key,
     dataclient_qualified_key_compare);

  return evicted_key_link != NULL ? evicted_key_link->data : NULL;
}

/*
  Decreases the reference count of the specified lock by one. Returns `TRUE' if
  the lock's updated reference count is reduced to zero or less, `FALSE' 
  otherwise.

  Upon hitting zero, the data client for the specified environment is notified
  that the lock should be released. The lock itself is also freed, via 
  `free_lock'.
*/

static gboolean
lock_unref (dataclient_environment *environment, dataclient_lock *lock)
{
  if (--lock->ref_count <= 0)
    {
      dataclient_evicted_key *evicted_key =
	find_evicted_key (environment, lock->key);
	
      gzochid_dataclient_release_key
	(environment->client, environment->app_name, lock->key->store,
	 lock->key->key);

      free_lock (lock);

      if (evicted_key != NULL)
	{
	  if (evicted_key->lock_request != NULL)
	    {
	      /* If this key was evicted and somebody tried to request it, 
		 they're currently blocking on it being ready to submit the
		 request to the data client. Setting its next request time to a
		 real value will allow the request to be made. */
	      
	      dataclient_lock_request *lock_request =
		evicted_key->lock_request;

	      g_mutex_lock (&lock_request->mutex);

	      lock_request->next_request_time = g_get_monotonic_time ();

	      /* Wake up the waiter so they can actually request it. */
	      
	      g_cond_signal (&lock_request->cond);
	      g_mutex_unlock (&lock_request->mutex);
	    }

	  /* Remove the key from the eviction list... */

	  g_mutex_lock (&environment->mutex);
	  
	  environment->evicted_keys = g_list_remove
	    (environment->evicted_keys, evicted_key);
	  
	  dataclient_evicted_key_free (evicted_key); /* ...and free it. */

	  g_mutex_unlock (&environment->mutex);
	}

      return TRUE;
    }
  
  return FALSE;
}

/* Create and return a new range lock structure for the specified qualified key
   range. The memory used by this structure should be freed via 
   `free_range_lock' when no longer in use. */

static dataclient_range_lock *
range_lock_new (dataclient_qualified_key_range *key_range)
{
  dataclient_range_lock *range_lock = malloc (sizeof (dataclient_range_lock));

  range_lock->ref_count = 1;
  range_lock->key_range = dataclient_qualified_key_range_copy (key_range);
  
  return range_lock;
}
  
/* Free the specified range lock structure. This function should only be called
   after the lock has been released by all users; i.e., all R/W locks have been
   released and the lock is no longer present in any lock tables. */

static void
free_range_lock (dataclient_range_lock *range_lock)
{
  dataclient_qualified_key_range_free (range_lock->key_range);
  free (range_lock);
}

/* Increases the reference count of the specified range lock by one, and returns
   the lock. This function should not be called on range locks that have been
   removed from their store's range lock interval tree. */

static dataclient_range_lock *
range_lock_ref (dataclient_range_lock *range_lock)
{
  range_lock->ref_count++;
  return range_lock;
}

/* Returns the evicted key range record for the specified qualified key range 
   in the target environment, or `NULL' if no such record exists. The 
   environment's mutex must be held by the caller. */

static dataclient_evicted_key_range *
find_evicted_key_range (dataclient_environment *environment,
			dataclient_qualified_key_range *qualified_key_range)
{
  GList *evicted_key_range_link = g_list_find_custom
    (environment->evicted_key_ranges, qualified_key_range,
     dataclient_qualified_key_range_compare);

  return evicted_key_range_link != NULL ? evicted_key_range_link->data : NULL;
}

/*
  Decreases the reference count of the specified range lock by one. Returns 
  `TRUE' if the range lock's updated reference count is reduced to zero or less,
  `FALSE' otherwise.

  Upon hitting zero, the data client for the specified environment is notified
  that the range lock should be released. The range lock itself is also freed,
  via `free_range_lock'.

  Because this function manipulates the eviction lists, the caller must hold
  the environment's mutex.
*/

static gboolean
range_lock_unref (dataclient_environment *environment,
		  dataclient_range_lock *range_lock)
{
  if (--range_lock->ref_count <= 0)
    {
      /* The eviction list is in terms of the requested ranges, which means for
	 the purposes of searching the list, the target upper bound is 
	 `NULL'. */
      
      dataclient_qualified_key_range qualified_key_range =
	(dataclient_qualified_key_range)
	{ range_lock->key_range->store, range_lock->key_range->from, NULL };
      dataclient_evicted_key_range *evicted_key_range =
	find_evicted_key_range (environment, &qualified_key_range);

      gzochid_dataclient_release_key_range
	(environment->client, environment->app_name,
	 range_lock->key_range->store, range_lock->key_range->from,
	 range_lock->key_range->to);

      free_range_lock (range_lock);

      if (evicted_key_range != NULL)
	{
	  if (evicted_key_range->range_lock_request != NULL)
	    {
	      /* If this key range was evicted and somebody tried to request it,
		 they're currently blocking on it being ready to submit the
		 request to the data client. Setting its next request time to a
		 real value will allow the request to be made. */

	      dataclient_range_lock_request *range_lock_request =
		evicted_key_range->range_lock_request;

	      g_mutex_lock (&range_lock_request->mutex);

	      range_lock_request->next_request_time = g_get_monotonic_time ();

	      /* Wake up the waiter so they can actually request it. */

	      g_cond_signal (&range_lock_request->cond);
	      g_mutex_unlock (&range_lock_request->mutex);
	    }

	  /* Remove the key range from the eviction list... */

	  environment->evicted_key_ranges = g_list_remove
	    (environment->evicted_key_ranges, evicted_key_range);

	  /* ...and free it. */
	  
	  dataclient_evicted_key_range_free (evicted_key_range); 
	}

      return TRUE;
    }

  return FALSE;
}

/* Captures state used during a search for a pending range lock request. */

struct _dataclient_range_lock_request_search_context
{
  char *store; /* The target store. */
  
  /* The lower end of the target key space, or `NULL' to indicate the beginning
     of the key space. */

  GBytes *from;

  /* The upper end of the target key space, or `NULL' to indicate the end of the
     key space. */

  GBytes *to;

  /* The range lock request that was found, or `NULL' if no such request was 
     found. */
  
  dataclient_range_lock_request *range_lock_request;
};

typedef struct _dataclient_range_lock_request_search_context
dataclient_range_lock_request_search_context;

/* Captures state used during a search for an active range lock. */

struct _dataclient_range_lock_search_context
{
  char *store; /* The target store. */

  /* The lower end of the target key space, or `NULL' to indicate the beginning
     of the key space. */

  GBytes *from;

  /* The upper end of the target key space, or `NULL' to indicate the end of the
     key space. */

  GBytes *to;

  /* The range lock that was found, or `NULL' if no such lock was found. */

  dataclient_range_lock *range_lock; 
};

typedef struct _dataclient_range_lock_search_context
dataclient_range_lock_search_context;

/* A `gzochid_itree_search_func' that matches the first range lock request for a
   key range that exactly matches the key range specified in the search 
   context. */

static gboolean
find_exact_range_lock_request (gpointer from, gpointer to, gpointer data,
			       gpointer user_data)
{
  dataclient_range_lock_request *request = data;
  dataclient_range_lock_request_search_context *search_context = user_data;
  
  if (strcmp (search_context->store, request->key_range->store) == 0
      && gzochid_util_bytes_compare_null_first (from, search_context->from) == 0
      && gzochid_util_bytes_compare_null_last (to, search_context->to) == 0)
    {
      search_context->range_lock_request = data;
      return TRUE;
    }
  else return FALSE;
}

/* A `gzochid_itree_search_func' that matches the first range lock for a key 
   ranges that exactly matches the key range specified in the search context. */

static gboolean
find_exact_range_lock (gpointer from, gpointer to, gpointer data,
		       gpointer user_data)
{
  dataclient_range_lock_request *request = data;
  dataclient_range_lock_search_context *search_context = user_data;
  
  if (strcmp (search_context->store, request->key_range->store) == 0
      && g_bytes_equal (from, search_context->from)
      && g_bytes_equal (to, search_context->to))
    {
      search_context->range_lock = data;
      return TRUE;
    }
  else return FALSE;
}

/* A `gzochid_itree_search_func' that matches the first range lock request for a
   key range that completely encloses the key range specified in the search 
   context. */

static gboolean
find_covering_range_lock_request (gpointer from, gpointer to, gpointer data,
				  gpointer user_data)
{
  dataclient_range_lock_request *request = data;
  dataclient_range_lock_request_search_context *search_context = user_data;
  
  if (strcmp (search_context->store, request->key_range->store) == 0
      && gzochid_util_bytes_compare_null_first (from, search_context->from) <= 0
      && gzochid_util_bytes_compare_null_last (to, search_context->to) >= 0)
    {
      search_context->range_lock_request = data;
      return TRUE;
    }
  else return FALSE;
}

/*
  A `gzochid_itree_search_func' that matches the first range lock for a key 
  range that encloses the lower bound of the key range specified in the search 
  context such that the key is greater than or equal to the lower bound of the 
  range lock and exactly less than the upper bound.

  If this lower bound is `NULL', this function will match the 
  first range lock with a `NULL' lower bound.
*/

static gboolean
find_range_lock_covering_point (gpointer from, gpointer to, gpointer data,
				gpointer user_data)
{
  dataclient_range_lock_request *request = data;
  dataclient_range_lock_search_context *search_context = user_data;

  if (strcmp (search_context->store, request->key_range->store) == 0)
    {
      if (search_context->from == NULL)
	{
	  if (from == NULL)
	    {
	      /* Any range lock that encloses the beginning of the key space 
		 will match a `NULL' search key. */
	  
	      search_context->range_lock = data;
	      return TRUE;
	    }
	  else return FALSE;
	}
      else if
	(gzochid_util_bytes_compare_null_first (search_context->from, from) >= 0
	 && gzochid_util_bytes_compare_null_last (search_context->from, to) < 0)
	{
	  /* The "from" key under search must be exactly less than the upper 
	     bound of the matched range lock. */
      
	  search_context->range_lock = data;
	  return TRUE;
	}
      else return FALSE;
    }
  else return FALSE;
}

/* Create and return a new callback data object with the specified store, 
   target key, and intention. */

static dataclient_callback_data *
create_callback_data (gzochid_storage_store *store, GBytes *key,
		      gboolean for_write)
{
  dataclient_callback_data *callback_data =
    malloc (sizeof (dataclient_callback_data));

  callback_data->store = store;
  callback_data->key = key == NULL ? NULL : g_bytes_ref (key);
  callback_data->for_write = for_write;
  
  return callback_data;
}

/* Frees the specified callback data object. This function should be called by
   the callback function. */

static void
callback_data_free (dataclient_callback_data *callback_data)
{
  if (callback_data->key != NULL)
    g_bytes_unref (callback_data->key);

  free (callback_data);
}

/*
  Notify any threads that may be waiting for a point lock on the specified key
  that the lock may have been acquired. (The awoken thread will need to check
  the lock table itself to make sure.)
  
  The caller of this function must hold the lock table mutex of the environment
  that encompasses the target lock table.
*/

static inline void
notify_waiters (GHashTable *lock_table, dataclient_qualified_key *qualified_key)
{
  dataclient_lock_request *lock_request = g_hash_table_lookup
    (lock_table, qualified_key);
      
  if (lock_request != NULL)
    {
      g_mutex_lock (&lock_request->mutex);
      g_cond_broadcast (&lock_request->cond);
      g_mutex_unlock (&lock_request->mutex);
    }
}

/* The "success" callback for point lock requests. Adds the lock to the store's
   lock table and notifies any waiting transaction threads so that they can add
   the lock to their local lock tables. */

static void
lock_success_callback (GBytes *data, gpointer user_data)
{
  dataclient_callback_data *callback_data = user_data;
  dataclient_database *database = callback_data->store->database;
  dataclient_environment *environment =
    callback_data->store->context->environment;
  dataclient_qualified_key qualified_key = (dataclient_qualified_key)
    { database->name, callback_data->key };
  
  dataclient_lock *lock = NULL;
  
  g_mutex_lock (&environment->lock_table_mutex);
    
  lock = g_hash_table_lookup (environment->locks, &qualified_key);
  
  if (lock != NULL)
    {
      /* If the lock already exists, conditionally upgrade it. */
      
      if (callback_data->for_write)

	/* Need to bump the lock's ref count, since an additional success 
	   callback indicates that another release callback will be 
	   forthcoming. */
	
	lock_ref (lock)->for_write = TRUE;
    }

  /* Otherwise create a new lock. */
  
  else g_hash_table_insert
	 (environment->locks, dataclient_qualified_key_copy (&qualified_key),
	  lock_new (&qualified_key, callback_data->for_write));

  if (data != NULL)

    /* If there's data, don't insert it directly into the mem store (since 
       doing so requires a transaction and may induce contention) put it in the
       fallback cache, which will be consulted by readers if there's a miss in
       the mem store. */
    
    g_hash_table_insert
      (environment->value_cache, dataclient_qualified_key_copy (&qualified_key),
       g_bytes_ref (data));
  
  if (callback_data->for_write)
    {
      notify_waiters (environment->write_lock_requests, &qualified_key);
      g_hash_table_remove (environment->write_lock_requests, &qualified_key);
    }

  /* Always notify threads waiting for read locks - they'll be happy with a
     write lock as well. */
  
  notify_waiters (environment->read_lock_requests, &qualified_key);
  g_hash_table_remove (environment->read_lock_requests, &qualified_key);

  g_mutex_unlock (&environment->lock_table_mutex);
}

/* The "failure" callback for point lock requests. Resets the request state of
   pending read / write lock request objects (if they exist) and wakes up any
   transaction threads waiting on the request so that they can handle a retry
   after the wait time expires. */

static void
lock_failure_callback (struct timeval wait_time, gpointer user_data)
{
  dataclient_callback_data *callback_data = user_data;
  dataclient_database *database = callback_data->store->database;
  dataclient_environment *environment =
    callback_data->store->context->environment;
  dataclient_qualified_key qualified_key =
    { database->name, callback_data->key };
 
  dataclient_lock_request *lock_request = NULL;

  g_mutex_lock (&environment->lock_table_mutex);
  
  if (callback_data->for_write)
    {
      lock_request = g_hash_table_lookup
	(environment->write_lock_requests, &qualified_key);

      /* There may be no lock request if there are no longer any threads waiting
	 for the lock. */
      
      if (lock_request != NULL)
	{      
	  lock_request->requested = FALSE;
	  lock_request->next_request_time = g_get_monotonic_time ()
	    + wait_time.tv_sec * 1000
	    + wait_time.tv_usec / 1000;
	  
	  notify_waiters (environment->write_lock_requests, &qualified_key);
	}
    }

  /* There may be no lock request if there are no longer any threads waiting
     for the lock. */

  lock_request = g_hash_table_lookup
    (environment->read_lock_requests, &qualified_key);

  if (lock_request != NULL)
    {      
      lock_request->requested = FALSE;
      lock_request->next_request_time = g_get_monotonic_time ()
	+ wait_time.tv_sec * 1000
	+ wait_time.tv_usec / 1000;

      notify_waiters (environment->read_lock_requests, &qualified_key);
    }

  /* The release callback won't be called for this request, so free the callback
     data here. */
  
  callback_data_free (callback_data);
  
  g_mutex_unlock (&environment->lock_table_mutex);
}

/* The "release" callback for point locks. Removes a reference to the lock and
   potentially adds the target key to the eviction list. */

static void
lock_release_callback (gpointer user_data)
{
  dataclient_lock *lock = NULL;
  dataclient_callback_data *callback_data = user_data;
  gzochid_storage_store *store = callback_data->store;
  dataclient_environment *environment = store->context->environment;
  dataclient_database *database = store->database;
  dataclient_qualified_key qualified_key = (dataclient_qualified_key)
    { database->name, callback_data->key };

  /* Grab the environment's mutex before the store's mutex; important to always
     take these in the same order. */
  
  g_mutex_lock (&environment->mutex);
  g_mutex_lock (&environment->lock_table_mutex);
  
  lock = g_hash_table_lookup (environment->locks, &qualified_key);

  if (lock != NULL)
    {
      /*
	Are there additional references to this key? I.e., are there any 
	active transactions still using it?

	Note that we need to check whether the key is _already_ in the evicted
	key list because of the special case around upgrading locks in unbounded
	transactions; having two copies of a key in the eviction list will harm
	the throughout of transactions that _are_ bounded.
      */
      
      if (! lock_unref (environment, lock))
	{
	  if (g_list_find_custom (environment->evicted_keys, &qualified_key,
				  dataclient_qualified_key_compare) == NULL)

	    /* If so, add the key to the list of in-progress evictions.  */
	
	    environment->evicted_keys = g_list_prepend
	      (environment->evicted_keys,
	       dataclient_evicted_key_new (database->name, callback_data->key));
	}
      
      /* If this was the last reference to the lock, remove it from the lock
	 table. */
      
      else g_hash_table_remove (environment->locks, &qualified_key);
    }

  g_hash_table_remove (environment->value_cache, &qualified_key);

  callback_data_free (callback_data);

  g_mutex_unlock (&environment->lock_table_mutex);
  g_mutex_unlock (&environment->mutex);
}

/*
  Increases the reference count of the specified lock request and returns it. 

  For safety's sake, the mutex of the store that owns the lock request should be
  held by the caller of this function during the lookup of the request and for 
  the duration of this function.
*/

static dataclient_lock_request *
lock_request_ref (dataclient_lock_request *lock_request)
{
  lock_request->ref_count++;
  return lock_request;
}

/*
  Decreases the reference count of the specified lock request. If the 
  reference count reaches zero, the lock request is freed.

  For safety's sake, the mutex of the store that owns the lock request should be
  held by the caller of this function during the lookup of the request, for the 
  duration of this function, and for the removal of the request - if 
  appropriate - from the lock request table.
*/

static gboolean
lock_request_unref (dataclient_lock_request *lock_request)
{
  /* Decrement the reference count before testing the value. */
  
  gboolean ret = --lock_request->ref_count <= 0;

  if (ret)
    {
      dataclient_qualified_key_free (lock_request->key);

      g_mutex_clear (&lock_request->mutex);
      g_cond_clear (&lock_request->cond);
      
      free (lock_request);
    }

  return ret;
}

/* Create and return a new lock request object for the specified key and 
   intention. The returned request will have an initial reference count of 1. */

static dataclient_lock_request *
lock_request_new (const char *store, GBytes *key, gboolean for_write)
{
  dataclient_lock_request *lock_request =
    malloc (sizeof (dataclient_lock_request));

  lock_request->key = dataclient_qualified_key_new (store, key);
  lock_request->for_write = for_write;

  lock_request->requested = FALSE;
  lock_request->next_request_time = 0;
  
  g_mutex_init (&lock_request->mutex);
  g_cond_init (&lock_request->cond);

  lock_request->ref_count = 1;
  
  return lock_request;
}

/*
  If the store holds a lock on the specified key with the specified intention, 
  add that lock to the specified transaction's set of local locks, and obtain a
  non-exclusive read lock on it. 

  To ensure that this operation is properly synchronous, the environment's lock
  table mutex must be held by the caller of this function.
*/

static inline gboolean
check_and_set_lock (dataclient_environment *env, dataclient_transaction *tx,
		    dataclient_qualified_key *qualified_key, gboolean for_write)
{
  dataclient_lock *lock = g_hash_table_lookup (env->locks, qualified_key);
  
  if (lock != NULL && (!for_write || lock->for_write))
    {
      if (! g_hash_table_contains (tx->locks, qualified_key))
	  
	/* If everything is properly synchronized, it should be impossible for
	   this block to be entered while a lock is being reaped. */	     
	
	g_hash_table_insert
	  (tx->locks, dataclient_qualified_key_copy (qualified_key),
	   lock_ref (lock));
	  
      return TRUE;
    }

  return FALSE;
}

/*
  Ensure that the specified transaction holds a point lock on the specified key
  in the specified store, with the specified read / write intention. If the 
  lock is not already held by the transaction, an attempt is made as follows to
  obtain it:
  
  1. If the store proxy on this application server contains a matching lock, add
     it to the transaction's set of locks.
  2. Otherwise, if an attempt to obtain a matching lock is already in progress,
     register to receive notifications on the outcome of that attempt, and wait.
     If the lock is successfully obtained, add it to the transaction; if the 
     attempt fails, wait out the timeout interval and repeat.
  3. Otherwise, send a request to the meta server for that lock, then wait, as
     described above.

  Returns `TRUE' if the lock was already held or could be obtained within the
  lifespan of the current transaction, `FALSE' otherwise.
*/

static gboolean
ensure_lock (gzochid_storage_transaction *tx, gzochid_storage_store *store,
	     char *key, size_t key_len, gboolean for_write)
{
  dataclient_database *database = store->database;
  dataclient_environment *environment = store->context->environment;
  dataclient_transaction *dataclient_tx = tx->txn;  
  GBytes *key_bytes = g_bytes_new (key, key_len);

  dataclient_qualified_key qualified_key = { database->name, key_bytes };
  
  dataclient_lock *lock = g_hash_table_lookup
    (dataclient_tx->locks, &qualified_key);
  dataclient_evicted_key *evicted_key = NULL;
  dataclient_lock_request *lock_request = NULL;
  
  /* Doees the transaction already hold a matching lock? */
  
  if (lock != NULL && (!for_write || lock->for_write))
    {
      g_bytes_unref (key_bytes);
      return TRUE;
    }

  g_mutex_lock (&environment->mutex);
  g_mutex_lock (&environment->lock_table_mutex);
  
  /* Make a synchronous attempt to seize a reference to the local cache / 
     proxy's instance of the lock, if it exists. */
  
  if (check_and_set_lock
      (environment, dataclient_tx, &qualified_key, for_write))
    {
      g_bytes_unref (key_bytes);
      g_mutex_unlock (&environment->lock_table_mutex);
      g_mutex_unlock (&environment->mutex);
      return TRUE;
    }

  /* Is there a lock request in progress? It could be attached to a key in the
     eviction list, so check there first. */

  evicted_key = find_evicted_key (environment, &qualified_key);
    
  if (evicted_key != NULL)
    {
      if (evicted_key->lock_request == NULL)
	{
	  /* The key's being evicted, but no one else has requested it. Create
	     a new lock request and add it to the eviction record and also
	     insert it into the store's lock request table. */
	  
	  lock_request = lock_request_new
	    (database->name, key_bytes, for_write);

	  /* There's a special case when the current transaction isn't timed:
	     A key with a read lock may be in the eviction list even as we're 
	     requesting to upgrade it to a write lock. It doesn't make sense to
	     make the transaction wait, because it'll wait forever. In this
	     case, ignore the fact that the key's being evicted and let it be
	     re-requested immediately. */
	  
	  if (dataclient_tx->end_time == G_MAXINT64)
	    lock_request->next_request_time = 0;

	  /* Otherwise, it can't be requested yet because it's in the eviction 
	     list. */
	  
	  else lock_request->next_request_time = G_MAXINT64;
	  
	  evicted_key->lock_request = lock_request;
	 
	  g_hash_table_insert
	    (for_write
	     ? environment->write_lock_requests
	     : environment->read_lock_requests,
	     dataclient_qualified_key_copy (&qualified_key), lock_request);
	  
	  g_mutex_lock (&lock_request->mutex);
	}
      else
	{
	  /* The request must have already been added to the store's request
	     table, so all we need to do here is increase its reference 
	     count. */
	  
	  g_mutex_lock (&evicted_key->lock_request->mutex);
	  lock_request = lock_request_ref (evicted_key->lock_request);
	}

      /* Once the request mutex is held it's safe to release these. */

      g_mutex_unlock (&environment->lock_table_mutex);
      g_mutex_unlock (&environment->mutex);
    }
  else
    {
      /* Release the general environment mutex, since we're done with the 
	 eviction list. */

      g_mutex_unlock (&environment->mutex);
      
      lock_request = g_hash_table_lookup
	(for_write
	 ? environment->write_lock_requests
	 : environment->read_lock_requests, &qualified_key);

      /* Is there a lock request in the store's request table? */
      
      if (lock_request != NULL)
	{
	  g_mutex_lock (&lock_request->mutex);
	  lock_request_ref (lock_request);
	}
      else 
	{
	  /* If not, create one. */
	  
	  lock_request = lock_request_new
	    (database->name, key_bytes, for_write);

	  g_hash_table_insert
	    (for_write
	     ? environment->write_lock_requests
	     : environment->read_lock_requests,
	     dataclient_qualified_key_copy (&qualified_key), lock_request);
	  
	  g_mutex_lock (&lock_request->mutex);
	}

      g_mutex_unlock (&environment->lock_table_mutex);
    }
  
  while (TRUE)
    {
      gboolean should_continue = TRUE;
      gboolean acquired_lock = FALSE;
      
      gint64 now = g_get_monotonic_time ();
      
      if (now >= dataclient_tx->end_time)
	{
	  /* We may have run out of time in the current transaction. */
	  
	  should_continue = FALSE;
	  
	  tx->rollback = TRUE;
	  tx->should_retry = TRUE;

	  g_mutex_unlock (&lock_request->mutex);
	}
      else
	{
	  if (!lock_request->requested)
	    {
	      /* If it's appropriate to send a request for the lock to the meta
		 server, and that hasn't happened yet, do so now. */
	      
	      if (now >= lock_request->next_request_time)
		{
		  dataclient_callback_data *callback_data =
		    create_callback_data
		    (store, lock_request->key->key, lock_request->for_write);

		  gzochid_dataclient_request_value
		    (environment->client, environment->app_name, database->name,
		     lock_request->key->key, lock_request->for_write,
		     lock_success_callback, callback_data,
		     lock_failure_callback, callback_data,
		     lock_release_callback, callback_data);
		  
		  lock_request->requested = TRUE;
		}
	      else
		{
		  /* If the request can't yet be sent, wait until it is - but no
		     longer than the current transaction duration. TODO: Short-
		     circuit the transaction if the wait time is longer than the
		     available time remaining. */
		  
		  g_cond_wait_until
		    (&lock_request->cond, &lock_request->mutex,
		     MIN (lock_request->next_request_time,
			  dataclient_tx->end_time));
		  
		  continue;
		}
	    }

	  /* Wait for a response to the request... */
	  
	  g_cond_wait_until
	    (&lock_request->cond, &lock_request->mutex,
	     dataclient_tx->end_time);

	  /* The only real purpose of the lock request mutex is to synchronize
	     the condition waiting above. So it's safe to release this here and
	     grab it again below if we need to re-enter the loop. */
	  
	  g_mutex_unlock (&lock_request->mutex);

	  /* ...and see if it was successful. */

	  g_mutex_lock (&environment->lock_table_mutex);	  
	  acquired_lock = check_and_set_lock
	    (environment, dataclient_tx, &qualified_key,
	     lock_request->for_write);
	  g_mutex_unlock (&environment->lock_table_mutex);
	}
      
      if (acquired_lock || !should_continue)
	{
	  gboolean for_write = lock_request->for_write;

	  /* Need to seize the environment's mutex because we're going to be
	     manipulating the eviction list. */
	  
	  g_mutex_lock (&environment->mutex);
	  g_mutex_lock (&environment->lock_table_mutex);

	  /* Whether or not the attempt to obtain the lock was successful or 
	     not, we need to relinquish our hold on the lock request. */
	  
	  if (lock_request_unref (lock_request))
	    {
	      /* ...and if we were the last interested party, clean it up. */
	      
	      GHashTable *table = for_write
		? environment->write_lock_requests
		: environment->read_lock_requests;

	      g_hash_table_remove (table, &qualified_key);

	      evicted_key = find_evicted_key (environment, &qualified_key);

	      if (evicted_key != NULL)
		evicted_key->lock_request = NULL;
	    }

	  g_bytes_unref (key_bytes);
	  g_mutex_unlock (&environment->lock_table_mutex);
	  g_mutex_unlock (&environment->mutex);

	  return acquired_lock;
	}
      else g_mutex_lock (&lock_request->mutex);
    }
}

/* The "success" callback for range lock requests. Adds the lock to the store's
   range lock table and notifies any waiting transaction threads so that they 
   can add the lock to their local range lock tables. */

static void
range_lock_success_callback (GBytes *key, gpointer user_data)
{
  dataclient_callback_data *callback_data = user_data;
  dataclient_database *database = callback_data->store->database;
  dataclient_range_lock_search_context search_context;
  dataclient_range_lock_request_search_context request_search_context;

  search_context.store = database->name;
  search_context.from = callback_data->key;
  search_context.to = key;
  search_context.range_lock = NULL;
  
  g_mutex_lock (&database->mutex);

  gzochid_itree_search_interval
    (database->range_locks, callback_data->key, key, find_exact_range_lock,
     &search_context);
  
  if (search_context.range_lock == NULL)
    {
      dataclient_qualified_key_range kr = (dataclient_qualified_key_range)
	{ database->name, callback_data->key, key };
      
      gzochid_itree_insert
	(database->range_locks, callback_data->key, key, range_lock_new (&kr));
    }

  request_search_context.store = database->name;
  request_search_context.from = callback_data->key;
  request_search_context.to = NULL;
  request_search_context.range_lock_request = NULL;

  gzochid_itree_search_interval
    (database->range_lock_requests, callback_data->key, NULL,
     find_exact_range_lock_request, &request_search_context);

  if (request_search_context.range_lock_request != NULL)
    {
      g_mutex_lock (&request_search_context.range_lock_request->mutex);
      g_cond_broadcast (&request_search_context.range_lock_request->cond);
      g_mutex_unlock (&request_search_context.range_lock_request->mutex);
    }

  gzochid_itree_remove
    (database->range_lock_requests, callback_data->key, NULL);

  g_mutex_unlock (&database->mutex);
}

/* The "failure" callback for range lock requests. Resets the request state of
   pending range lock request objects (if they exist) and wakes up any 
   transaction threads waiting on the request so that they can handle a retry
   after the wait time expires. */

static void
range_lock_failure_callback (struct timeval wait_time, gpointer user_data)
{
  dataclient_callback_data *callback_data = user_data;
  dataclient_database *database = callback_data->store->database;
  dataclient_range_lock_request_search_context search_context;

  search_context.store = database->name;
  search_context.from = callback_data->key;
  search_context.to = NULL;
  search_context.range_lock_request = NULL;

  gzochid_itree_search_interval
    (database->range_lock_requests, callback_data->key, NULL,
     find_exact_range_lock_request, &search_context);

  /* There may be no lock request if there are no longer any threads waiting
     for the lock. */

  if (search_context.range_lock_request != NULL)
    {
      g_mutex_lock (&search_context.range_lock_request->mutex);

      search_context.range_lock_request->requested = FALSE;
      search_context.range_lock_request
	->next_request_time = g_get_monotonic_time ()
	+ wait_time.tv_sec * 1000
	+ wait_time.tv_usec / 1000;
      
      g_cond_broadcast (&search_context.range_lock_request->cond);
      g_mutex_unlock (&search_context.range_lock_request->mutex);
    }

  callback_data_free (callback_data);
}

/* A `gzochid_itree_search_func' that returns `TRUE' on the first range lock it
   finds that begins with the same key as the `from' field in the 
   `dataclient_range_lock_search_context' struct passed via the user data 
   pointer. */

static gboolean
find_range_lock_from (gpointer from, gpointer to, gpointer data,
		      gpointer user_data)
{
  dataclient_range_lock_search_context *search_context = user_data;

  if ((from == NULL && search_context->from == NULL)
      || g_bytes_equal (from, search_context->from))
    {
      search_context->range_lock = data;
      return TRUE;
    }
  else return FALSE;
}

/* The "release" callback for range locks. Removes a reference to the range 
   lock and potentially adds the target key range to the eviction list. */

static void
range_lock_release_callback (gpointer user_data)
{
  dataclient_callback_data *callback_data = user_data;
  gzochid_storage_store *store = callback_data->store;
  dataclient_environment *environment = store->context->environment;
  dataclient_database *database = store->database;
  dataclient_range_lock_search_context search_context =
    (dataclient_range_lock_search_context)
    { database->name, callback_data->key, NULL, NULL };

  /* Grab the environment's mutex before the store's mutex; important to always
     take these in the same order. */

  g_mutex_lock (&environment->mutex);
  g_mutex_lock (&database->mutex);

  /* This is a little hacky, but it lets us identify the actual range lock being
     released (if any) under the assumption that in practice as a result of 
     requesting "first key" or "next key" there can't be more than one range
     lock with the same `from' key. */
  
  gzochid_itree_search_interval
    (database->range_locks, callback_data->key, NULL, find_range_lock_from,
     &search_context); 

  if (search_context.range_lock != NULL)
    {
      /* We need to seize a references to the `from' key in the range (if it's 
	 non-`NULL') because after unref'ing the lock below, its reference count
	 will be decremented. */
      
      GBytes *from = search_context.range_lock->key_range->from == NULL
	? NULL : g_bytes_ref (search_context.range_lock->key_range->from);

      gzochid_itree_remove
	(database->range_locks, from, search_context.range_lock->key_range->to);
      
      /* Are there additional references to this key range? I.e., are there any 
	 active transactions still using it? */

      if (! range_lock_unref
	  (store->context->environment, search_context.range_lock))

	/* If so, add the key range (or, at least, the `from' key) to the list 
	   of in-progress evictions.  */

	environment->evicted_key_ranges = g_list_prepend
	  (environment->evicted_key_ranges,
	   dataclient_evicted_key_range_new (database->name, from, NULL));

      /* No longer needs to be kept alive here; the eviction range struct (if we
	 created one) will take care of that. */
      
      if (from != NULL)
	g_bytes_unref (from);
    }
  
  callback_data_free (callback_data);

  g_mutex_unlock (&database->mutex);
  g_mutex_unlock (&environment->mutex);
}

/*
  If the store holds a range lock on the specified key range, add that lock to 
  the specified transaction's set of local range locks, and obtain a 
  non-exclusive read lock on it. 

  To ensure that this operation is properly synchronous, the store's lock table
  mutex must be held by the caller of this function.
*/

static inline gboolean
check_and_set_range_lock (dataclient_transaction *tx,
			  dataclient_database *database, GBytes *from)
{
  dataclient_range_lock_search_context search_context;

  search_context.store = database->name;
  search_context.from = from;
  search_context.to = NULL;
  search_context.range_lock = NULL;
  
  gzochid_itree_search_interval
    (database->range_locks, from, NULL, find_range_lock_covering_point,
     &search_context);
      
  if (search_context.range_lock != NULL)
    {
      /* If everything is properly synchronized, it should be impossible for 
	 this block to be entered while a lock is being reaped. */	     

      gzochid_itree_insert
	(tx->range_locks, search_context.range_lock->key_range->from,
	 search_context.range_lock->key_range->to,
	 range_lock_ref (search_context.range_lock));
      
      return TRUE;
    }

  return FALSE;
}

/*
  Increases the reference count of the specified range lock request and returns
  it. 

  For safety's sake, the mutex of the store that owns the range lock request 
  should be held by the caller of this function during the lookup of the request
  and for the duration of this function.
*/

static dataclient_range_lock_request *
range_lock_request_ref (dataclient_range_lock_request *range_lock_request)
{
  range_lock_request->ref_count++;
  return range_lock_request;
}

/*
  Decreases the reference count of the specified range lock request. If the 
  reference count reaches zero, the lock request is freed.

  For safety's sake, the mutex of the store that owns the range lock request 
  should be held by the caller of this function during the lookup of the 
  request, for the duration of this function, and for the removal of the 
  request - if appropriate - from the range lock request table.
*/

static gboolean
range_lock_request_unref (dataclient_range_lock_request *range_lock_request)
{
  /* Decrement the reference count before testing the value. */

  gboolean ret = --range_lock_request->ref_count <= 0;

  if (ret)    
    {
      dataclient_qualified_key_range_free (range_lock_request->key_range);

      g_mutex_clear (&range_lock_request->mutex);
      g_cond_clear (&range_lock_request->cond);
      
      free (range_lock_request);
    }

  return ret;
}

/* Create and return a new range lock request object for the specified store and
   key range. The "from" and "to" keys may be `NULL' to indicate the beginning 
   or end of the key space, respectively. The returned request will have an 
   initial reference count of 1. */

static dataclient_range_lock_request *
range_lock_request_new (const char *store, GBytes *from, GBytes *to)
{
  dataclient_range_lock_request *range_lock_request =
    malloc (sizeof (dataclient_range_lock_request));

  range_lock_request->key_range =
    dataclient_qualified_key_range_new (store, from, to);

  range_lock_request->requested = FALSE;
  range_lock_request->next_request_time = 0;
  
  g_mutex_init (&range_lock_request->mutex);
  g_cond_init (&range_lock_request->cond);

  range_lock_request->ref_count = 1;
  
  return range_lock_request;
}

/* Ensure that the specified transaction holds a range lock on the part of the 
   key space in the specified store that begins with the specified key - which
   may be `NULL' to indicate the beginning of the key space. If a matching range
   lock is not already held by the transaction, an attempt is made as follows to
   obtain it:

   1. If the store proxy on this application server contains a matching lock,
      add it to the transaction's set of locks.
   2. Otherwise, if an attempt to obtain a matching lock is already in progress,
      register to receive notifications on the outcome of that attempt, and 
      wait. If the lock is successfully obtained, add it to the transaction; if
      the attempt fails, wait out the timeout interval and repeat.
   3. Otherwise, send a request to the meta server for that lock, then wait, as
      described above.

   Returns `TRUE' if the lock was already held or could be obtained within the
   lifespan of hte current transaction, `FALSE' otherwise.
*/

static gboolean
ensure_range_lock (gzochid_storage_transaction *tx,
		   gzochid_storage_store *store, char *key, size_t key_len)
{
  dataclient_environment *environment = store->context->environment;
  dataclient_database *database = store->database;
  dataclient_transaction *dataclient_tx = tx->txn;

  dataclient_range_lock_search_context search_context;
  dataclient_range_lock_request_search_context request_search_context;

  GBytes *key_bytes = key == NULL ? NULL : g_bytes_new (key, key_len);

  dataclient_qualified_key_range qualified_key_range =
    (dataclient_qualified_key_range) { database->name, key_bytes, NULL };
  dataclient_evicted_key_range *evicted_key_range = NULL;
  dataclient_range_lock_request *range_lock_request = NULL;

  search_context.store = database->name;
  search_context.from = key_bytes;
  search_context.to = NULL;
  search_context.range_lock = NULL;
  
  gzochid_itree_search_interval
    (dataclient_tx->range_locks, key_bytes, NULL,
     find_range_lock_covering_point, &search_context);

  /* Doees the transaction already hold a matching range lock? */

  if (search_context.range_lock != NULL)
    {
      if (key_bytes != NULL)
	g_bytes_unref (key_bytes);
      
      return TRUE;
    }
  
  g_mutex_lock (&environment->mutex);
  g_mutex_lock (&database->mutex);
  
  /* Make a synchronous attempt to seize a reference to the local cache / 
     proxy's instance of the range lock, if it exists. */

  if (check_and_set_range_lock (dataclient_tx, database, key_bytes))
    {
      if (key_bytes != NULL)
	g_bytes_unref (key_bytes);
      
      g_mutex_unlock (&database->mutex);
      g_mutex_unlock (&environment->mutex);
      return TRUE;
    }

  /* Is there a range lock request in progress? It could be attackedt o a key
     range in the eviction list, so check there first. */
  
  evicted_key_range = find_evicted_key_range
    (environment, &qualified_key_range);
    
  if (evicted_key_range != NULL)
    {
      if (evicted_key_range->range_lock_request == NULL)
	{
	  /* The key range is being evicted, but no one else has requested it.
	     Create a new range lock request and add it to the eviction record
	     and also insert it into the store's range lock request tree. */
	  
	  range_lock_request = range_lock_request_new
	    (database->name, key_bytes, NULL);

	  /* Can't be requested yet because it's in the eviction list. */
	  
	  range_lock_request->next_request_time = G_MAXINT64;
	  
	  evicted_key_range->range_lock_request = range_lock_request;
	 
	  gzochid_itree_insert
	    (database->range_lock_requests, key_bytes, NULL,
	     range_lock_request);

	  g_mutex_lock (&range_lock_request->mutex);
	}
      else
	{
	  /* The request must have already been added to the store's request
	     tree, so all we need to do here is increase its reference count. */
	  
	  g_mutex_lock (&evicted_key_range->range_lock_request->mutex);
	  range_lock_request = range_lock_request_ref
	    (evicted_key_range->range_lock_request);
	}

      /* Once the request mutex is held it's safe to release these. */
      
      g_mutex_unlock (&database->mutex);
      g_mutex_unlock (&environment->mutex);
    }
  else
    {
      /* Release the environment mutex, since we're done with the eviction 
	 list. */
      
      g_mutex_unlock (&environment->mutex);
      
      request_search_context.store = database->name;
      request_search_context.from = key_bytes;
      request_search_context.to = NULL;
      request_search_context.range_lock_request = NULL;
      
      gzochid_itree_search
	(database->range_lock_requests, key, find_covering_range_lock_request,
	 &request_search_context);
  
      /* Is there a range lock request in the store's request tree? */

      if (request_search_context.range_lock_request != NULL)
	{
	  g_mutex_lock (&request_search_context.range_lock_request->mutex);
	  range_lock_request_ref (request_search_context.range_lock_request);
	  g_mutex_unlock (&database->mutex);
	}
      else 
	{
	  /* If not, create one. */
	  
	  range_lock_request = range_lock_request_new
	    (database->name, key_bytes, NULL);
	  
	  gzochid_itree_insert
	    (database->range_lock_requests, key_bytes, NULL,
	     range_lock_request);
	  
	  g_mutex_lock (&range_lock_request->mutex);
	  g_mutex_unlock (&database->mutex);
	}
    }

  while (TRUE)
    {
      gboolean should_continue = TRUE;
      gboolean acquired_lock = FALSE;

      gint64 now = g_get_monotonic_time ();

      if (now >= dataclient_tx->end_time)
	{
	  /* We may have run out of time in the current transaction. */

	  should_continue = FALSE;

	  tx->rollback = TRUE;
	  tx->should_retry = TRUE;
	}
      else
	{
	  if (!range_lock_request->requested)
	    {
	      /* If it's appropriate to send a request for the range lock to the
		 meta server, and that hasn't happened yet, do so now. */

	      if (now >= range_lock_request->next_request_time)
		{
		  dataclient_environment *environment =
		    store->context->environment;
		  dataclient_callback_data *callback_data = create_callback_data
		    (store, range_lock_request->key_range->from, FALSE);
      
		  gzochid_dataclient_request_next_key
		    (environment->client, environment->app_name, database->name,
		     range_lock_request->key_range->from,
		     range_lock_success_callback, callback_data,
		     range_lock_failure_callback, callback_data,
		     range_lock_release_callback, callback_data);
		  
		  range_lock_request->requested = TRUE;
		}
	      else
		{
		  /* If the request can't yet be sent, wait until it is - but no
		     longer than the current transaction duration. TODO: Short-
		     circuit the transaction if the wait time is longer than the
		     available time remaining. */

		  g_cond_wait_until
		    (&range_lock_request->cond, &range_lock_request->mutex,
		     MIN (range_lock_request->next_request_time,
			  dataclient_tx->end_time));

		  continue;
		}
	    }
      
	  /* Wait for a response to the request... */

	  g_cond_wait_until
	    (&range_lock_request->cond, &range_lock_request->mutex,
	     dataclient_tx->end_time);
	  
	  /* ...and see if it was successful. */

	  g_mutex_lock (&database->mutex);
	  acquired_lock = check_and_set_range_lock
	    (dataclient_tx, database, range_lock_request->key_range->from);
	  g_mutex_unlock (&database->mutex);
	}
      
      if (acquired_lock || !should_continue)
	{
	  GBytes *from = range_lock_request->key_range->from == NULL
	    ? NULL : g_bytes_ref (range_lock_request->key_range->from);
	  GBytes *to = range_lock_request->key_range->to == NULL
	    ? NULL : g_bytes_ref (range_lock_request->key_range->to);

	  /* Need to seize the environment's mutex because we're going to be
	     manipulating the eviction list. */
	  
	  g_mutex_lock (&environment->mutex);
	  g_mutex_lock (&database->mutex);
	  g_mutex_unlock (&range_lock_request->mutex);
	  
	  /* Whether or not the attempt to obtain the range lock was successful
	     or not, we need to relinquish our hold on the range lock 
	     request. */

	  if (range_lock_request_unref (range_lock_request))
	    {
	      /* ...and if we were the last interested party, clean it up. */
	    
	      gzochid_itree_remove (database->range_lock_requests, from, to);

	      evicted_key_range = find_evicted_key_range
		(environment, &qualified_key_range);

	      if (evicted_key_range != NULL)
		evicted_key_range->range_lock_request = NULL;
	    }

	  if (from != NULL)
	    g_bytes_unref (from);
	  if (to != NULL)
	    g_bytes_unref (to);
	  if (key_bytes != NULL)
	    g_bytes_unref (key_bytes);
	  
	  g_mutex_unlock (&database->mutex);
	  g_mutex_unlock (&environment->mutex);
	  return acquired_lock;
	}
    }
}

/* The functions below implement the gzochid storage engine interface (as
   defined in `gzochid-storage.h') in terms of the client lock caching system 
   defined above. */

/* Create and return a new storage context in the form of a dataclient 
   environment. The "basename" of the specified `path' argument is used as the
   application name that is used in communications with the meta server. */

static gzochid_storage_context *
initialize (char *path)
{
  gzochid_storage_context *context = malloc (sizeof (gzochid_storage_context));

  dataclient_environment *environment =
    calloc (1, sizeof (dataclient_environment));

  environment->app_name = g_path_get_basename (path);
  environment->delegate_iface = &gzochid_storage_engine_interface_mem;
  environment->delegate_context =
    environment->delegate_iface->initialize (path);

  g_mutex_init (&environment->mutex);
  g_mutex_init (&environment->lock_table_mutex);
  
  /* The lock table. */

  environment->locks = g_hash_table_new_full
    (dataclient_qualified_key_hash, dataclient_qualified_key_equal,
     dataclient_qualified_key_free, NULL);

  /* The lock request tables. */
  
  environment->read_lock_requests = g_hash_table_new_full
    (dataclient_qualified_key_hash, dataclient_qualified_key_equal,
     dataclient_qualified_key_free, NULL);
  environment->write_lock_requests = g_hash_table_new_full
    (dataclient_qualified_key_hash, dataclient_qualified_key_equal,
     dataclient_qualified_key_free, NULL);

  environment->value_cache = g_hash_table_new_full
    (dataclient_qualified_key_hash, dataclient_qualified_key_equal,
     dataclient_qualified_key_free, (GDestroyNotify) g_bytes_unref);

  context->environment = environment;
  
  return context;
}

/* Close and clean up the specified storage context (and its associated
   dataclient environment). */

static void
close_context (gzochid_storage_context *context)
{
  dataclient_environment *environment = context->environment;
  environment->delegate_iface->close_context (environment->delegate_context);

  if (environment->client != NULL)
    g_object_unref (environment->client);
  
  g_free (environment->app_name);

  g_hash_table_destroy (environment->locks);
  g_hash_table_destroy (environment->read_lock_requests);
  g_hash_table_destroy (environment->write_lock_requests);
  
  g_hash_table_destroy (environment->value_cache);

  free (context->environment);  
  free (context);
}

/* This function is a no-op. */

static void
destroy_context (char *path)
{
}

/* Create and return a new store that proxies a persistent store on the meta 
   server, caching data locally in a delegate B+tree-based store. The "basename"
   of the specified path is used to identify the store in communication with the
   meta server (and consequently must be "names" or "oids"). The `flags' 
   arguments is ignored. */

static gzochid_storage_store *
open (gzochid_storage_context *context, char *path, unsigned int flags)
{
  dataclient_environment *environment = context->environment;
  gzochid_storage_store *store = malloc (sizeof (gzochid_storage_store));
  dataclient_database *database = malloc (sizeof (dataclient_database));

  database->name = g_path_get_basename (path);

  /* The range lock table. */

  database->range_locks = gzochid_itree_new
    (gzochid_util_bytes_compare_null_first,
     gzochid_util_bytes_compare_null_last);

  /* The range lock request table. */
  
  database->range_lock_requests = gzochid_itree_new
    (gzochid_util_bytes_compare_null_first,
     gzochid_util_bytes_compare_null_last);

  g_mutex_init (&database->mutex);

  /* The delegate B+tree store. */
  
  database->delegate_store = environment->delegate_iface->open
    (environment->delegate_context, path, flags);
  
  store->context = context;
  store->database = database;

  return store;
}

/* A `gzochid_itree_search_func' that frees every range lock it encounters, as a
   means of cleaning up the range lock table. */

static gboolean
free_range_lock_visitor (gpointer from, gpointer to, gpointer data,
			 gpointer user_data)
{
  free_range_lock (data);
  return FALSE;
}

/* Close and clean up the specified store (and its delegate B+tree store). */

static void
close_store (gzochid_storage_store *store)
{
  dataclient_environment *environment = store->context->environment;
  dataclient_database *database = store->database;

  free (database->name);

  /* Walk the interval tree and free any of the range locks. */
  
  gzochid_itree_search_interval
    (database->range_locks, NULL, NULL, free_range_lock_visitor, NULL);
  gzochid_itree_free (database->range_locks);
  
  gzochid_itree_free (database->range_lock_requests);  
  
  g_mutex_clear (&database->mutex);

  /* Let the delegate B+tree interface clean up the B+tree. */
  
  environment->delegate_iface->close_store (database->delegate_store);

  free (database);
  free (store);
}

/* This function merely delegates to the B+tree store's `destroy' function. */

static void
destroy_store (gzochid_storage_context *context, char *name)
{
  dataclient_environment *environment = context->environment;
  environment->delegate_iface->destroy_store
    (environment->delegate_context, name);
}

/*
  A `GDestroyNotify' implementation to clear out an element in a `GArray' of
  `gzochid_data_change' elements. 

  Note that because this is used with `g_array_free' this function does not 
  free the `gzochid_data_change' struct itself.
*/

static void
clear_changeset (gpointer data)
{
  gzochid_data_change *change = data;

  free (change->store);
  g_bytes_unref (change->key);

  if (change->data != NULL)
    g_bytes_unref (change->data);
}

/* Create and return a new transaction over the specified dataclient 
   environment. The transaction will not attempt to acquire any locks after the
   specified monotonic timestamp has elapsed. */

static gzochid_storage_transaction *
create_transaction (gzochid_storage_context *context,
		    gzochid_storage_transaction *delegate_tx, gint64 end_time)
{
  gzochid_storage_transaction *tx =
    calloc (1, sizeof (gzochid_storage_transaction));
  dataclient_transaction *txn = calloc (1, sizeof (dataclient_transaction));

  txn->end_time = end_time;
  txn->delegate_tx = delegate_tx;

  /* Create the table of read and write locks; may include locks from multiple 
     stores. */
  
  txn->locks = g_hash_table_new_full
    (dataclient_qualified_key_hash, dataclient_qualified_key_equal,
     dataclient_qualified_key_free, NULL);

  /* Create the table of range locks; may include locks from multiple stores. */

  txn->range_locks = gzochid_itree_new
    (gzochid_util_bytes_compare_null_first,
     gzochid_util_bytes_compare_null_last);

  txn->changeset = g_array_new (FALSE, FALSE, sizeof (gzochid_data_change));
  g_array_set_clear_func (txn->changeset, clear_changeset);
  
  tx->context = context;
  tx->txn = txn;
  
  return tx;
}

/* Creates and returns a new transaction in the specified storage context, with
   a timeout equivalent to 2^64 - 1. */

static gzochid_storage_transaction *
transaction_begin (gzochid_storage_context *context)
{
  dataclient_environment *environment = context->environment;
  gzochid_storage_transaction *delegate_tx = environment->delegate_iface
    ->transaction_begin (environment->delegate_context);

  return create_transaction (context, delegate_tx, G_MAXINT64);
}

/* Creates and returns a new transaction with the specified timeout in the 
   specified storage context. */

static gzochid_storage_transaction *
transaction_begin_timed (gzochid_storage_context *context,
			 struct timeval timeout)
{
  dataclient_environment *environment = context->environment;
  gzochid_storage_transaction *delegate_tx = environment->delegate_iface
    ->transaction_begin_timed (environment->delegate_context, timeout);

  gint64 now = g_get_monotonic_time ();
  gint64 duration_usec = timeout.tv_sec * 1000000 + timeout.tv_usec;

  return create_transaction (context, delegate_tx, now + duration_usec);
}

/* A `GHFunc' implementation for use with `GHashTables' whose values are 
   `dataclient_lock' objects; calls `lock_unref' on the value, using the
   `dataclient_environment' from the composite user data argument as context;
   the keys for released locks are added to the user data `GPtrArray' array. */

static void
lock_unref_ghfunc (gpointer key, gpointer value, gpointer user_data)
{
  gpointer *args = user_data;
  dataclient_environment *environment = args[0];
  GPtrArray *released_keys = args[1];
  
  if (lock_unref (environment, value))
    g_ptr_array_add (released_keys, key);
}

/*
  A `gzochid_itree_search_func' implementation for use with `gzochid_itrees'
  whose values are `dataclient_range_lock' objects; calls `range_lock_unref'
  on the value, passing the user data argument as the `dataclient_environment'
  to which the range lock belongs. 
  
  Intended for use in clearing the entire interval tree; always returns `FALSE'
  to continue the traversal.
*/

static gboolean
range_lock_unref_search_func (gpointer from, gpointer to, gpointer data,
			      gpointer user_data)
{
  range_lock_unref (user_data, data);
  return FALSE;
}

/* Frees the resources allocated for the specified transaction and removes it
   from the enclosing dataclient environment. */

static void
cleanup_transaction (gzochid_storage_transaction *tx)
{
  dataclient_transaction *txn = tx->txn;
  dataclient_environment *env = tx->context->environment;
  GPtrArray *released_keys = g_ptr_array_new ();
  gpointer args[2] = { env, released_keys };
  gint i = 0;

  /* Unref all the locks in the table. Need to pass the environment as context,
     so this can't be done as a `GDestroyNotify' on the value. */

  g_mutex_lock (&env->lock_table_mutex);
  g_hash_table_foreach (txn->locks, lock_unref_ghfunc, args);

  for (; i < released_keys->len; i++)
    {
      dataclient_qualified_key *qualified_key =
	g_ptr_array_index (released_keys, i);
      g_hash_table_remove (env->locks, qualified_key);
    }

  g_ptr_array_unref (released_keys);
  g_mutex_unlock (&env->lock_table_mutex);
  
  g_hash_table_destroy (txn->locks);

  /* Unref all the range locks in the tree. */
  
  gzochid_itree_search_interval
    (txn->range_locks, NULL, NULL, range_lock_unref_search_func, env);
  gzochid_itree_free (txn->range_locks);
  
  g_array_free (txn->changeset, TRUE);
  
  g_list_free_full (txn->deleted_keys, (GDestroyNotify) callback_data_free);

  free (txn);
  free (tx);
}

/*
  Make "permanent" all of the modifications performed in the specified 
  transaction, across the client side of all stores in the environment to which
  the transaction belongs. The set of modifications will be enqueued for 
  transmission to the meta server, where they will be durably persisted as an
  atomic unit; until they are persisted by the meta server, the transaction's
  changes are not guaranteed to survive a failure of all or part of the cluster.
  
  When this function returns, all read and write locks held by the transaction
  will have been released, and the transaction itself will have been cleaned 
  up; its handle should not be used again.
*/

static void
transaction_commit (gzochid_storage_transaction *tx)
{
  dataclient_transaction *txn = tx->txn;
  dataclient_environment *env = tx->context->environment;
  GList *deleted_key_ptr = txn->deleted_keys;

  env->delegate_iface->transaction_commit (txn->delegate_tx);

  if (txn->changeset->len > 0)
    gzochid_dataclient_submit_changeset
      (env->client, env->app_name, txn->changeset);

  /* For each key deleted in this transacton, remove it from the cache. */

  g_mutex_lock (&env->lock_table_mutex);
  
  while (deleted_key_ptr != NULL)
    {
      dataclient_callback_data *key = deleted_key_ptr->data;
      dataclient_database *database = key->store->database;
      dataclient_qualified_key qualified_key = { database->name, key->key };
      
      g_hash_table_remove (env->value_cache, &qualified_key);
      deleted_key_ptr = deleted_key_ptr->next;
    }

  g_mutex_unlock (&env->lock_table_mutex);
  cleanup_transaction (tx);
}

/*
  Undo all of the modifications performed in the specified transaction, across
  the client side of all stores in the environment to which the transaction 
  belongs.
  
  When this function returns, all read and write locks held by the transaction
  will have been released, and the transaction itself will have been cleaned 
  up; its handle should not be used again.
*/

static void
transaction_rollback (gzochid_storage_transaction *tx)
{
  dataclient_transaction *txn = tx->txn;
  dataclient_environment *env = tx->context->environment;

  env->delegate_iface->transaction_rollback (txn->delegate_tx);
  cleanup_transaction (tx);
}

/* This function merely delegates to the B+tree store's `prepare' function. */

static void
transaction_prepare (gzochid_storage_transaction *tx)
{
  dataclient_transaction *txn = tx->txn;
  dataclient_environment *env = tx->context->environment;

  env->delegate_iface->transaction_prepare (txn->delegate_tx);
}

/* If the specified transaction's inner B+tree transaction has been marked for
   rollback, transfer its status to the outer transaction; otherwise this 
   function is a no-op. */

static void
propagate_transaction_status (gzochid_storage_transaction *tx)
{
  dataclient_transaction *dataclient_tx = tx->txn;
  gzochid_storage_transaction *delegate_tx = dataclient_tx->delegate_tx;
  
  if (!tx->rollback)
    {
      tx->rollback = delegate_tx->rollback;
      tx->should_retry = delegate_tx->should_retry;
    }
}

/* A `GCompareFunc' implementation for `dataclient_callback_data'. */

static gint
callback_data_compare (gconstpointer a, gconstpointer b)
{
  const dataclient_callback_data *cba = a;
  const dataclient_callback_data *cbb = b;

  if (cba->store == cbb->store)
    return g_bytes_compare (cba->key, cbb->key);
  else return -1;
}

/* Returns `TRUE' if the specified key has been deleted from the target store in
   the specified transaction, `FALSE' otherwise. */

static gboolean
is_deleted_bytes (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		  GBytes *key_bytes)
{
  dataclient_transaction *dtx = tx->txn;
  dataclient_callback_data cb = (dataclient_callback_data)
    { store, key_bytes, TRUE };
  
  return g_list_find_custom
    (dtx->deleted_keys, &cb, callback_data_compare) != NULL;
}

/* A variation on `is_deleted_bytes' that works on an `unsigned char *' and an 
   explicit length argument. */

static gboolean
is_deleted (gzochid_storage_transaction *tx, gzochid_storage_store *store,
	    char *key, size_t key_len)
{
  GBytes *key_bytes = g_bytes_new_static (key, key_len);
  gboolean ret = is_deleted_bytes (tx, store, key_bytes);
  
  g_bytes_unref (key_bytes);
  return ret;
}

/*
  Retrieves the value (if any) bound to the specified key in the dataclient
  cache for the target environment.
  
  Requires that the caller hold the environment's lock table mutex.
*/

static unsigned char *
get_from_cache (dataclient_environment *environment,
		dataclient_qualified_key *qualified_key, size_t *value_len)
{
  unsigned char *ret = NULL;
  GBytes *value = g_hash_table_lookup
    (environment->value_cache, qualified_key);
  
  if (value != NULL)
    {
      size_t tmp_len = 0;
      gconstpointer value_data = g_bytes_get_data (value, &tmp_len);
      
      ret = malloc (sizeof (unsigned char) * tmp_len);
      memcpy (ret, value_data, tmp_len);
      
      if (value_len != NULL)
	*value_len = tmp_len;
    }

  return ret;
}

/* Returns the value for the specified key or `NULL' if none exists. */

static char *
transaction_get (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, size_t *value_len)
{  
  if (ensure_lock (tx, store, key, key_len, FALSE))
    {
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_environment *environment = tx->context->environment;
      dataclient_database *database = store->database;
      
      char *ret = environment->delegate_iface->transaction_get
	(dataclient_tx->delegate_tx, database->delegate_store, key, key_len,
	 value_len);

      propagate_transaction_status (tx);

      /* Didn't find a value in the mem store? (And we haven't already deleted 
	 it in this transaction?) Check the cache. */

      if (ret == NULL && !tx->rollback && !is_deleted (tx, store, key, key_len))
	{
	  GBytes *key_bytes = g_bytes_new_static (key, key_len);
	  dataclient_qualified_key qualified_key =
	    { database->name, key_bytes };
	  
	  ret = (char *) get_from_cache
	    (environment, &qualified_key, value_len);

	  g_bytes_unref (key_bytes);
	}

      return ret;
    }
  else return NULL;
}

/* Returns the value for the specified key or `NULL' if none exists. If the key
   is found, a write lock is established on it before this function returns. */

static char *
transaction_get_for_update (gzochid_storage_transaction *tx,
			    gzochid_storage_store *store, char *key,
			    size_t key_len, size_t *value_len)
{  
  if (ensure_lock (tx, store, key, key_len, TRUE))
    {
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_environment *environment = tx->context->environment;
      dataclient_database *database = store->database;
      
      char *ret = environment->delegate_iface->transaction_get_for_update
	(dataclient_tx->delegate_tx, database->delegate_store, key, key_len,
	 value_len);

      propagate_transaction_status (tx);

      /* Didn't find a value in the mem store? (And we haven't already deleted 
	 it in this transaction?) Check the cache. */
      
      if (ret == NULL && !tx->rollback && !is_deleted (tx, store, key, key_len))
	{
	  GBytes *key_bytes = g_bytes_new_static (key, key_len);
	  dataclient_qualified_key qualified_key =
	    { database->name, key_bytes };
	  
	  ret = (char *) get_from_cache
	    (environment, &qualified_key, value_len);

	  g_bytes_unref (key_bytes);
	}
      
      return ret;
    }
  else return NULL;
}

/* Inserts or updates the value for the specified key. */

static void
transaction_put (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, char *value, size_t value_len)
{
  if (ensure_lock (tx, store, key, key_len, TRUE))
    {
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_environment *environment = tx->context->environment;
      dataclient_database *database = store->database;

      gzochid_data_change change;
      
      environment->delegate_iface->transaction_put
	(dataclient_tx->delegate_tx, database->delegate_store, key, key_len,
	 value, value_len);

      propagate_transaction_status (tx);

      if (!tx->rollback)
	{
	  /* Enqueue the change for persistence on the meta server. */
	  
	  change.store = strdup (database->name);
	  change.delete = FALSE;
	  change.key = g_bytes_new (key, key_len);
	  change.data = g_bytes_new (value, value_len);
      
	  g_array_append_val (dataclient_tx->changeset, change);
	}
    }
}

/* Deletes the value for the specified key. Returns ENOTFOUND if no such value
   exists; ETXFAILURE if a lock could not be acquired; 0 otherwise. */

static int
transaction_delete (gzochid_storage_transaction *tx,
		    gzochid_storage_store *store, char *key, size_t key_len)
{
  if (ensure_lock (tx, store, key, key_len, TRUE))
    {
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_environment *environment = tx->context->environment;
      dataclient_database *database = store->database;

      gboolean ret = environment->delegate_iface->transaction_delete
	(dataclient_tx->delegate_tx, database->delegate_store, key, key_len);

      /* If the value wasn't found in the delegate store, that could be because
	 it's been fetched but not modified. In that case, it'll be in the value
	 cache. We'll need to check the deletion list to ensure that it hasn't
	 already been deleted in this transaction. */
      
      if (ret == GZOCHID_STORAGE_ENOTFOUND)
	{
	  GBytes *key_bytes = g_bytes_new_static (key, key_len);
	  dataclient_qualified_key qualified_key =
	    { database->name, key_bytes };
	  
	  if (g_hash_table_contains (environment->value_cache, &qualified_key)
	      && !is_deleted_bytes (tx, store, key_bytes))
	    ret = 0;

	  g_bytes_unref (key_bytes);
	}
      
      if (ret == 0)
	{
	  /* Enqueue the change for persistence on the meta server. */

	  gzochid_data_change change;
	  GBytes *key_bytes = g_bytes_new (key, key_len);
	  
	  change.store = strdup (database->name);
	  change.delete = TRUE;
	  change.key = g_bytes_ref (key_bytes);
	  change.data = NULL;

	  g_array_append_val (dataclient_tx->changeset, change);

	  /* Append this key to the deleted key list, so that we can purge the
	     key from the cache on commit. */
	  
	  dataclient_tx->deleted_keys = g_list_append
	    (dataclient_tx->deleted_keys,
	     create_callback_data (store, key_bytes, TRUE));

	  g_bytes_unref (key_bytes);
	}

      propagate_transaction_status (tx);
      
      return ret;
    }
  else return GZOCHID_STORAGE_ETXFAILURE;
}

/* Returns the first key in the specified store. */

static char *
transaction_first_key (gzochid_storage_transaction *tx,
		       gzochid_storage_store *store, size_t *key_len)
{
  if (ensure_range_lock (tx, store, NULL, 0))
    {
      dataclient_database *database = store->database;
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_range_lock_search_context search_context =
	(dataclient_range_lock_search_context)
	{ database->name, NULL, NULL, NULL };

      gzochid_itree_search_interval
	(dataclient_tx->range_locks, NULL, NULL, find_range_lock_covering_point,
	 &search_context);

      if (search_context.range_lock != NULL)
	{
	  char *ret = NULL;
	  size_t ret_len = 0;

	  /* The key doesn't have to come from the delegate B+tree store. We can
	     get it directly from the lock obtained by the transaction. */
	  
	  const char *data = g_bytes_get_data
	    (search_context.range_lock->key_range->to, &ret_len);
	  
	  ret = malloc (sizeof (char) * ret_len);
	  memcpy (ret, data, ret_len);

	  if (key_len != NULL)
	    *key_len = ret_len;
	  
	  return ret;
	}	  
      else return NULL;
    }
  else return NULL;
}

/* Returns the key immediately after the specified key in the specified 
   store. */

static char *
transaction_next_key (gzochid_storage_transaction *tx,
		      gzochid_storage_store *store, char *key, size_t key_len,
		      size_t *next_key_len)
{
  if (ensure_range_lock (tx, store, key, key_len))
    {
      GBytes *key_bytes = g_bytes_new_static (key, key_len);
      dataclient_database *database = store->database;
      dataclient_transaction *dataclient_tx = tx->txn;
      dataclient_range_lock_search_context search_context =
	(dataclient_range_lock_search_context)
	{ database->name, key_bytes, NULL, NULL };

      gzochid_itree_search_interval
	(dataclient_tx->range_locks, key_bytes, NULL,
	 find_range_lock_covering_point, &search_context);

      g_bytes_unref (key_bytes);
      
      if (search_context.range_lock != NULL)
	{
	  char *ret = NULL;
	  size_t ret_len = 0;
	  
	  /* The key doesn't have to come from the delegate B+tree store. We can
	     get it directly from the lock obtained by the transaction. */

	  if (search_context.range_lock->key_range->to != NULL)
	    {
	      const char *data = g_bytes_get_data
		(search_context.range_lock->key_range->to, &ret_len);
	  
	      ret = malloc (sizeof (char) * ret_len);
	      memcpy (ret, data, ret_len);

	      if (next_key_len != NULL)
		*next_key_len = ret_len;
	    }    
	  return ret;
	}	  
      else return NULL;
    }
  else return NULL;
}

gzochid_storage_engine_interface gzochid_storage_engine_interface_dataclient = 
  {
    "dataclient",

    initialize,
    close_context,
    destroy_context,
    open,
    close_store,
    destroy_store,

    transaction_begin,
    transaction_begin_timed,
    transaction_commit,
    transaction_rollback,
    transaction_prepare,
    
    transaction_get,
    transaction_get_for_update,
    transaction_put,
    transaction_delete,
    transaction_first_key,
    transaction_next_key
  };
