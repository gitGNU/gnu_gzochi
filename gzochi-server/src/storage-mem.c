/* storage-mem.c: Database storage routines for gzochid (in-memory)
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
#include <ctype.h>
#include <glib.h>
#include <gzochi-common.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gzochid-storage.h"

/* 
   The following data structures and functions provide transactional access to
   keys and values arranged in a B+tree. Concurrency is managed in a pessimistic
   fashion, with transaction-bound threads acquiring read / write locks to nodes
   within the tree, and blocking until the desired level of access is granted. 
   Lock cycle (deadlock) detection is performed whenever a transaction's thread
   prepares to block.

   These transaction semantics are intended to (roughly) replicate those of
   Berkeley DB at Serializable isolation, without the complexity incurred to
   support durable persistence to physical media.
*/

#define BRANCHING_FACTOR 8
#define MIN_INTERNAL_CHILDREN BRANCHING_FACTOR / 2
#define MAX_INTERNAL_CHILDREN BRANCHING_FACTOR
#define MIN_LEAF_CHILDREN BRANCHING_FACTOR / 2
#define MAX_LEAF_CHILDREN BRANCHING_FACTOR - 1
#define MAX_PAGE_SIZE 4096

/* A datum (key or value) to be stored within the B+tree. */

struct _btree_datum
{
  unsigned char *data; /* The data. */
  size_t data_len; /* The length of the data. */
};

typedef struct _btree_datum btree_datum;

/* A lock that can be acquired by a transaction / thread. */

struct _btree_lock
{
  GCond cond; /* A condition for lockers to wait on. */

  struct _btree_transaction *writer; /* The current holder of the write lock. */
  GList *readers; /* The current readers. */

  /* The number of readers and writers attempting to seize this lock, plus one
     if the lock is still stored in the lock table. */

  guint ref_count; 
};

typedef struct _btree_lock btree_lock;

/* The "forest" of related B+trees across which transactional operations may be
   applied. */

struct _btree_environment
{
  GList *btrees; /* The B+trees opened in this environment. */
  GList *transactions; /* The set of active transactions. */

  GHashTable *lock_table; /* A mapping of `btree_node' to `btree_lock'. */
  GMutex lock_table_mutex; /* Protects access to the lock table. */
  
  /* A mutex to protect the lists of B+trees and transactions. */

  GMutex mutex; 
};

typedef struct _btree_environment btree_environment;

/* Possible transactional lock failure modes. */

enum _btree_tx_error
  {
    TXN_LOCK_TIMEOUT, /* Timed out waiting for the lock to become available. */
    TXN_DEADLOCK, /* Inconsistent lock access detected. */
    TXN_FAILED /* General / unknown transaction failure. */
  };

#define MEMORY_TRANSACTION_ERROR memory_transaction_error_quark ()

static GQuark
memory_transaction_error_quark (void)
{
  return g_quark_from_static_string ("memory-transaction-error");
}

/* An enumeration of responses from attempting to obtain the lock associated
   with a node in a B+tree. */

enum btree_lock_node_response
  {
    SUCCESS, /* The lock was successfully acquired. */
    FAILURE, /* The lock could not be acquired. */

    /* The lock could not be acquired because the lock or the node was 
       deleted. */
    
    DELETED 
  };

/* Holds transaction state. */

struct _btree_transaction
{
  gint64 end_time; /* The expiration timestamp, in microseconds. */

  btree_environment *environment; /* The enclosing environment. */

  btree_lock *waiting_to_read; /* The read lock being attempted. */
  btree_lock *waiting_to_write; /* The write lock being attempted. */

  GList *modifications; /* The list of modified B+tree nodes. */
  GList *write_locks; /* The write locks held by the transaction. */
  GList *read_locks; /* The read locks held by the transaction. */
};

typedef struct _btree_transaction btree_transaction;

/* Linkage information for a B+tree node, encapsulated to simplify modification
   detection, application, and rollback. */

struct _btree_node_header
{
  struct _btree_node *parent; /* The parent of the node. */
  struct _btree_node *next; /* The next sibling of the node. */
  struct _btree_node *prev; /* The previous sibling of the node. */
  struct _btree_node *first_child; /* The first child of the node. */
};

typedef struct _btree_node_header btree_node_header;

/* B+tree node flag bits. */

#define FLAG_NEW       1 /* The node was created in an active transaction. */
#define FLAG_TOMBSTONE 2 /* The node was deleted in an active transaction. */

/* A block of key-value pairs in the B+tree. */

struct _btree_page
{
  size_t page_size; /* The occupied portion of the block. */
  unsigned char data[MAX_PAGE_SIZE]; /* The block data. */
};

typedef struct _btree_page btree_page;

/* A structural element in the B+tree. */

struct _btree_node
{
  btree_node_header header; /* The node header. */
  btree_node_header *new_header; /* Alternate header for temporary changes. */
				    
  unsigned int min_children; /* The minimum children before triggering merge. */
  unsigned int max_children; /* The maximum children before triggering split. */
  unsigned int flags; /* Status flags; see above. */

  btree_datum key; /* The key. */
  btree_datum new_key; /* Alternate key for temporary changes. */

  btree_page *page; /* The page. `NULL' for internal nodes. */
  btree_page *new_page; /* Alternate page for temporary changes. */
};

typedef struct _btree_node btree_node;

/* The B+tree wrapper structure. */

struct _btree
{
  btree_environment *environment; /* The enclosing B+tree environment. */
  btree_node *root; /* The root node. */
  btree_node *new_root; /* Alternate root for temporary changes. */
  btree_lock *lock; /* The read / write lock for the root. */
};

typedef struct _btree btree;

/* Compares two `btree_datum' structures, returning -1, 0, or 1, respectively,
   if the first is byte-lexicographically less than, equal to, or greater than
   the second. */

static gint
compare_datum (gconstpointer a, gconstpointer b)
{
  size_t i = 0, j = 0;

  const btree_datum *datum_a = a;
  const btree_datum *datum_b = b;

  while (i < datum_a->data_len && j < datum_b->data_len)
    {
      if (datum_a->data[i] < datum_b->data[j])
	return -1;
      else if (datum_b->data[j] < datum_a->data[i])
	return 1;

      i++;
      j++;
    }

  /* If the two buffers equivalent up to the length of the smaller buffer, then
     the shorter one is less than the longer one. */

  return (datum_a->data_len - i) - (datum_b->data_len - j);
}

/* "Moves" the specified buffer from the source to the destination datum, first
   freeing the destination buffer if it exists. When this function returns, the
   buffer pointer of the source datum will be NULL. */

static void
transfer_datum (btree_datum *dest, btree_datum *source)
{
  if (dest->data != NULL)
    free (dest->data);

  dest->data = source->data;
  dest->data_len = source->data_len;
  
  source->data = NULL;
  source->data_len = 0;
}

/* "Clears" the specified datum, freeing its buffer and setting the buffer
   pointer to NULL. */

static void
clear_datum (btree_datum *datum)
{
  free (datum->data);

  datum->data = NULL;
  datum->data_len = 0;
}

/* Compares the datum at the specified page offset with the specified key.
   Returns an integer less than, equal to, or greater than zero if the key is
   found, respectively, to be less than, to match, or to be grater than the page
   datum. */

static gint
compare_page_datum_to_key (btree_page *page, size_t offset, unsigned char *key,
			   size_t key_len)
{
  unsigned short len = 0; 
  int c = 0; 

  if (offset >= page->page_size)
    return -1;

  len = gzochi_common_io_read_short (page->data, offset);
  c = memcmp (page->data + offset + 2, key, MIN (len, key_len));
  
  if (c == 0)

    /* If the key and the page datum are of different lengths, they can't be
       equal. */
    
    return len == key_len ? 0 : len < key_len ? -1 : 1;
  else return c;
}

/* Returns the offset within the specified page at which the specified key is
   stored, or would be stored if it is not currently present in the page. */

static size_t
page_key_offset (btree_page *page, unsigned char *key, size_t key_len)
{
  size_t offset = 0;

  while (offset < page->page_size)
    {
      unsigned short page_key_len = gzochi_common_io_read_short
	(page->data, offset);
      unsigned short page_value_len = 0;
      int c = compare_page_datum_to_key (page, offset, key, key_len);
      
      if (c >= 0)
	return offset;
      
      page_value_len = gzochi_common_io_read_short
	(page->data, offset + page_key_len + 2);

      offset += page_key_len + page_value_len + 4;
    }

  assert (offset == page->page_size);

  /* If we've reached the end of the occupied region of the page, then
     that's where the key would have to go. */
	
  return offset;
}

/*
  Returns a pointer into the specified page data block to the value 
  associated with the specified key, or `NULL' if the key does not exist within
  the specified page. If the key exists and the `value_len' argument is 
  provided, its value will be set to the length of the value.
  
  The returned pointer is owned by the page and should not be freed or 
  otherwise modified.
*/ 

static unsigned char *
lookup_page_value (btree_page *page, unsigned char *key, size_t key_len,
		   size_t *value_len)
{
  size_t offset = 0;
  
  while (offset < page->page_size)
    {
      unsigned short len = gzochi_common_io_read_short (page->data, offset);
      int c = memcmp (key, page->data + offset + 2, MIN (len, key_len));

      offset += len + 2;

      if (c == 0 && len == key_len)
	{
	  unsigned char *ret = page->data + offset + 2;
	  
	  if (value_len != NULL)
	    {
	      len = gzochi_common_io_read_short (page->data, offset);
	      *value_len = len;
	    }
	  
	  return ret;
	}
      else if (c > 0)
	{	  
	  len = gzochi_common_io_read_short (page->data, offset);
	  offset += len + 2;
	}

      /* Keys are stored in sorted order, so once we pass the key's insertion
	 point, we might as well bail out. */
      
      else break;
    }

  return NULL;
}

/* Create and return a new B+tree environment. */

static btree_environment *
create_btree_environment ()
{
  btree_environment *btree_env = calloc (1, sizeof (btree_environment));

  g_mutex_init (&btree_env->mutex);

  btree_env->lock_table = g_hash_table_new (g_direct_hash, g_direct_equal);
  g_mutex_init (&btree_env->lock_table_mutex);
  
  return btree_env;
}

/* Frees the specified B+tree environment. There must be no open B+trees or
   transactions in the environment at the time this function is called. */

static void
close_btree_environment (btree_environment *btree_env)
{
  assert (btree_env->btrees == NULL);
  assert (btree_env->transactions == NULL);

  g_mutex_clear (&btree_env->mutex);

  g_hash_table_destroy (btree_env->lock_table);
  g_mutex_clear (&btree_env->lock_table_mutex);
  
  free (btree_env);
}

/* Initializes and allocates resources for a new read / write lock. */

static btree_lock *
lock_new ()
{
  btree_lock *lock = calloc (1, sizeof (btree_lock));

  g_cond_init (&lock->cond);

  return lock;
}

/* Frees the resources associated with the specified read / write lock. There
   must be no readers or writers of the lock at the time this function is
   called. */

static void
lock_free (btree_lock *lock)
{
  assert (lock->writer == NULL);
  assert (lock->readers == NULL);
  
  g_cond_clear (&lock->cond);
  free (lock);
}

/* Returns `TRUE' if the node's `FLAG_NEW' bit is set, `FALSE' otherwise. */

static gboolean
is_new (btree_node *node)
{
  return node->flags & FLAG_NEW;
}

/* Returns `TRUE' if the node's `FLAG_TOMBSTONE' bit is set, `FALSE' 
   otherwise. */

static gboolean
is_deleted (btree_node *node)
{
  return node->flags & FLAG_TOMBSTONE;
}

/* Clears the specified node's flags. */

static void
clear_flags (btree_node *node)
{
  node->flags = 0;
}

/* Sets the specified node's `FLAG_TOMBSTONE' flag. */

static void
mark_deleted (btree_node *node)
{
  node->flags |= FLAG_TOMBSTONE;
}

/* Create and return a new B+tree node with the specified (optional) parent,
   min and max child counts, and (optional) key datum buffer and length. If
   the key buffer is non-NULL, it will be copied (not assigned) into the new
   node's key datum buffer. */

static btree_node *
create_btree_node (btree_node *parent, unsigned int min_children,
		   unsigned int max_children, unsigned char *key, 
		   size_t key_len)
{
  btree_node *node = calloc (1, sizeof (btree_node));

  node->flags = FLAG_NEW;

  node->min_children = min_children;
  node->max_children = max_children;

  node->header.parent = parent;

  if (key != NULL)
    {
      node->key.data = malloc (sizeof (char) *key_len);
      memcpy (node->key.data, key, key_len);
      node->key.data_len = key_len;
    }

  return node;
}

/* Associates the specified B+tree node with a new `btree_lock' in the lock
   table for the B+tree environment that contains the specified 
   `btree_transaction'. 

   The reference count of the new lock is initially set to 1 to indicate its
   housing within the lock table. */

static void
lock_attach (btree_node *node, btree_transaction *btx)
{
  btree_lock *lock = lock_new ();
  
  g_mutex_lock (&btx->environment->lock_table_mutex);
  g_atomic_int_inc (&lock->ref_count);
  g_hash_table_insert (btx->environment->lock_table, node, lock);
  g_mutex_unlock (&btx->environment->lock_table_mutex);
}

/* A wrapper around `create_btree_node' that separates the concern of creating
   and registering a new `btree_lock' from the concern of creating a new B+tree
   node. */

static btree_node *
create_lockable_btree_node (btree_node *parent, unsigned int min_children,
			    unsigned int max_children, unsigned char *key,
			    size_t key_len, btree_transaction *btx)
{
  btree_node *node = create_btree_node
    (parent, min_children, max_children, key, key_len);

  lock_attach (node, btx);

  return node;
}
  
/* Create and return a new transaction over the specified B+tree environment. 
   The transaction will not attempt to acquire any locks after the specified
   monotonic timestamp has elapsed. */

static btree_transaction *
create_transaction (btree_environment *btree_env, gint64 end_time)
{
  btree_transaction *btx = calloc (1, sizeof (btree_transaction));

  btx->end_time = end_time;
  btx->environment = btree_env;

  /* Add the transaction to the environment. */

  g_mutex_lock (&btree_env->mutex);
  btree_env->transactions = g_list_prepend (btree_env->transactions, btx);
  g_mutex_unlock (&btree_env->mutex);

  return btx;
}

/* Frees the resources allocated for the specified transaction and removes it
   from the enclosing B+tree environment. */

static void
cleanup_transaction (btree_transaction *btx)
{
  btree_environment *btree_env = btx->environment;
  GList *tx_link = g_list_find (btree_env->transactions, btx);

  assert (tx_link != NULL);

  /* Remove the transaction from the environment. */

  g_mutex_lock (&btree_env->mutex);
  btree_env->transactions =
    g_list_delete_link (btree_env->transactions, tx_link);
  g_mutex_unlock (&btree_env->mutex);

  free (btx);
}

/* Returns `TRUE' if the specified transaction's end timestamp has elapsed,
   `FALSE' otherwise. */

static gboolean
check_tx_timeout (btree_transaction *btx)
{
  return btx->end_time > g_get_monotonic_time ();
}

/* Returns 0 if there is another transaction in the environment attempting to
   acquire the specified lock for writing, 1 otherwise.

   This function is used with `g_list_find_custom' during deadlock detection. */

static gint
competing_w_lock (gconstpointer a, gconstpointer b)
{
  const btree_lock *lock = a;
  const gpointer *args = b;

  const btree_transaction *self = args[0];
  const GList *ancestor_transaction_ptr = args[1];

  while (ancestor_transaction_ptr != NULL)
    {
      btree_transaction *ancestor_btx = ancestor_transaction_ptr->data;

      if (ancestor_btx != self && ancestor_btx->waiting_to_write == lock)
	return 0;
      else ancestor_transaction_ptr = ancestor_transaction_ptr->next;
    }

  return 1;
}

/* Returns 0 if there is another transaction in the environment attempting to
   acquire the specified lock for reading OR writing, 1 otherwise.

   This function is used with `g_list_find_custom' during deadlock detection. */

static gint
competing_rw_lock (gconstpointer a, gconstpointer b)
{
  const btree_lock *lock = a;
  const GList *ancestor_transaction_ptr = b;

  while (ancestor_transaction_ptr != NULL)
    {
      btree_transaction *ancestor_btx = ancestor_transaction_ptr->data;

      if (ancestor_btx->waiting_to_write == lock
	  || ancestor_btx->waiting_to_read == lock)
	return 0;
      else ancestor_transaction_ptr = ancestor_transaction_ptr->next;
    }

  return 1;
}

/* Returns `TRUE' if an inconsistently-ordered lock attempt is detected in the
   specified B+tree environment - in the locks currently held by the 
   environment's transactions or in the locks they have declared an intent to
   acquire - `FALSE' otherwise.

   This function is a simple implementation of A. B. Kahn's topological sort
   algorithm, described in "Topological sorting of large networks." The gist of
   the algorithm is that it attempts to discover and grow the subset of the 
   graph (i.e., the set of locks, with edge direction determined by allocation 
   and intent to acquire) that has no cycles. If this subset cannot be grown to
   include the entire set, then there is a deadlock.
*/

static gboolean
lock_detect (btree_environment *btree_env)
{
  GList *q = NULL;
  GList *l = g_list_copy (btree_env->transactions);
  gboolean should_continue = TRUE;
  gboolean ret = FALSE;
  gpointer args[2];
  
  while (l != NULL && should_continue)
    {
      GList *l_ptr = l;
      should_continue = FALSE;

      while (l_ptr != NULL)
	{
	  btree_transaction *btx = l_ptr->data;
	  GList *competing_transaction_ptr = NULL;	  

	  args[0] = btx;
	  args[1] = l;
	  
	  competing_transaction_ptr = g_list_find_custom
	    (btx->read_locks, args, competing_w_lock);

	  if (competing_transaction_ptr == NULL)
	    competing_transaction_ptr = g_list_find_custom
	      (btx->write_locks, l, competing_rw_lock);

	  if (competing_transaction_ptr == NULL)
	    {
	      q = g_list_prepend (q, btx);
	      l = g_list_remove (l, btx);
	      l_ptr = l;

	      should_continue = TRUE;
	    }
	  else l_ptr = l_ptr->next;
	}
    }

  ret = g_list_length (q) != g_list_length (btree_env->transactions);

  g_list_free (l);
  g_list_free (q);

  return ret;
}

/* Attempts to acquire the specified lock for reading or writing. If the 
   specified lock cannot be acquired immediately, this function will wait on the
   lock's condition variable until either the lock's state changes or the 
   specified transaction's timeout elapses.

   - A read lock may only be acquired if no other transaction has acquired it 
   for writing.

   - A write lock may only be acquired if no other transaction has acquired it 
   for reading or for writing.

   Returns `TRUE' if the lock was acquired with the requested exclusivity,
   `FALSE' otherwise - in which case, the lock set of the transaction has not
   changed. */

static gboolean
tx_lock (btree_lock *lock, btree_transaction *btx, gboolean write, GError **err)
{
  gboolean needs_lock = TRUE;
  btree_environment *btree_env = btx->environment;

  /* An environment-global lock is required (at the moment) to support deadlock
     detection, which must be able to interrogate the state of every other
     transaction. */

  g_mutex_lock (&btree_env->mutex);

  assert (btx->waiting_to_write == NULL);
  assert (btx->waiting_to_read == NULL);

  if (write)
    {
      if (lock->writer == btx)

	/* The lock is already sufficient. */

	needs_lock = FALSE;
      else
	{
	  GList *read_link = g_list_find (btx->read_locks, lock);
	  GList *reader_link = g_list_find (lock->readers, btx);

	  /* Declare the transaction's intent to write. */

	  btx->waiting_to_write = lock;

	  if (read_link != NULL)
	    {
	      /* The lock is being upgraded. */

	      btx->read_locks =
		g_list_delete_link (btx->read_locks, read_link);

	      assert (reader_link != NULL);
	      lock->readers = g_list_delete_link (lock->readers, reader_link);
	    }
	}
    }
  else if (lock->writer == btx || g_list_find (btx->read_locks, lock) != NULL)

    /* The lock is already sufficient. */

    needs_lock = FALSE;

  /* Declare the transaction's intent to read. */
  
  else btx->waiting_to_read = lock; 

  while (needs_lock)
    {
      if (!check_tx_timeout (btx))
	{
	  /* The transaction's timeout has elapsed. */

	  g_set_error
	    (err, MEMORY_TRANSACTION_ERROR, TXN_LOCK_TIMEOUT,
	     "Transaction timeout exceeded.");

	  break;
	}

      if (write)
	{
	  if (lock->writer == NULL && lock->readers == NULL)
	    {
	      /* If there are no other holders of the lock, it can be acquired
		 for writing by the transaction. */

	      lock->writer = btx;
	      btx->write_locks = g_list_prepend (btx->write_locks, lock);
	      needs_lock = FALSE;
	    }
	}
      else if (lock->writer == NULL)
	{
	  /* If the lock has no writer, it can be acquired for reading by the
	     transaction. */
	  
	  lock->readers = g_list_prepend (lock->readers, btx);
	  btx->read_locks = g_list_prepend (btx->read_locks, lock);
	  needs_lock = FALSE;
	}

      if (needs_lock)
	{
	  /* Check for a deadlock after declaring the transaction's intent to
	     lock but before waiting on the lock's condition. */

	  if (lock_detect (btx->environment))
	    {
	      /* There's a deadlock. */

	      g_set_error
		(err, MEMORY_TRANSACTION_ERROR, TXN_DEADLOCK,
		 "Deadlock detected.");

	      break;
	    }
	  else g_cond_wait_until
		 (&lock->cond, &btree_env->mutex, btx->end_time);
	}
    }

  /* Clear the transaction's intent, regardless of whether the lock was
     successfully acquired. */

  if (write)
    btx->waiting_to_write = NULL;
  else btx->waiting_to_read = NULL;

  g_mutex_unlock (&btree_env->mutex);

  return !needs_lock;
}

/* Attempts to obtain and return the `btree_lock' associated with the specified
   B+tree node. If the lock is still in the environment's lock table, its
   reference count is also incremented. */

static btree_lock *
lock_ref (btree_node *node, btree_transaction *btx)
{
  btree_lock *lock = NULL;

  g_mutex_lock (&btx->environment->lock_table_mutex);
  lock = g_hash_table_lookup (btx->environment->lock_table, node);

  if (lock != NULL)
    g_atomic_int_inc (&lock->ref_count);
  
  g_mutex_unlock (&btx->environment->lock_table_mutex);
  return lock;
}

/* Decrements the reference count on the specified lock. If the reference count
   reaches zero, the lock is freed. */

static void
lock_unref (btree_lock *lock)
{
  if (g_atomic_int_dec_and_test (&lock->ref_count))
    lock_free (lock);
}

/* A wrapper around `tx_lock' that negotiates the challenges of obtaining a lock
   associated with a B+tree node pointer that may be in the process of being
   freed or may already be freed.

   If this function returns `SUCCESS', the requested lock has been acquired,
   and cannot be deleted out from under the current transaction; if it returns
   `FAILURE', the lock belongs to a node that (for the moment) still exists but
   the requested lock could not be acquired, likely because of a timeout or
   deadlock; if the function returns `DELETED', then the node is being or has
   already been deleted. In this final case, the caller's transaction may still
   be in a healthy state. */

static enum btree_lock_node_response
tx_lock_node (btree_node *node, btree_transaction *btx, gboolean write,
	      GError **err)
{
  /* First, obtain a reference to the node to prevent it from being deleted
     while the lock is attempted, if it still exists. */
  
  btree_lock *lock = lock_ref (node, btx);
  enum btree_lock_node_response ret;

  if (lock == NULL)
    return DELETED;
  else
    {
      /* Attempt to obtain the lock. */
      
      ret = tx_lock (lock, btx, write, err) ? SUCCESS : FAILURE;
      lock_unref (lock); /* Decrement the ref count. */
      
      g_mutex_lock (&btx->environment->lock_table_mutex);

      /* If the lock has subsequently been removed from the lock table, then it
	 must be the case that another thread was in the process of deleting it.
	 In that case, ignore the previous return value from `tx_lock'. */
      
      if (! g_hash_table_contains (btx->environment->lock_table, node))
	ret = DELETED;      
      g_mutex_unlock (&btx->environment->lock_table_mutex);

      return ret;
    }
}

/* Releases the specified lock with respect to the specified transaction. Any
   other transactions waiting to acquire this lock are notified so that they
   may re-attempt it. */

static void
tx_unlock (btree_lock *lock, btree_transaction *btx)
{
  gboolean found_tx = FALSE;
  btree_environment *btree_env = btx->environment;

  g_mutex_lock (&btree_env->mutex);

  if (lock->writer == btx)
    {
      GList *write_link = g_list_find (btx->write_locks, lock);

      /* Remove this lock from the transaction's write set. */

      assert (write_link != NULL);

      found_tx = TRUE;
      btx->write_locks = g_list_delete_link (btx->write_locks, write_link);
      lock->writer = NULL;
    }
  else
    {
      GList *read_link = g_list_find (btx->read_locks, lock);
      GList *reader_link = g_list_find (lock->readers, btx);

      /* Remove this lock from transaction's read set. */

      if (read_link != NULL)
	{
	  assert (reader_link != NULL);

	  found_tx = TRUE;
	  btx->read_locks = g_list_delete_link (btx->read_locks, read_link);
	  lock->readers = g_list_delete_link (lock->readers, reader_link);
	}
    }

  assert (found_tx);
  g_cond_broadcast (&lock->cond); /* Wake up any waiting transactions. */

  g_mutex_unlock (&btree_env->mutex);
}

/* Releases the lock associated with the specified node with respect to the 
   specified transaction. */

static void
tx_unlock_node (btree_node *node, btree_transaction *btx)
{
  btree_lock *lock = g_hash_table_lookup (btx->environment->lock_table, node);

  assert (lock != NULL);
  tx_unlock (lock, btx);
}

/* Breaks the connection between the specified node and its associated lock in
   the environment's lock table. This is necessary as part of the destruction
   of the node.

   The lock is not necessarily deleted, since there may be other transactions in
   other threads attempting to manipulate it. Instead, its reference count is 
   decremented, which will trigger its deletion only when no other operations
   are in progress against it. */

static void
lock_detach (btree_node *node, btree_transaction *btx)
{
  btree_lock *lock = NULL;

  g_mutex_lock (&btx->environment->lock_table_mutex);

  lock = g_hash_table_lookup (btx->environment->lock_table, node);
  assert (lock != NULL);
  assert (lock->writer == btx);

  g_hash_table_remove (btx->environment->lock_table, node);

  tx_unlock (lock, btx);
  
  lock_unref (lock);
  
  g_mutex_unlock (&btx->environment->lock_table_mutex);
}

/* Marks this node as having been modified by the specified transaction, so that
   its changes are properly tracked. 

   This function should be called only when a modification is actually 
   requested, not merely when a write lock is acquired. Otherwise the 
   modification may not be correctly detected during commit or rollback. */

static void
mark_modification (btree_node *node, btree_transaction *btx)
{
  if (g_list_find (btx->modifications, node) == NULL)
    btx->modifications = g_list_prepend (btx->modifications, node);
}

/* Removes this node from the list of nodes modified by the specified 
   transaction.

   This function should only be called when a newly-created node is deleted
   within the same transaction. */

static void
unmark_modification (btree_node *node, btree_transaction *btx)
{
  GList *link = g_list_find (btx->modifications, node);

  assert (link != NULL);

  btx->modifications = g_list_delete_link (btx->modifications, link);
}

/* The following functions provide transactional facades for navigating the
   B+tree and reading keys and values. */

/* Returns the "effective" B+tree node header. The effective header for writers
   is the default header for newly created nodes; otherwise it is the "scratch"
   header. For readers, it is the default header unless a scratch header is
   present.
   
   This function assumes its call originates within the scope of a transaction 
   that holds at least a read lock on the specified node. 
*/

static btree_node_header *
effective_header (btree_node *node, gboolean write)
{
  if (write)
    {
      if (is_new (node))
	return &node->header;
      else if (node->new_header == NULL)
	{
	  node->new_header = malloc (sizeof (btree_node_header));
	  memcpy (node->new_header, &node->header,
		  sizeof (btree_node_header));
	}
      return node->new_header;
    }
  else return node->new_header != NULL ? node->new_header : &node->header;
}

/* Returns the "effective" B+tree key datum. The effective key datum for writers
   is the default key for newly created nodes; otherwise it is the "scratch" 
   datum. For readers, it is the default keu unless a scratch datum is present.
   
   This function assumes its call originates within the scope of a transaction 
   that holds at least a read lock on the specified node. 
*/

static btree_datum *
effective_key (btree_node *node, gboolean write)
{
  if (write)
    return is_new (node) ? &node->key : &node->new_key;
  else return node->new_key.data != NULL ? &node->new_key : &node->key;
}

/*
  Returns the "effective" B+tree page. The effective page for writers is the 
  default page for newly created nodes; otherwise it is the "scratch" page. 
  For readers, it is the default page unless a scratch datum is present.
  
  This function assumes its call originates within the scope of a transaction 
  that holds at least a read lock on the target node; if the page for writers is
  requested, the transaction must hold a write lock. 
*/

static btree_page *
effective_page (btree_node *node, gboolean write)
{
  if (write)
    {
      if (is_new (node))
	return node->page;
      else if (node->page != NULL && node->new_page == NULL)
	{
	  /*
	    If the node doesn't have a default page, it's not a leaf. If it is
	    a leaf and it's not new and it doesn't yet have a scratch page,
	    allocate one now.
	  */
	  
	  node->new_page = malloc (sizeof (btree_page));

	  node->new_page->page_size = node->page->page_size;
	  memcpy (node->new_page->data, node->page->data,
		  node->page->page_size);
	}

      return node->new_page;
    }
  else return node->new_page != NULL ? node->new_page : node->page;
}

/* Returns the first child of the specified node, from the point of view of the
   specified transaction, which is assumed to have at least a read lock on the 
   node. This function returns NULL if there is no first child, or if the new
   lock cannot be established (in which case, the error return will be set). 
*/

static btree_node *
tx_first_child (btree_node *node, btree_transaction *btx, GError **err)
{
  btree_node_header *header = effective_header (node, FALSE);
  btree_node *first_child = header->first_child;

  if (first_child != NULL &&
      tx_lock_node (first_child, btx, FALSE, err) != SUCCESS)
    return NULL;
  else return first_child;
}

/* Returns the parent of the specified node, from the point of view of the
   specified transaction, which is assumed to have at least a read lock on the 
   node. This function returns NULL if the node is the root, or if the new
   lock cannot be established (in which case, the error return will be set). 
*/

static btree_node *
tx_parent (btree_node *node, btree_transaction *btx, GError **err)
{
  btree_node_header *header = effective_header (node, FALSE);
  btree_node *parent = header->parent;

  if (parent != NULL && tx_lock_node (parent, btx, FALSE, err) != SUCCESS)
    return NULL;
  else return parent;
}

/* Returns the next sibling of the specified node, from the point of view of the
   specified transaction, which is assumed to have at least a read lock on the 
   node. This function returns NULL if there is no next sibling, or if the new
   lock cannot be established (in which case, the error return will be set). 
*/

static btree_node *
tx_next_sibling (btree_node *node, btree_transaction *btx, GError **err)
{
  btree_node_header *header = effective_header (node, FALSE);
  btree_node *next = header->next;

  if (next != NULL && tx_lock_node (next, btx, FALSE, err) != SUCCESS)
    return NULL;
  else return next;
}

/* Returns the last child of the specified node, from the point of view of the
   specified transaction, which is assumed to have at least a read lock on the 
   node. This function returns NULL if the node has no children, or if the new
   lock cannot be established (in which case, the error return will be set). 
*/

static btree_node *
tx_last_child (btree_node *node, btree_transaction *btx, GError **err)
{
  GError *tmp_err = NULL;
  btree_node *child = tx_first_child (node, btx, &tmp_err);

  if (child == NULL)
    {
      if (tmp_err != NULL)
	g_propagate_error (err, tmp_err);
      return NULL;
    }

  while (TRUE)
    {
      btree_node *next = tx_next_sibling (child, btx, &tmp_err);

      if (tmp_err != NULL)
	{
	  g_propagate_error (err, tmp_err);
	  return NULL;
	}

      if (next == NULL)
	break;
      else
	child = next;
    }

  return child;
}

/* Returns the previous sibling of the specified node, from the point of view of
   the specified transaction, which is assumed to have at least a read lock on 
   the node. This function returns NULL if there is no next sibling, or if the 
   new lock cannot be established (in which case, the error return will be 
   set). 
*/

static btree_node *
tx_prev_sibling (btree_node *node, btree_transaction *btx, GError **err)
{
  btree_node_header *header = effective_header (node, FALSE);
  btree_node *prev = header->prev;

  if (prev != NULL && tx_lock_node (prev, btx, FALSE, err) != SUCCESS)
    return NULL;
  else return prev;
}

static gboolean
locked_for_write (btree_node *node, btree_transaction *btx)
{
  btree_lock *lock = NULL;
  gboolean ret = FALSE;
  
  g_mutex_lock (&btx->environment->lock_table_mutex);
  lock = g_hash_table_lookup (btx->environment->lock_table, node);
  if (lock != NULL)
    ret = lock->writer == btx;
  g_mutex_unlock (&btx->environment->lock_table_mutex);

  return ret;
}

/* Returns the root of the specified B+tree, from the point of view of the
   specified transaction, which must either have a write lock on the root or
   can acquire read locks on the B+tree wrapper and the root. This function 
   returns NULL if there was a failure to acquire the requisite locks. 
*/

static btree_node *
tx_root (btree *btree, btree_transaction *btx)
{
  if (!tx_lock (btree->lock, btx, FALSE, NULL))
    return NULL;
  else if (btree->new_root != NULL && locked_for_write (btree->new_root, btx))
    return btree->new_root;
  else if (tx_lock_node (btree->root, btx, FALSE, NULL) == SUCCESS)
    return btree->root;
  else return NULL;
}

/* Returns the number of children of the specified node.
   
   Read locks are established on all children. If any locks cannot be acquired,
   this function returns 0 and sets the error value accordingly. 
*/

static unsigned int
count_children (btree_transaction *btx, btree_node *node, GError **err)
{
  GError *tmp_err = NULL;
  unsigned int num_children = 0;
  btree_node *child = NULL;

  child = tx_first_child (node, btx, &tmp_err);

  while (TRUE)
    {
      if (child == NULL)
	{
	  if (tmp_err != NULL)
	    {
	      g_propagate_error (err, tmp_err);
	      return 0;
	    }
	  else return num_children;
	}
      else num_children++;

      child = tx_next_sibling (child, btx, &tmp_err);
    }
}

/* Sets the first child of the specified node with respect to the specified
   transaction, which is assumed to have at least a read lock on the node. This
   function attempts to establish a write lock on the node; it returns `TRUE' if
   the lock is successfully acquired, `FALSE' otherwise. 
*/

static gboolean
tx_set_first_child (btree_node *node, btree_transaction *btx, 
		    btree_node *first_child)
{
  GError *err = NULL;
  btree_node_header *header = effective_header (node, TRUE);

  if (tx_lock_node (node, btx, TRUE, &err) != SUCCESS)
    {
      g_clear_error (&err);
      return FALSE;
    }

  header->first_child = first_child;
  mark_modification (node, btx);
  return TRUE;
}

/* Sets the next sibling of the specified node with respect to the specified
   transaction, which is assumed to have at least a read lock on the node. This
   function attempts to establish a write lock on the node; it returns `TRUE' if
   the lock is successfully acquired, `FALSE' otherwise. 
*/

static gboolean
tx_set_next_sibling (btree_node *node, btree_transaction *btx, btree_node *next)
{
  GError *err = NULL;
  btree_node_header *header = effective_header (node, TRUE);

  if (tx_lock_node (node, btx, TRUE, &err) != SUCCESS)
    {
      g_clear_error (&err);
      return FALSE;
    }

  header->next = next;
  mark_modification (node, btx);
  return TRUE;
}

/* Sets the previous sibling of the specified node with respect to the specified
   transaction, which is assumed to have at least a read lock on the node. This
   function attempts to establish a write lock on the node; it returns `TRUE' if
   the lock is successfully acquired, `FALSE' otherwise. 
*/

static gboolean 
tx_set_prev_sibling (btree_node *node, btree_transaction *btx, btree_node *prev)
{
  GError *err = NULL;
  btree_node_header *header = effective_header (node, TRUE);

  if (tx_lock_node (node, btx, TRUE, &err) != SUCCESS)
    {
      g_clear_error (&err);
      return FALSE;
    }

  header->prev = prev;
  mark_modification (node, btx);
  return TRUE;
}

/* Sets the parent of the specified node with respect to the specified
   transaction, which is assumed to have at least a read lock on the node. This
   function attempts to establish a write lock on the node; it returns `TRUE' if
   the lock is successfully acquired, `FALSE' otherwise. 
*/

static gboolean
tx_set_parent (btree_node *node, btree_transaction *btx, btree_node *parent)
{
  GError *err = NULL;
  btree_node_header *header = effective_header (node, TRUE);

  if (tx_lock_node (node, btx, TRUE, &err) != SUCCESS)
    {
      g_clear_error (&err);
      return FALSE;
    }

  header->parent = parent;
  mark_modification (node, btx);
  return TRUE;
}

/* Sets the root of the specified B+tree with respect to the specified
   transaction, which is assumed to have at least a read lock on the B+tree
   wrapper. This function attempts to establish a write lock on the wrapper; it
   returns `TRUE' if the lock is successfully acquired, `FALSE' otherwise. 
*/

static gboolean
tx_set_root (btree *btree, btree_transaction *btx, btree_node *root)
{
  GError *err = NULL;

  if (!tx_lock (btree->lock, btx, TRUE, &err))
    {
      g_clear_error (&err);
      return FALSE;
    }

  btree->new_root = root;
  return TRUE;
}

/* Unlinks a node from its parent and next and previous siblings, with respect
   to the current transaction. Returns `FALSE' if write locks cannot be 
   acquired on all adjacent nodes, `TRUE' otherwise. 
*/

static gboolean
unlink_node (btree_node *node, btree_transaction *btx)
{
  GError *err = NULL;
  
  btree_node *first_child = NULL;
  btree_node *next = NULL;
  btree_node *parent = NULL;
  btree_node *prev = NULL;
  
  next = tx_next_sibling (node, btx, &err);
  if (err == NULL)
    prev = tx_prev_sibling (node, btx, &err);
  if (err == NULL && prev == NULL)
    {
      btree_node *parent_first_child = NULL;
      
      parent = tx_parent (node, btx, &err); 
      parent_first_child = tx_first_child (parent, btx, &err);

      if (err == NULL && node == parent_first_child)
	if (! tx_set_first_child (parent, btx, next))
	  return FALSE;
    }

  if (err == NULL && first_child != NULL)

    /* If the node being unlinked has children, those children are surely going
       to be unlinked as well. No other fixups are necessary here, but they 
       should not attempt to dereference their parent. */
    
    if (! tx_set_parent (first_child, btx, NULL))
      return FALSE;

  if (err == NULL && next != NULL)
    if (! tx_set_prev_sibling (next, btx, prev))
      return FALSE;
  if (err == NULL && prev != NULL)
    if (! tx_set_next_sibling (prev, btx, next))
      return FALSE;
  
  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }
  else return TRUE;
}

/* Frees the resources associated with the specified page. */

static void
free_btree_page (btree_page *page)
{
  free (page);
}

/* Frees the resources associated with the specified node. */

static void
free_btree_node (btree_node *node)
{
  free (node->key.data);

  if (node->page != NULL)
    free_btree_page (node->page);

  free (node);
}

/* Destructively removes a node from the specified B+tree and frees its 
   resources.

   The specified transaction must have a write lock on the node, which will be
   released when this function returns. 
*/

static void
delete_node (btree_node *node, btree_transaction *btx)
{
  assert (node->new_key.data == NULL);
  assert (node->new_page == NULL);

  lock_detach (node, btx);
  
  if (node->new_header != NULL)
    free (node->new_header);
  
  free_btree_node (node);
}

/* Non-destructively marks a node as deleted, or deletes it destructively if it
   was added in the current transaction. In either case, it is unlinked from its
   parent and siblings.

   This function attempts to acquire a write lock on the target node and nodes
   adjacent to it. It will return `FALSE' if any of these locks cannot be
   acquired, `TRUE' otherwise.
*/

static gboolean
mark_node_deleted (btree_transaction *btx, btree_node *node)
{
  if (tx_lock_node (node, btx, TRUE, NULL) != SUCCESS)
    return FALSE;

  if (is_new (node))
    {
      /* If the node was created in the current transaction, just scrap it;
	 we'll never need to roll it back to its previous state. */

      unmark_modification (node, btx);
      if (!unlink_node (node, btx))
	return FALSE;
      delete_node (node, btx);
    }
  else
    {
      if (!unlink_node (node, btx))
	return FALSE;

      mark_deleted (node);
      mark_modification (node, btx);

      if (node->new_key.data != NULL)

	/* We're deleting a structural node that we modified earlier during this
	   transaction - possibly because of a split / adjacent insertion. Clean
	   up its previous key. */
	
	clear_datum (&node->new_key);
      
      if (node->new_page != NULL)
	{
	
	  /* We're deleting a leaf node that we modified earlier during this
	     transaction. Clean up its scratch page. */

	  free_btree_page (node->new_page);
	  node->new_page = NULL;
	}
    }

  return TRUE;
}

/* Applies changes to the content or location of a leaf or internal node in a 
   B+tree. This function is a `GFunc' visitor to be used with `g_list_foreach' 
   from `commit'. 
*/

static void
apply_modification (gpointer data, gpointer user_data)
{
  btree_node *node = data;
  btree_transaction *btx = user_data;

  if (! is_new (node))
    assert (locked_for_write (node, btx));

  /* Perform the deletion of a node with a tombstone marker. */

  if (is_deleted (node))
    delete_node (node, btx);
  else
    {
      if (node->new_key.data != NULL)
	transfer_datum (&node->key, &node->new_key); /* Commit key change. */
      if (node->new_page != NULL)
	{
	  /* Commit page change. */

	  free_btree_page (node->page);
	  node->page = node->new_page;
	  node->new_page = NULL;
	}

      if (node->new_header != NULL)
	{
	  /* Commit any changes made to the node's location within the 
	     B+tree. */

	  memcpy (&node->header, node->new_header,
		  sizeof (btree_node_header));
	  free (node->new_header);
	  node->new_header = NULL;
	}

      clear_flags (node);
      tx_unlock_node (node, btx);
    }
}

/* Applies changes to the identity of the root node in a B+tree. This function
   is a `GFunc' visitor to be used with `g_list_foreach' from `commit'. 
*/

static void
apply_root_modification (gpointer data, gpointer user_data)
{
  btree *btree = data;
  btree_transaction *btx = user_data;

  if (btree->new_root != NULL && btree->lock->writer == btx)
    {
      btree->root = btree->new_root;
      btree->new_root = NULL;
    }
}

/* Make permanent all of the modifications performed in the specified 
   transaction, across all B+trees in the environment to which the transaction 
   belongs.

   When this function returns, all read and write locks held by the transaction
   will have been released, and the transaction itself will have been cleaned 
   up; its handle should not be used again.
*/

static void
commit (btree_transaction *btx)
{
  btree_environment *btree_env = btx->environment;

  g_list_foreach (btree_env->btrees, apply_root_modification, btx);

  g_list_foreach (btx->modifications, apply_modification, btx);
  g_list_foreach (btx->write_locks, (GFunc) tx_unlock, btx);
  g_list_foreach (btx->read_locks, (GFunc) tx_unlock, btx);

  g_list_free (btx->modifications);
  g_list_free (btx->write_locks);
  g_list_free (btx->read_locks);

  cleanup_transaction (btx);
}

/* Undoes changes to the content or location of a leaf or internal node in a 
   B+tree. This function is a `GFunc' visitor to be used with `g_list_foreach' 
   from `rollback'. 
*/

static void
rollback_modification (gpointer data, gpointer user_data)
{
  btree_node *node = data;
  btree_transaction *btx = user_data;

  assert (locked_for_write (node, btx));

  if (is_new (node))
    /* Remove any newly created nodes. */
      
    delete_node (node, btx);
  else
    {
      if (node->new_key.data != NULL)
	clear_datum (&node->new_key); /* Undo key changes. */
      if (node->new_page != NULL)
	{
	  /* Undo page changes. */

	  free_btree_page (node->new_page);
	  node->new_page = NULL;
	}
      
      if (node->new_header != NULL)
	{
	  /* Restore the original location of any node that got moved around. */

	  free (node->new_header);
	  node->new_header = NULL;
	}

      clear_flags (node);
      tx_unlock_node (node, btx);
    }
}

/* Undoes changes to the identity of the root node in a B+tree. This function
   is a `GFunc' visitor to be used with `g_list_foreach' from `rollback'. 
*/

static void
rollback_root_modification (gpointer data, gpointer user_data)
{
  btree *btree = data;
  btree_transaction *btx = user_data;

  if (btree->new_root != NULL && btree->lock->writer == btx)
    btree->new_root = NULL;
}

/* Undo all of the modifications performed in the specified transaction, across
   all B+trees in the environment to which the transaction belongs.

   When this function returns, all read and write locks held by the transaction
   will have been released, and the transaction itself will have been cleaned 
   up; its handle should not be used again.
*/

static void
rollback (btree_transaction *btx)
{
  btree_environment *btree_env = btx->environment;

  /* Discard any changes to B+tree root nodes. */

  g_list_foreach (btree_env->btrees, rollback_root_modification, btx);

  g_list_foreach (btx->modifications, rollback_modification, btx);
  g_list_foreach (btx->write_locks, (GFunc) tx_unlock, btx);
  g_list_foreach (btx->read_locks, (GFunc) tx_unlock, btx);

  g_list_free (btx->modifications);
  g_list_free (btx->write_locks);
  g_list_free (btx->read_locks);

  cleanup_transaction (btx);
}

static void
free_btree_node_wrapper (gpointer data, gpointer user_data)
{
  free_btree_node (data);
}

static void
walk_btree (btree_node *root, GFunc func, gpointer user_data)
{
  GList *to_visit = g_list_append (NULL, root);

  while (to_visit != NULL)
    {
      GList *to_visit_ptr = to_visit;
      btree_node *node = to_visit_ptr->data;
      btree_node_header *header = effective_header (node,  FALSE);
      
      if (header->next != NULL)
	to_visit = g_list_prepend (to_visit, header->next);

      if (header->first_child != NULL)
	to_visit = g_list_prepend (to_visit, header->first_child);

      to_visit = g_list_delete_link (to_visit, to_visit_ptr);
      func (node, user_data);
    }
}

/* Non-recursively frees all of the B+tree nodes reachable from the specified 
   root node. */

static void
free_btree (btree_node *root)
{
  walk_btree (root, free_btree_node_wrapper, NULL);
}

static void
force_lock_detach (gpointer data, gpointer user_data)
{
  btree_transaction *btx = user_data;
  btree_lock *lock = g_hash_table_lookup (btx->environment->lock_table, data);

  assert (lock != NULL);
  
  g_hash_table_remove (btx->environment->lock_table, data);
  lock_free (lock);
}

/* Frees the resources allocated for the specified B+tree (including all key
   and value datums) and removes it from its enclosing environment. */

static void
close_btree (btree *bt)
{
  GList *btree_link = NULL;
  btree_environment *btree_env = bt->environment;
  btree_transaction *btx = create_transaction (btree_env, G_MAXINT64);
  
  /* Remove the B+tree from the environment. */
  
  g_mutex_lock (&btree_env->mutex);
  
  btree_link = g_list_find (btree_env->btrees, bt);
  assert (btree_link != NULL);
  btree_env->btrees = g_list_delete_link (btree_env->btrees, btree_link);
  
  g_mutex_unlock (&btree_env->mutex);

  walk_btree (bt->root, force_lock_detach, btx);
  commit (btx);

  lock_free (bt->lock);
  free_btree (bt->root);
  free (bt);
}

/* Finds and returns the node with the specified key if it exists in the 
   specified B+tree, or the node that would be the parent of the node with that
   key if it does not currently exist. Use this function to locate leaf nodes 
   for reading or updating their values, or to find the right place to insert
   new leaves.

   This function establishes a read lock on each node it visits during the
   search.   
*/

static btree_node *
search (btree_transaction *btx, btree *bt, char *key, size_t key_len)
{
  btree_node *node = tx_root (bt, btx);
  gboolean is_root = TRUE;
  btree_datum search_datum;
  
  search_datum.data = (unsigned char *) key;
  search_datum.data_len = key_len;

  while (TRUE)
    {
      GError *err = NULL;
      btree_node *child = NULL, *prev = NULL;
      gboolean node_is_leaf_parent = FALSE;
      
      if (node == NULL)
	return NULL;

      child = tx_first_child (node, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return NULL;
	}

      while (child != NULL)
	{
	  btree_page *page = effective_page (child, FALSE);

	  if (page != NULL)
	    {
	      /* If we're examining leaf nodes, choose the greatest whose first
		 key is smaller than the search key. */
	      
	      node_is_leaf_parent = TRUE;

	      if (compare_page_datum_to_key
		  (page, 0, search_datum.data, search_datum.data_len) > 0)
		return prev != NULL ? prev : child;
	    }
	  else
	    {
	      /* If we're examining internal nodes, choose the first whose 
		 greatest key is greater than the search key. */
	      
	      btree_datum *child_datum = effective_key (child, FALSE);

	      if (compare_datum (&search_datum, child_datum) < 0)
		break;
	    }

	  prev = child;
	  child = tx_next_sibling (child, btx, &err);

	  if (err != NULL)
	    {
	      g_error_free (err);
	      return NULL;
	    }
	}

      if (child == NULL)
	{
	  if (node_is_leaf_parent)
	    return prev;
	  if (is_root)
	    return node;
	  else node = prev;
	}
      else node = child;
      
      is_root = FALSE;
    }

  return node;
}

/* Returns a newly allocated datum buffer with a key guaranteed to fall after
   the one specified. */

static unsigned char *
key_after (unsigned char *key, size_t key_len)
{
  /* The next key after the specified key must be immediately >= the specified
     key with '\0' appended to it. */

  unsigned char *next_key = calloc (key_len + 1, sizeof (char));
  return memcpy (next_key, key, key_len);
}

/* A shorthand for adjusting a node's key to exceed a specified maximum, a
   common necessity during inserts, splits, and merged. */

static void
set_node_key_to_key_after (btree_node *node, btree_datum *key)
{
  btree_datum *node_key = effective_key (node, TRUE);
  unsigned char *next_key = key_after (key->data, key->data_len);

  if (node_key->data != NULL)
    free (node_key->data);

  node_key->data = next_key;
  node_key->data_len = key->data_len + 1;
}

/*
  Updates the key for a leaf or internal node based, respectively, on the first
  record in its page or its first child, whose key might have changed as the
  result of a change to the contents of a page; then do the same to the parent
  and so on.

  Call this function after making a change to a page that affects the first key.
*/

static void
update_key (btree_node *node, btree_transaction *btx, size_t offset,
	    GError **err)
{
  btree_page *page = effective_page (node, FALSE);

  size_t key_len = gzochi_common_io_read_short (page->data, offset);
  unsigned char *key = page->data + offset + 2;

  btree_datum new_key = { key, key_len };
  
  while (node != NULL)
    {
      btree_node_header *header = effective_header (node, FALSE);
      
      if (tx_lock_node (node, btx, TRUE, err) == SUCCESS)
	set_node_key_to_key_after (node, &new_key);
      else return;

      if (header->next == NULL)
	{
	  btree_node *parent = tx_parent (node, btx, err);

	  if (parent != NULL)
	    {
	      header = effective_header (parent, FALSE);
	      if (header->parent != NULL)
		node = parent;
	      else return;
	    }
	}
      else return;
    }
}

/*
  Inserts the specified key and value into the page data for the specified leaf
  node at the specified offset. Subsequent records will be pushed "right" to
  make room for the new record. If the offset is zero (i.e., the record is being
  inserted at the beginning of the block) the node's key (and potentially the
  keys of its ancestors) will be updated. This function acquires a write lock on
  the target node and on any ancestors as necessary. If any of these locks 
  cannot be obtained, the provided `GError' will be set accordingly.

  This function assumes that:

  - The target node is a leaf node
  - There is enough room for the new record in the target page
  - The specified offset falls within or exactly at the end of the current page
    data block.
  - The key does not currently exist in the target page.
*/

static void
insert_page_record (btree_transaction *btx, btree_node *leaf, size_t offset,
		    unsigned char *key, size_t key_len,
		    unsigned char *value, size_t value_len, GError **err)
{
  btree_page *page = NULL;
  size_t total_len = key_len + value_len + 4;

  if (tx_lock_node (leaf, btx, TRUE, err) != SUCCESS)
    return;

  page = effective_page (leaf, TRUE);

  assert (page != NULL);
  assert (page->page_size + total_len <= MAX_PAGE_SIZE);

  /* Move subsequent data out of the way if necessary. */
  
  if (offset < page->page_size)
    memmove (page->data + offset + total_len, page->data + offset,
	     page->page_size - offset);

  gzochi_common_io_write_short (key_len, page->data, offset);
  memcpy (page->data + offset + 2, key, key_len);
  gzochi_common_io_write_short (value_len, page->data, offset + key_len + 2);
  memcpy (page->data + offset + key_len + 4, value, value_len);

  page->page_size += total_len;

  /* Add the leaf to the modification list for commit / rollback. */
  
  mark_modification (leaf, btx);
  
  if (offset + total_len == page->page_size)

    /* Update the leaf's key if necessary. */

    update_key (leaf, btx, offset, err);
}

/*
  Removes the record at the specified offset within the page of the specified
  leaf node, shifting any subsequent records "left" to fill in the gap.
  
  If the record being removed is the first record in the page (offset zero),
  the leaf node's key and potentially its ancestor's keys will be updated.
  
  If the record being removed is the only record in the page, the leaf node
  will be removed from its parent. (This may trigger a merge on the parent 
  internal node.)
  
  If the locks required to complete any of these operations cannnot be 
  acquired, this function sets the supplied `GError' argument accordingly.
*/

static void
remove_page_record (btree_transaction *btx, btree_node *leaf, size_t offset,
		    GError **err)
{
  btree_page *page = NULL;
  unsigned short key_len = 0;
  unsigned short value_len = 0;
  size_t record_len = 0;
  size_t next_record_offset = 0;

  if (tx_lock_node (leaf, btx, TRUE, err) != SUCCESS)
    return;

  page = effective_page (leaf, TRUE);

  key_len = gzochi_common_io_read_short (page->data, offset);
  value_len = gzochi_common_io_read_short (page->data, offset + key_len + 2);
  record_len = key_len + value_len + 4;
  next_record_offset = offset + record_len;
  
  /* Move subsequent data to fill the gap if necessary. */

  if (next_record_offset < page->page_size)  
    memmove (page->data + offset, page->data + next_record_offset,
	     page->page_size - next_record_offset);
  
  page->page_size -= record_len;

  /* Add the leaf to the modification list for commit / rollback. */

  mark_modification (leaf, btx);
  
  if (offset == page->page_size && page->page_size > 0)

    /* Update the leaf's key if necessary. */

    update_key (leaf, btx, offset, err);
}

/* Performs a merge on the specified node by "borrowing" enough children from 
   its next and previous siblings (who must have the specified number of "spare"
   children) that the node meets its minimum child threshold. 

   This function assumes at least read locks have been established on the node
   and its next and previous siblings (if they are present). Additional read and
   write locks will be acquired on any children that are moved or conjoined.

   This function returns `FALSE' if any locks cannot be acquired, `TRUE' 
   otherwise.
*/

static gboolean
merge_borrow (btree_node *prev, btree_node *node, btree_node *next,
	      btree_transaction *btx, unsigned int spare_prev,
	      unsigned int num_children, unsigned int spare_next)
{
  GError *err = NULL;
  btree_node *first_child = NULL;
  btree_node *last_child = NULL;
  btree_node *first_next_child = NULL;
  btree_node *last_prev_child = NULL;

  gboolean borrowed_prev = FALSE;
  gboolean borrowed_next = FALSE;

  first_child = tx_first_child (node, btx, &err);
  if (err == NULL)
    last_child = tx_last_child (node, btx, &err);
  if (err == NULL && spare_next > 0)
    first_next_child = tx_first_child (next, btx, &err);
  if (err == NULL && spare_prev > 0)
    last_prev_child = tx_last_child (prev, btx, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }

  /* Borrow children from the previous sibling. */

  /* First, link the last child of the previous sibling to the first_child of
     the borrowing node. */

  if (spare_prev > 0)
    {
      if (!tx_set_next_sibling (last_prev_child, btx, first_child)
	  || (first_child != NULL
	      && !tx_set_prev_sibling (first_child, btx, last_prev_child)))
	return FALSE;
      else borrowed_prev = TRUE;
    }
    
  while (spare_prev > 0 && num_children < node->min_children)
    {
      btree_node *prev_last_prev_child =
	tx_prev_sibling (last_prev_child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      if (!tx_set_parent (last_prev_child, btx, node))
	return FALSE;

      spare_prev--;
      num_children++;

      last_prev_child = prev_last_prev_child;
    }

  if (borrowed_prev)
    {
      btree_node *prev_next = tx_next_sibling (last_prev_child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      if (!tx_set_first_child (node, btx, prev_next)
	  || !tx_set_next_sibling (last_prev_child, btx, NULL)
	  || !tx_set_prev_sibling (prev_next, btx, NULL))
	return FALSE;

      /* Update the prev sibling's key, since its keyspace has been altered. */

      set_node_key_to_key_after (prev, effective_key (last_prev_child, FALSE));
    }

  /* Borrow children from the next sibling. */

  if (spare_next > 0 && num_children < node->min_children)
    {
      /* It's possible that the last child is `NULL' if the node has no
	 children. */
      
      if ((last_child != NULL
	   && !tx_set_next_sibling (last_child, btx, first_next_child))
	  || !tx_set_prev_sibling (first_next_child, btx, last_child))
	return FALSE;
      else borrowed_next = TRUE;
    }

  while (spare_next > 0 && num_children < node->min_children)
    {
      btree_node *next_first_next_child =
	tx_next_sibling (first_next_child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      /* Set the new parent. */

      if (!tx_set_parent (first_next_child, btx, node))
	return FALSE;

      spare_next--;
      num_children++;

      last_child = first_next_child;
      first_next_child = next_first_next_child;
    }

  /* If any nodes were borrowed from the next sibling, update its first 
     child. */

  if (borrowed_next)
    {
      btree_node *next_prev = tx_prev_sibling (first_next_child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      if (!tx_set_first_child (next, btx, first_next_child)
	  || !tx_set_next_sibling (next_prev, btx, NULL)
	  || !tx_set_prev_sibling (first_next_child, btx, NULL))
	return FALSE;

      /* Update the merge target's key, since its keyspace has been altered. */

      set_node_key_to_key_after (node, effective_key (next_prev, FALSE));
    }

  return TRUE;
}

/* Performs a merge by distribution, diverting the children of the specified
   node to its immediate previous and next siblings (who must have the specified
   number of slots to receive them) and ultimately deleting the original 
   node.

   This function assumes at least read locks have been established on the node
   and its next and previous siblings (if they are present). Additional read and
   write locks will be acquired on any children that are moved or conjoined.

   This function returns `FALSE' if any locks cannot be acquired, `TRUE' 
   otherwise.
*/

static gboolean
merge_distribute (btree_node *prev, btree_node *node, btree_node *next,
		  btree_transaction *btx, unsigned int prev_slots,
		  unsigned int num_children, unsigned int next_slots)
{
  GError *err = NULL;
  btree_node *child = NULL;
  btree_node *last_prev_child = NULL;
  btree_node *first_next_child = NULL;

  child = tx_first_child (node, btx, &err);
  if (err == NULL && prev_slots > 0)
    last_prev_child = tx_last_child (prev, btx, &err);
  if (err == NULL && next_slots > 0)
    first_next_child = tx_first_child (next, btx, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }

  /* Move children to the previous sibling. */

  while (prev_slots > 0 && num_children > 0)
    {
      btree_node *child_next = tx_next_sibling (child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      if (!tx_set_next_sibling (last_prev_child, btx, child)
	  || !tx_set_next_sibling (child, btx, NULL)
	  || !tx_set_parent (child, btx, prev)
	  || !tx_set_prev_sibling (child, btx, last_prev_child)
	  || (child_next != NULL
	      && !tx_set_prev_sibling (child_next, btx, NULL)))
	return FALSE;

      prev_slots--;
      num_children--;

      last_prev_child = child;
      child = child_next;
    }

  /* If any children were moved, the previous sibling will now have a
     different key range. Its own key needs to be updated to reflect that. */

  if (last_prev_child != NULL)
    set_node_key_to_key_after (prev, effective_key (last_prev_child, FALSE));

  /* If there are any remaining children to be moved, they must be going to the
     next sibling. Set its first child to be the first unmoved child of the node
     under merge. */

  if (num_children > 0)
    if (!tx_set_first_child (next, btx, child))
      return FALSE;

  while (next_slots > 0 && num_children > 0)
    {
      btree_node *child_next = tx_next_sibling (child, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      /* Adjust the parents of the merged node's remaining children... */
      
      if (!tx_set_parent (child, btx, next)
	  || (child_next == NULL
	      && (!tx_set_next_sibling (child, btx, first_next_child)
		  || !tx_set_prev_sibling (first_next_child, btx, child))))
	return FALSE;

      next_slots--;
      num_children--;

      child = child_next;
    }
  
  /* ...and delete it. */
  
  return mark_node_deleted (btx, node);
}

/* Performs a merge on the specified node which has fewer than its minimum
   required children; either by distributing its children to its siblings, or by
   acquiring spare children from them. The number of children of the node and
   its siblings are used to choose the best approach.

   When this function returns, the target node has either been deleted or has
   reached its minimum child threshold.
*/

static gboolean
merge (btree_node *node, unsigned int num_children, btree_transaction *btx)
{
  GError *err = NULL;
  unsigned int required_children = node->min_children - num_children;
  btree_node *next = NULL;
  btree_node *prev = NULL;
  btree_node *parent = NULL;
  unsigned int num_next = 0, spare_next = 0, next_slots = 0;
  unsigned int num_prev = 0, spare_prev = 0, prev_slots = 0;

  parent = tx_parent (node, btx, &err);
  if (err == NULL)
    next = tx_next_sibling (node, btx, &err);
  if (err == NULL && next != NULL)
    num_next = count_children (btx, next, &err);
  if (err == NULL)
    prev = tx_prev_sibling (node, btx, &err);
  if (err == NULL && prev != NULL)
    num_prev = count_children (btx, prev, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }

  if (next == NULL && prev == NULL && parent != NULL)
    {
      /* Ordinarily, a node with too few children is guaranteed to have siblings
	 it can merge with because its parent has the same commitment to 
	 maintain minimuma and maximum child counts. However, this isn't the 
	 case with interstitial children of the root, which is allowed to have a
	 single child node. So if an "only child" of the root needs a merge,
	 just delete it and attach its children directly to the root. */
      
      btree_node *child = tx_first_child (node, btx, &err);
      btree_node *child_ptr = child;

      if (mark_node_deleted (btx, node))
	{
	  while (child_ptr != NULL && err == NULL)
	    {
	      tx_set_parent (child_ptr, btx, parent);
	      child_ptr = tx_next_sibling (child_ptr, btx, &err);	  
	    }
	  
	  if (err == NULL)
	    return tx_set_first_child (parent, btx, child_ptr);
	  else g_error_free (err);
	}

      return FALSE;
    }

  spare_next = next != NULL ? num_next - next->min_children : 0;
  spare_prev = prev != NULL ? num_prev - prev->min_children : 0;
  next_slots = next != NULL ? next->max_children - num_next : 0;
  prev_slots = prev != NULL ? prev->max_children - num_prev : 0;

  if (spare_next + spare_prev >= required_children)
    {
      /* Fill in this node with spare children from adjacent nodes. */

      if (!merge_borrow
	  (prev, node, next, btx, spare_prev, num_children, spare_next))
	return FALSE;
    }
  else if (next_slots + prev_slots >= num_children)
    {
      /* Redistribute the children of this node to adjacent nodes and
	 destroy it. */

      if (!merge_distribute
	  (prev, node, next, btx, prev_slots, num_children, next_slots))
	return FALSE;
    }
  else assert (1 == 0);

  return TRUE;
}

/* Decides whether the specified B+tree node needs to be merged because it has
   fewer than the minimum allowable child nodes. If so, it is merged (via a
   call to `merge') and its parent is evaluated for the same; and so on,
   iteratively.

   Returns `TRUE' if all required merges were successful, `FALSE' otherwise.
*/

static gboolean
maybe_merge (btree_transaction *btx, btree_node *node)
{
  GError *err = NULL;
  int num_children = count_children (btx, node, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }

  while (num_children < node->min_children)
    {
      btree_node *parent = tx_parent (node, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}
      else if (parent == NULL)

	/* The root can't be merged. */

	return TRUE;

      if (!merge (node, num_children, btx))
	return FALSE;
      else if (parent == NULL)
	return TRUE;
      else node = parent;

      num_children = count_children (btx, node, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}
    }

  return TRUE;
}

/* Split the specified B+tree node by creating a new next sibling for it and 
   moving half of its children to the new sibling. Returns the newly created 
   sibling node, which is locked for write. The new sibling node's min and max
   child count is determined by the `leaf_parent' argument - internal nodes that
   are the parents of only leaf nodes may have different min / max child 
   thresholds. */ 

static btree_node *
split (btree_node *node, btree_transaction *btx, gboolean leaf_parent)
{
  GError *err = NULL;
  unsigned int split_position = 1;
  btree_node *new_node = NULL;
  
  btree_node *next = NULL;
  btree_node *parent = NULL;
  btree_node *split_point = NULL;
  btree_node *pre_split_point = NULL;

  if (leaf_parent)
    new_node = create_lockable_btree_node
      (NULL, MIN_LEAF_CHILDREN, MAX_LEAF_CHILDREN, NULL, 0, btx);
  else new_node = create_lockable_btree_node
	 (NULL, MIN_INTERNAL_CHILDREN, MAX_INTERNAL_CHILDREN, NULL, 0, btx);
  
  if (tx_lock_node (new_node, btx, TRUE, NULL) != SUCCESS)
    return NULL;

  next = tx_next_sibling (node, btx, &err);
  if (err == NULL)
    parent = tx_parent (node, btx, &err);
  if (err == NULL)
    split_point = tx_first_child (node, btx, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return NULL;
    }

  while (split_position <= node->max_children / 2)
    {
      pre_split_point = split_point;
      split_point = tx_next_sibling (split_point, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return NULL;
	}
      else split_position++;
    }

  /* Adjust the target node's key to reflect the change to its keyspace. */

  set_node_key_to_key_after (node, effective_key (pre_split_point, FALSE));

  if (!tx_set_next_sibling (pre_split_point, btx, NULL)
      || !tx_set_prev_sibling (split_point, btx, NULL))
    return NULL;

  if (!tx_set_first_child (new_node, btx, split_point)
      || !tx_set_next_sibling (new_node, btx, next)
      || !tx_set_next_sibling (node, btx, new_node)
      || !tx_set_parent (new_node, btx, parent)
      || !tx_set_prev_sibling (new_node, btx, node)
      || (next != NULL && !tx_set_prev_sibling (next, btx, new_node)))
    return NULL;

  /* Update the parent of all child nodes moved to the new sibling... */
  
  while (split_point != NULL)
    {
      pre_split_point = split_point;
      if (!tx_set_parent (split_point, btx, new_node))
	return NULL;
      split_point = tx_next_sibling (split_point, btx, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return NULL;
	}
    }

  /* ...and set the new sibling's key to the one directly after its final new
     child. 
  */
  
  set_node_key_to_key_after (new_node, effective_key (pre_split_point, FALSE));

  return new_node;
}

/* Decides whether the specified B+tree node needs to be split because it has
   more than the maximum allowable child nodes. If so, it is merged (via a
   call to `merge') and its parent is evaluated for the same; and so on,
   iteratively.

   Returns `TRUE' if all required splits were successful, `FALSE' otherwise.
*/

static gboolean
maybe_split (btree_transaction *btx, btree *btree, btree_node *node)
{
  GError *err = NULL;
  int num_children = count_children (btx, node, &err);
  gboolean leaf_parent = TRUE;
  
  if (err != NULL)
    {
      g_error_free (err);
      return FALSE;
    }

  while (num_children > node->max_children)
    {
      btree_node *parent = tx_parent (node, btx, &err);
      btree_node *new_node = NULL;

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}

      new_node = split (node, btx, leaf_parent);
      leaf_parent = FALSE;
      
      if (new_node == NULL)
	return FALSE;
      else if (parent == NULL)
	{
	  /* We just split the root. */

	  parent = create_lockable_btree_node
	    (NULL, 1, BRANCHING_FACTOR - 1, NULL, 0, btx);

	  if (tx_lock_node (parent, btx, TRUE, NULL) != SUCCESS)
	    return FALSE;

	  node->min_children = MIN_INTERNAL_CHILDREN;
	  node->max_children = MAX_INTERNAL_CHILDREN;

	  if (!tx_set_first_child (parent, btx, node)
	      || !tx_set_parent (node, btx, parent)
	      || !tx_set_parent (new_node, btx, parent)
	      || !tx_set_root (btree, btx, parent))
	    return FALSE;

	  return maybe_merge (btx, node);
	}
      else node = parent;

      num_children = count_children (btx, node, &err);

      if (err != NULL)
	{
	  g_error_free (err);
	  return FALSE;
	}
    }

  return TRUE;
}

/*
  Creates and returns a new leaf node whose page contains the specified key and
  value as its only record.
   
  The new key is inserted as a child of the specified internal parent node 
  after the specified sibling, or as the first child if the `after' argument is
  `NULL'.

  This function returns `NULL' if the necessary structural updates to the parent
  and the adjacent children could not be performed.
*/

static btree_node *
insert_leaf_with_record (btree_transaction *btx, btree *btree,
			 btree_node *parent, btree_node *after,
			 unsigned char *key, size_t key_len,
			 unsigned char *value, size_t value_len)
{
  GError *err = NULL;
  btree_node *new_child = create_lockable_btree_node
    (parent, 0, 0, NULL, 0, btx);
  
  if (tx_lock_node (new_child, btx, TRUE, NULL) != SUCCESS)
    return NULL;
  else
    {
      /* If a relative position in the child list was specified, use it.
	 Otherwise, attempt to find the parent's last child. */
      
      btree_node *prev = after != NULL
	? after : tx_last_child (parent, btx, &err);

      if (prev == NULL)
	{
	  /* If that results in `NULL' was it because there was an error
	     traversing the list? Or because the parent has no children 
	     (unlikely). */
	  
	  if (err != NULL)
	    {
	      g_error_free (err);
	      return NULL;
	    }
	  else
	    {
	      btree_node *first_child = tx_first_child (parent, btx, &err);

	      if (first_child != NULL)
		{
		  if (!tx_set_next_sibling (new_child, btx, prev)
		      || !tx_set_prev_sibling (prev, btx, new_child))
		    return NULL;
		}
	      else if (err != NULL)
		{
		  g_error_free (err);
		  return NULL;
		}	      

	      if (!tx_set_first_child (parent, btx, new_child))
		return NULL;
	    }
	}
      else
	{
	  /* If there is a previous sibling for the new leaf, there might be a
	     next sibling for it as well. */
	  
	  btree_node *next = tx_next_sibling (prev, btx, &err);
	  btree_node *new_sibling = new_child;
	  gboolean needs_merge = FALSE;
	  
	  if (effective_page (prev, FALSE) == NULL)
	    {
	      btree_node *interstitial = create_lockable_btree_node
		(parent, MIN_INTERNAL_CHILDREN, MAX_INTERNAL_CHILDREN, NULL, 0,
		 btx);

	      if (tx_lock_node (interstitial, btx, TRUE, NULL) != SUCCESS)
		return FALSE;

	      tx_set_first_child (interstitial, btx, new_child);
	      tx_set_parent (new_child, btx, interstitial);

	      new_sibling = interstitial;

	      if (1 < MIN_INTERNAL_CHILDREN)
		needs_merge = TRUE;
	    }
	  
	  if (next != NULL)
	    {
	      if (!tx_set_next_sibling (new_sibling, btx, next)
		  || !tx_set_prev_sibling (next, btx, new_sibling))
		return NULL;
	    }
	  else if (err != NULL)
	    {
	      g_error_free (err);
	      return NULL;
	    }

	  if (!tx_set_next_sibling (prev, btx, new_sibling)
	       || !tx_set_prev_sibling (new_sibling, btx, prev))
	    return NULL;

	  if (needs_merge
	      && (!merge (new_sibling, 1, btx) || !maybe_merge (btx, parent)))
	    return NULL;
	}
    }

  mark_modification (new_child, btx);
  new_child->page = calloc (1, sizeof (btree_page));

  /* Write the value to the new leaf's page. */

  insert_page_record (btx, new_child, 0, key, key_len, value, value_len, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return NULL;
    }
  
  if (!maybe_split (btx, btree, parent))
    return NULL;
  else return new_child;
}

/* Create and return a new B+tree wrapper structure. */

static btree *
create_btree (btree_environment *btree_env)
{
  btree *bt = malloc (sizeof (btree));
  btree_transaction *btx = create_transaction (btree_env, G_MAXINT64);

  bt->lock = lock_new ();
  
  /* The root node's min / max children are special. */

  bt->root = create_lockable_btree_node
    (NULL, 1, BRANCHING_FACTOR - 1, NULL, 0, btx);

  bt->new_root = NULL;

  clear_flags (bt->root);

  /* Add the B+tree to the environment. */

  g_mutex_lock (&btree_env->mutex);
  btree_env->btrees = g_list_prepend (btree_env->btrees, bt);
  g_mutex_unlock (&btree_env->mutex);

  bt->environment = btree_env;
  
  commit (btx);
  
  return bt;
}

/* The following functions implement the gzochid storage engine interface (as
   defined in `storage.h') in terms of the B+tree management routines defined
   above. */

/* Create and return a new storage context in the form of a B+tree environment.
   The `path' argument is ignored. */

static gzochid_storage_context *
initialize (char *path)
{
  gzochid_storage_context *context = malloc (sizeof (gzochid_storage_context));

  context->environment = create_btree_environment ();

  return context;
}

/* Close and clean up the specified storage context (and its associated B+tree
   environment). */

static void
close_context (gzochid_storage_context *context)
{
  close_btree_environment (context->environment);
  free (context);
}

/* This function is a no-op. */

static void
destroy_context (char *path)
{
}

/* Create and return a new store backed by a new B+tree, associated with the
   B+tree environment enclosed by the specified storage context. The `name' and
   `flags' arguments are ignored. */

static gzochid_storage_store *
open (gzochid_storage_context *context, char *name, unsigned int flags)
{
  gzochid_storage_store *store = malloc (sizeof (gzochid_storage_store));
  btree_environment *btree_env = context->environment;

  store->context = context;
  store->database = create_btree (btree_env);

  return store;
}

/* Close and clean up the specified store (and its associated B+tree). */

static void
close_store (gzochid_storage_store *store)
{
  close_btree (store->database);
  free (store);
}

/* This function is a no-op. */

static void
destroy_store (gzochid_storage_context *context, char *path)
{
}

/* Creates and returns a new transaction in the specified storage context, with
   a timeout equivalent to 2^64 - 1.
*/

static gzochid_storage_transaction *
transaction_begin (gzochid_storage_context *context)
{
  gzochid_storage_transaction *tx =
    calloc (1, sizeof (gzochid_storage_transaction));

  tx->txn = create_transaction (context->environment, G_MAXINT64);

  return tx;
}

/* Creates and returns a new transaction with the specified timeout in the 
   specified storage context.
*/

static gzochid_storage_transaction *
transaction_begin_timed (gzochid_storage_context *context, 
			 struct timeval timeout)
{
  gzochid_storage_transaction *tx = 
    calloc (1, sizeof (gzochid_storage_transaction));

  gint64 now = g_get_monotonic_time ();
  gint64 duration_usec = timeout.tv_sec * 1000000 + timeout.tv_usec;

  tx->txn = create_transaction (context->environment, now + duration_usec);

  return tx;
}

/* Returns `TRUE' if the specified transaction has not been marked for rollback
   nor has exceeded its execution time, `FALSE' otherwise. 
*/

static gboolean
check_tx (gzochid_storage_transaction *tx)
{
  if (tx->rollback)
    return FALSE;
  else if (!check_tx_timeout (tx->txn))
    {
      tx->rollback = TRUE;
      tx->should_retry = TRUE;
      return FALSE;
    }
  else return TRUE;
}

/* Marks the specified transaction for rollback. Transactions marked for 
   rollback cannot be committed. 
*/

static void
mark_for_rollback (gzochid_storage_transaction *tx, gboolean should_retry)
{
  tx->rollback = TRUE;
  tx->should_retry = should_retry;
}

/* Shared, transactional implementation of `get' and `get_for_update'. Locates
   the leaf node with the specified key and establishes a read or write lock on
   it (depending on the value of update) before returning its value.

   Returns NULL if no such leaf node exists.
*/

static char *
get_internal (gzochid_storage_transaction *tx, gzochid_storage_store *store, 
	      char *key, size_t key_len, size_t *value_len, gboolean update)
{
  btree_transaction *btx = tx->txn;
  btree_node *node = search (btx, store->database, key, key_len);

  btree_page *page = NULL;
  
  if (node == NULL)
    {
      /* `search' never returns NULL unless there was a problem with the
	 transaction. */

      g_warning ("Failed to retrieve key %s in transaction.", key);
      
      mark_for_rollback (tx, TRUE);
      return NULL;
    }
  else if (update && tx_lock_node (node, btx, TRUE, NULL) != SUCCESS)
    {
      g_warning ("Failed to lock retrieved key %s in transaction.", key);

      mark_for_rollback (tx, TRUE);
      return NULL;
    }

  page = effective_page (node, FALSE);

  if (page == NULL)
    return NULL;
  else
    {
      size_t tmp_value_len = 0;
      unsigned char *value = lookup_page_value
	(page, (unsigned char *) key, key_len, &tmp_value_len);

      if (value != NULL)
	{
	  char *ret = malloc (sizeof (char) * tmp_value_len);
	  memcpy (ret, value, tmp_value_len);
	  
	  if (value_len != NULL)
	    *value_len = tmp_value_len;

	  return ret;
	}
      else return NULL;
    }
}

/* Returns the value for the specified key or NULL if none exists. */

static char *
transaction_get (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, size_t *value_len)
{
  if (!check_tx (tx))
    return NULL;

  return get_internal (tx, store, key, key_len, value_len, FALSE);
}

/* Commits the specified transaction, which must not have been marked for
   rollback. 
*/

static void
transaction_commit (gzochid_storage_transaction *tx)
{
  assert (!tx->rollback);
  commit (tx->txn);
  free (tx);
}

/* Rolls back the specified transaction. */

static void
transaction_rollback (gzochid_storage_transaction *tx)
{
  rollback (tx->txn);
  free (tx);
}

/* This function is a no-op. */

static void
transaction_prepare (gzochid_storage_transaction *tx)
{
}

/*
  Inserts the specified key and value into the page of the target leaf node. The
  process works as follows:

  If the key already exists in the target page, it is first removed.

  If there is not enough room for the new record in the target page, the 
  contents of the page following the offset of the new record are moved to a new
  leaf node and page.
  
  If there is still not enough room for the new record in the target page, it is
  inserted as a new leaf node and page following the target leaf node (and
  preceding the new leaf node potentially created earlier). Otherwise the new
  record is inserted as the new last record in the target page.

  If any of the operations above cannot be completed (inserting new leaf nodes
  may trigger splitting of internal nodes) the provided `GError' argument is set
  accordingly.

  The caller must hold a write lock on the target node.
*/

static void
put_internal (btree_transaction *btx, btree *database, btree_node *node,
	      unsigned char *key, size_t key_len, unsigned char *value,
	      size_t value_len, GError **err)
{
  GError *local_err = NULL;
  btree_page *page = NULL;
  size_t offset = 0;
  size_t required_size = key_len + value_len + 4;

  page = effective_page (node, TRUE);
  offset = page_key_offset (page, key, key_len);

  /* Does the key already exist in the target page? */
  
  if (offset + 2 < page->page_size
      && compare_page_datum_to_key (page, offset, key, key_len) == 0)
    {
      /* If so, remove it. */
      
      remove_page_record (btx, node, offset, &local_err);
      
      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);
	  return;
	}
    }

  /* Is there room for the new record in the target page? */
  
  if (page->page_size + required_size <= MAX_PAGE_SIZE)
    {
      /* If so, write it. */
      
      insert_page_record
	(btx, node, offset, key, key_len, value, value_len, &local_err);

      if (local_err != NULL)
	g_propagate_error (err, local_err);
    }
  else
    {
      /* Otherwise, find some way to make room for the new record. */
      
      btree_node *new_leaf = NULL;
      btree_page *new_page = NULL;
      btree_node *new_parent = tx_parent (node, btx, &local_err);

      if (local_err != NULL)
	{
	  g_propagate_error (err, local_err);
	  return;	  
	}

      /* First, if there's stuff on the page that comes after the target offset,
	 try moving that to its own leaf / page. */
      
      if (offset < page->page_size)
	{      
	  size_t next_key_len =
	    gzochi_common_io_read_short (page->data, offset);
	  size_t next_value_len = gzochi_common_io_read_short
	    (page->data, offset + next_key_len + 2);
	  
	  /* Create a leaf with the first subsequent record. */
	  
	  new_leaf = insert_leaf_with_record
	    (btx, database, new_parent, node,
	     page->data + offset + 2, next_key_len,
	     page->data + offset + next_key_len + 4, next_value_len);

	  if (new_leaf == NULL)
	    {
	      g_set_error
		(err, MEMORY_TRANSACTION_ERROR, TXN_FAILED,
		 "Transaction failure while splitting page.");

	      return;
	    }

	  /* Remove that record from the source page. */
	  
	  remove_page_record (btx, node, offset, &local_err);
	      
	  if (local_err != NULL)
	    {
	      g_propagate_error (err, local_err);
	      return;
	    }
	  
	  new_page = effective_page (new_leaf, TRUE);

	  /* Then, if there's more stuff after it, transfer it. */

	  while (offset < page->page_size)
	    {
	      next_key_len = gzochi_common_io_read_short (page->data, offset);
	      next_value_len = gzochi_common_io_read_short
		(page->data, offset + next_key_len + 2);
	      	      
	      insert_page_record
		(btx, new_leaf, new_page->page_size,
		 page->data + offset + 2, next_key_len,
		 page->data + offset + next_key_len + 4, next_value_len,
		 &local_err);

	      if (local_err == NULL)
		remove_page_record (btx, node, offset, &local_err);
	      
	      if (local_err != NULL)
		{
		  g_propagate_error (err, local_err);
		  return;
		}
	    }

	  /* Did that free up enough space on the target page? */
	  
	  if (page->page_size + required_size <= MAX_PAGE_SIZE)
	    {
	      /* If so, write the new record. */
	      
	      insert_page_record
		(btx, node, offset, key, key_len, value, value_len, err);
	      return;
	    }
	}

      /* Get the parent again; it may have changed as the result of a split. */
      
      new_parent = tx_parent (node, btx, &local_err);

      if (local_err != NULL)
	g_propagate_error (err, local_err);

      /* Just give the record its own dedicated page. */
      
      else if (insert_leaf_with_record
	       (btx, database, new_parent, node, key, key_len, value,
		value_len) == NULL)
	
	g_set_error
	  (err, MEMORY_TRANSACTION_ERROR, TXN_FAILED,
	   "Transaction failure while inserting singleton record.");
    }
}

/* Inserts or updates the value for the specified key. */

static void
transaction_put (gzochid_storage_transaction *tx, gzochid_storage_store *store,
		 char *key, size_t key_len, char *value, size_t value_len)
{
  GError *err = NULL;
  btree_node *node = NULL;
  btree_transaction *btx = tx->txn;
  gboolean rollback = FALSE;
  
  if (!check_tx (tx))
    return;

  node = search (btx, store->database, key, key_len);

  if (node != NULL && tx_lock_node (node, btx, TRUE, NULL) == SUCCESS)
    {
      btree_page *page = effective_page (node, FALSE);
      
      if (page != NULL)
	{
	  put_internal
	    (btx, store->database, node, (unsigned char *) key, key_len,
	     (unsigned char *) value, value_len, &err);

	  if (err != NULL)
	    rollback = TRUE;
	}
      else
	{
	  btree_node *new_leaf = insert_leaf_with_record
	    (btx, store->database, node, NULL, (unsigned char *) key, key_len,
	     (unsigned char *) value, value_len);

	  if (new_leaf == NULL)
	    rollback = TRUE;
	}
    }
  else rollback = TRUE;

  if (rollback)
    {
      if (err != NULL)
	{
	  g_warning
	    ("Failed to store key %s in transaction: %s", key, err->message);
	  g_error_free (err);
	}
      else g_warning ("Failed to store key %s in transaction.", key);

      mark_for_rollback (tx, TRUE);
    }
}

/* Deletes the value for the specified key. Returns ENOTFOUND if no such value
   exists; ETXFAILURE if a lock could not be acquired; 0 otherwise.
*/

static int
transaction_delete (gzochid_storage_transaction *tx, 
		    gzochid_storage_store *store, char *key, size_t key_len)
{
  int ret = 0;
  btree_node *node = NULL;
  btree_transaction *btx = tx->txn;

  if (!check_tx (tx))
    return GZOCHID_STORAGE_ETXFAILURE;

  node = search (btx, store->database, key, key_len);

  if (node != NULL && tx_lock_node (node, btx, TRUE, NULL) == SUCCESS)
    {
      btree_page *page = effective_page (node, TRUE);

      if (page == NULL)
	ret = GZOCHID_STORAGE_ENOTFOUND;
      else
	{
	  GError *err = NULL;
	  btree_node *parent = NULL;	  
	  size_t offset = page_key_offset
	    (page, (unsigned char *) key, key_len);

	  if (compare_page_datum_to_key
	      (page, offset, (unsigned char *) key, key_len) != 0)
	    ret = GZOCHID_STORAGE_ENOTFOUND;
	  else
	    {
	      remove_page_record (btx, node, offset, &err);

	      if (err != NULL)
		{
		  g_error_free (err);
		  ret = GZOCHID_STORAGE_ETXFAILURE;
		}
	      else if (page->page_size == 0)
		{	  
		  parent = tx_parent (node, btx, &err);
		  
		  if (err != NULL)
		    {
		      g_error_free (err);
		      ret = GZOCHID_STORAGE_ETXFAILURE;
		    }
		  else if (mark_node_deleted (btx, node))
		    {
		      if (!maybe_merge (btx, parent))
			ret = GZOCHID_STORAGE_ETXFAILURE;
		    }
		  else ret = GZOCHID_STORAGE_ETXFAILURE;
		}
	    }
	}
    }
  else ret = GZOCHID_STORAGE_ETXFAILURE;

  if (ret == GZOCHID_STORAGE_ETXFAILURE)
    {
      g_warning ("Failed to delete key %s in transaction.", key);
      mark_for_rollback (tx, TRUE);
    }

  return ret;
}

/* Shared, transactional implementation of `first_key' and `next_key'. Locates
   the leaf node with the specified key directly after the specified key, and
   returns its value.

   Returns NULL if no such leaf node exists, such as when the store is empty or
   there is no following key.

   Also returns NULL when the required read locks could not be acquired, and
   sets the error value accordingly.
*/

static char *
find_key_gte (btree_transaction *btx, btree *bt, char *key, size_t key_len, 
	      size_t *found_key_len, GError **err)
{
  btree_node *node = search (btx, bt, key, key_len);

  if (node != NULL)
    {
      btree_page *page = effective_page (node, FALSE);

      if (page != NULL)
	{
	  int i = 0;
	  size_t offset = 0;

	  while (offset < page->page_size)
	    {
	      unsigned short len = gzochi_common_io_read_short
		(page->data, offset);
	      int c = memcmp (key, page->data + offset + 2, MIN (len, key_len));

	      if (c < 0 || (c == 0 && len >= key_len))
		{
		  char *ret = malloc (sizeof (char) * len);
		  memcpy (ret, page->data + offset + 2, len);
		  
		  if (found_key_len != NULL)
		    *found_key_len = len;

		  return ret;
		}
	      else
		{
		  offset += len + 2;
		  len = gzochi_common_io_read_short (page->data, offset);
		  offset += len + 2;
		  
		  i++;
		}
	    }
	  return NULL;
	}
      else return NULL;
    }
  else return NULL; /* The transaction is in a bad state. */
}

/* Returns the first key in the B+tree. */

static char *
transaction_first_key (gzochid_storage_transaction *tx, 
		       gzochid_storage_store *store, size_t *key_len)
{
  char *ret = NULL;
  GError *err = NULL;

  if (!check_tx (tx))
    return NULL;

  /* The first key in the store must be immediately >= '\0'. */

  ret = find_key_gte (tx->txn, store->database, "", 1, key_len, &err);

  if (err != NULL)
    {
      g_warning
	("Failed to seek to first key in transaction: %s", err->message);
      g_error_free (err);
      mark_for_rollback (tx, TRUE);
    }

  return ret;
}

/* Returns the key in the B+tree immediately after the specified key. */

static char *
transaction_next_key (gzochid_storage_transaction *tx, 
		      gzochid_storage_store *store, char *key, size_t key_len, 
		      size_t *next_key_len)
{
  GError *err = NULL;
  char *next_key = NULL;
  char *ret = NULL;

  if (!check_tx (tx))
    return NULL;

  next_key = (char *) key_after ((unsigned char *) key, key_len);

  ret = find_key_gte 
    (tx->txn, store->database, next_key, key_len + 1, next_key_len, &err);
  free (next_key);

  if (err != NULL)
    {
      g_warning ("Failed to advance cursor in transaction: %s", err->message);
      g_error_free (err);
      mark_for_rollback (tx, TRUE);
    }

  return ret;
}

/* Returns the value for the specified key or NULL if none exists. If the key
   is found, a write lock is established on it before this function returns. 
*/

static char *
transaction_get_for_update (gzochid_storage_transaction *tx, 
			    gzochid_storage_store *store, char *key, 
			    size_t key_len, size_t *value_len)
{
  if (!check_tx (tx))
    return NULL;

  return get_internal (tx, store, key, key_len, value_len, TRUE);
}

gzochid_storage_engine_interface gzochid_storage_engine_interface_mem = 
  {
    "mem",

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

/* A `GFunc' implementation to support `gzochid_data_print_btree_structure' 
   below. */

static void
print_btree_structure_visitor (gpointer data, gpointer user_data)
{
  btree_node *node = data;
  GList *lines = NULL, *lines_ptr = NULL;
  int i = 0;

  while (node != NULL)
    {
      btree_node_header *header = effective_header (node, FALSE);

      if (node == data)
	lines = g_list_append (lines, "|- ");
      else if (header->next != NULL)
	lines = g_list_append (lines, "|  ");
      else lines = g_list_append (lines, "   ");

      node = header->parent;
    }

  lines = g_list_reverse (lines);
  lines_ptr = lines;

  while (lines_ptr != NULL)
    {
      printf ("%s", (char *) lines_ptr->data);
      lines_ptr = lines_ptr->next;
    }

  g_list_free (lines);
  
  node = data;
  
  if (node->key.data == NULL)
    printf ("[ROOT]\n");
  else
    {
      for (i = 0; i < node->key.data_len; i++)
	if (isalnum (node->key.data[i]))
	  printf ("%c", node->key.data[i]);
	else printf ("\\x%.2x", node->key.data[i]);

      printf ("\n");
    }
}

/*
  Print to standard output the structure of the specified `btree', as determined
  via an "inorder" traversal, including a representation of the keys of internal
  and leaf nodes.

  Use this function within GDB to inspect the distribution of keys in the memory
  storage engine.
*/

void
gzochid_storage_mem_print_btree_structure (btree *btree)
{
  walk_btree (btree->root, print_btree_structure_visitor, NULL);
}

/*
  Print to standard output the state of the specified btree transaction in terms
  of its (pending) read and write locks against leaf and internal nodes.

  Use this function within GDB to examine deadlocks and other lock contention 
  issues.
*/

void
gzochid_storage_mem_print_transaction_state (btree_transaction *btx)
{
  GHashTableIter iter;
  gpointer key, value;
  
  g_hash_table_iter_init (&iter, btx->environment->lock_table);

  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      btree_node *node = key;
      btree_lock *lock = value;

      btree_datum *key_data = effective_key (node, FALSE);
	  
      if (lock->writer == btx)
	{
	  if (node->page != NULL)
	    printf ("WRITE: %p key %s [leaf]\n", node, key_data->data);
	  else printf ("WRITE: %p key %s [interstitial]\n", node,
		       key_data->data);
	}
      if (g_list_find (lock->readers, btx))
	{
	  if (node->page != NULL)
	    printf ("READ: %p key %s [leaf]\n", node, key_data->data);
	  else printf ("READ: %p key %s [interstitial]\n", node,
		       key_data->data);
	}
      if (btx->waiting_to_write == lock)
	{
	  if (node->page != NULL)
	    printf ("WAITING TO WRITE: %p key %s [leaf]\n", node,
		    key_data->data);
	  else printf ("WAITING TO WRITE: %p key %s [interstitial]\n", node,
		       key_data->data);
	}
      else if (btx->waiting_to_read == lock)
	{
	  if (node->page != NULL)
	    printf ("WAITING TO READ: %p key %s [leaf]\n", node,
		    key_data->data);
	  else printf ("WAITING TO READ: %p key %s [interstitial]\n", node,
		       key_data->data);
	}
    }
}
