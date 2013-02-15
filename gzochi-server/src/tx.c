/* tx.c: Application-level transactions implementation for gzochid
 * Copyright (C) 2013 Julian Graham
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
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "log.h"
#include "tx.h"
#include "util.h"

static GStaticMutex transaction_mutex = G_STATIC_MUTEX_INIT;
static GStaticPrivate thread_transaction_key = G_STATIC_PRIVATE_INIT;
static GStaticPrivate thread_transaction_timing_key = G_STATIC_PRIVATE_INIT;
static mpz_t transaction_counter;
static gboolean transactions_initialized;

enum gzochid_transaction_state
  {
    GZOCHID_TRANSACTION_STATE_ACTIVE,
    GZOCHID_TRANSACTION_STATE_PREPARING,
    GZOCHID_TRANSACTION_STATE_COMMITTING,
    GZOCHID_TRANSACTION_STATE_ROLLING_BACK,
    GZOCHID_TRANSACTION_STATE_COMMITTED,
    GZOCHID_TRANSACTION_STATE_ROLLED_BACK
  };

typedef struct _gzochid_transaction_timing
{
  struct timeval start_time;
  struct timeval *timeout;
} gzochid_transaction_timing;

typedef struct _gzochid_transaction 
{
  mpz_t id;
  char *name;
  GHashTable *participants;
  enum gzochid_transaction_state state;

  gzochid_transaction_timing *timing;
} gzochid_transaction;

static gzochid_transaction *transaction_new (void)
{
  gzochid_transaction *transaction = calloc (1, sizeof (gzochid_transaction));
  transaction->participants = g_hash_table_new_full 
    (g_str_hash, g_str_equal, NULL, free);

  g_static_mutex_lock (&transaction_mutex);
  if (!transactions_initialized)
    {
      mpz_init (transaction_counter);
      transactions_initialized = TRUE;
    }
  mpz_set (transaction->id, transaction_counter);
  mpz_add_ui (transaction_counter, transaction_counter, 1);
  g_static_mutex_unlock (&transaction_mutex);

  transaction->name = mpz_get_str (NULL, 16, transaction->id);
  transaction->state = GZOCHID_TRANSACTION_STATE_ACTIVE;

  return transaction;
}

static void transaction_free (gzochid_transaction *transaction)
{
  mpz_clear (transaction->id);
  free (transaction->name);
  g_hash_table_destroy (transaction->participants);
  free (transaction);
}

typedef struct _gzochid_transaction_participant_registration
{
  gzochid_transaction_participant *participant;

  gpointer data;
  gboolean rollback;
} gzochid_transaction_participant_registration;

gzochid_transaction_participant *gzochid_transaction_participant_new 
(char *name, gzochid_transaction_prepare prepare, 
 gzochid_transaction_commit commit, gzochid_transaction_rollback rollback)
{
  gzochid_transaction_participant *participant = calloc 
    (1, sizeof (gzochid_transaction_participant));

  participant->name = name;
  participant->prepare = prepare;
  participant->commit = commit;

  return participant;
}

void gzochid_transaction_participant_free
(gzochid_transaction_participant *participant)
{
  free (participant);
}

void gzochid_transaction_join 
(gzochid_transaction_participant *participant, gpointer user_data)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);
  if (transaction == NULL)
    {
      transaction = transaction_new ();
      transaction->timing = g_static_private_get 
	(&thread_transaction_timing_key);
      g_static_private_set (&thread_transaction_key, transaction, NULL);
    }
  else assert (transaction->state == GZOCHID_TRANSACTION_STATE_ACTIVE);

  if (g_hash_table_lookup 
      (transaction->participants, participant->name) == NULL)
    {
      gzochid_transaction_participant_registration *registration =
	calloc (1, sizeof (gzochid_transaction_participant_registration));
      
      registration->participant = participant;
      registration->data = user_data;

      g_hash_table_insert 
	(transaction->participants, participant->name, registration);

      gzochid_debug ("Participant '%s' joined transaction '%s'.", 
		     participant->name, transaction->name);
    }
}

gpointer gzochid_transaction_context 
(gzochid_transaction_participant *participant)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);
  gzochid_transaction_participant_registration *registration = NULL;

  assert (transaction != NULL);

  registration = (gzochid_transaction_participant_registration *)
    g_hash_table_lookup (transaction->participants, participant->name);
  return registration == NULL ? NULL : registration->data;
}

static int prepare (gzochid_transaction *transaction)
{
  GList *participants = g_hash_table_get_values (transaction->participants);
  GList *participant_ptr = participants;

  transaction->state = GZOCHID_TRANSACTION_STATE_PREPARING;
  gzochid_debug ("Preparing transaction '%s' for commit.", transaction->name);
  
  while (participant_ptr != NULL)
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) participant_ptr->data;
      if (!participant->participant->prepare (participant->data))
	{
	  gzochid_info 
	    ("Participant '%s' in transaction '%s' failed to prepare.",
	     participant->participant->name, transaction->name);

	  g_list_free (participants);	  
	  return FALSE;
	}

      participant_ptr = participant_ptr->next;
    }

  g_list_free (participants);
  return TRUE;
}

static void commit (gzochid_transaction *transaction)
{
  GList *participants = g_hash_table_get_values (transaction->participants);
  GList *participant_ptr = participants;

  transaction->state = GZOCHID_TRANSACTION_STATE_COMMITTING;

  while (participant_ptr != NULL)
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) participant_ptr->data;
      participant->participant->commit (participant->data);
      participant_ptr = participant_ptr->next;
    }

  transaction->state = GZOCHID_TRANSACTION_STATE_COMMITTED;
  gzochid_debug ("Committed transaction '%s'.", transaction->name);

  g_list_free (participants);
}

static gboolean rollback_only (gzochid_transaction *transaction)
{
  GHashTableIter iter;
  gpointer key, value;
  
  g_hash_table_iter_init (&iter, transaction->participants);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) value;
      if (participant->rollback)
	return TRUE;
    }

  return FALSE;
}

static void rollback (gzochid_transaction *transaction)
{
  GList *participants = g_hash_table_get_values (transaction->participants);
  GList *participant_ptr = participants;

  transaction->state = GZOCHID_TRANSACTION_STATE_ROLLING_BACK;
  
  while (participant_ptr != NULL)
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) participant_ptr->data;
      participant->participant->rollback (participant->data);
      participant_ptr = participant_ptr->next;
    }

  transaction->state = GZOCHID_TRANSACTION_STATE_ROLLED_BACK;
  gzochid_debug ("Rolled back transaction '%s'.", transaction->name);

  g_list_free (participants);
}

void gzochid_transaction_mark_for_rollback 
(gzochid_transaction_participant *participant)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);  

  gzochid_transaction_participant_registration *registration =
    (gzochid_transaction_participant_registration *) 
    g_hash_table_lookup (transaction->participants, participant->name);

  assert (registration != NULL);
  registration->rollback = TRUE;
}

static struct timeval time_remaining (gzochid_transaction_timing *timing)
{
  struct timeval now, ret, zero = { 0, 0 };

  if (timing->timeout == NULL)
    return zero;

  gettimeofday (&now, NULL);
  timersub (&now, &timing->start_time, &ret);
  if (timercmp (&ret, timing->timeout, >))
    ret = zero;
  else 
    {
      struct timeval ret2;
      timersub (timing->timeout, &ret, &ret2);
      ret = ret2;
    }

  return ret; 
}

struct timeval gzochid_transaction_time_remaining ()
{
  gzochid_transaction_timing *timing = g_static_private_get 
    (&thread_transaction_timing_key);

  assert (timing != NULL);

  return time_remaining (timing);  
}

static int transaction_execute
(void (*func) (gpointer), gpointer data, struct timeval *timeout)
{
  gzochid_transaction *transaction = NULL;
  gzochid_transaction_timing timing;
  gboolean success = TRUE;
  struct timeval tx_finish;
  int ms = 0;

  /* Set up the transaction timing information here, even before any
     participants have joined - the idea being that if a transaction does end
     up getting created because a participant requiring transactional semantics
     joins, then this we should consider it as having begun now. */

  gettimeofday (&timing.start_time, NULL);
  timing.timeout = timeout;

  g_static_private_set (&thread_transaction_timing_key, &timing, NULL);

  func (data);

  g_static_private_set (&thread_transaction_timing_key, NULL, NULL);

  transaction = g_static_private_get (&thread_transaction_key);
  if (transaction == NULL)
    return TRUE;

  if (rollback_only (transaction) || !prepare (transaction))
    {
      rollback (transaction);
      success = FALSE;
    }
  else 
    {
      struct timeval tx_finish;
      struct timeval remaining = time_remaining (&timing);

      if (timing.timeout != NULL 
	  && remaining.tv_sec <= 0 
	  && remaining.tv_usec <= 0)
	{
	  rollback (transaction);
	  success = FALSE;
	}
      else commit (transaction);
	  
      gettimeofday (&tx_finish, NULL);
      ms = ((tx_finish.tv_sec - timing.start_time.tv_sec) * 1000)
	+ ((tx_finish.tv_usec - timing.start_time.tv_usec) / 1000);
      
      if (success)
	gzochid_debug ("Transaction completed in %dms", ms);
      else gzochid_warning ("Transaction timed out after %dms", ms);
    }

  g_static_private_set (&thread_transaction_key, NULL, NULL);

  transaction_free (transaction);
  return success;
}

int gzochid_transaction_execute (void (*func) (gpointer), gpointer data)
{
  return transaction_execute (func, data, NULL);
}

int gzochid_transaction_execute_timed
(void (*func) (gpointer), gpointer data, struct timeval timeout)
{
  return transaction_execute (func, data, &timeout);
}

gboolean gzochid_transaction_active ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);
  return transaction != NULL 
    && transaction->state == GZOCHID_TRANSACTION_STATE_ACTIVE;
}

gboolean gzochid_transaction_rollback_only ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);

  assert (transaction != NULL);

  return rollback_only (transaction);
}

gboolean gzochid_transaction_timed_out ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_static_private_get (&thread_transaction_key);

  assert (transaction != NULL);

  if (transaction->timing->timeout != NULL)
    {
      struct timeval remaining = gzochid_transaction_time_remaining ();
      return remaining.tv_sec <= 0 && remaining.tv_usec <= 0;
    }
  else return FALSE;
}

gboolean gzochid_transaction_timed ()
{
  gzochid_transaction_timing *timing = g_static_private_get 
    (&thread_transaction_timing_key);

  assert (timing != NULL);
  return timing->timeout != NULL;
}
