/* tx.c: Application-level transactions implementation for gzochid
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

#include <assert.h>
#include <glib.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>

#include "tx.h"
#include "util.h"

static GMutex transaction_mutex;
static GPrivate thread_transaction_key;
static GPrivate thread_transaction_timing_key;
static guint64 transaction_counter;

enum gzochid_transaction_state
  {
    GZOCHID_TRANSACTION_STATE_ACTIVE,
    GZOCHID_TRANSACTION_STATE_PREPARING,
    GZOCHID_TRANSACTION_STATE_COMMITTING,
    GZOCHID_TRANSACTION_STATE_ROLLING_BACK,
    GZOCHID_TRANSACTION_STATE_COMMITTED,
    GZOCHID_TRANSACTION_STATE_ROLLED_BACK
  };

struct _gzochid_transaction 
{
  guint64 id;
  char *name;
  GHashTable *participants;
  enum gzochid_transaction_state state;

  gzochid_transaction_timing *timing;
};

typedef struct _gzochid_transaction gzochid_transaction;

static gzochid_transaction *
transaction_new (void)
{
  gzochid_transaction *transaction = calloc (1, sizeof (gzochid_transaction));
  transaction->participants = g_hash_table_new_full 
    (g_str_hash, g_str_equal, NULL, free);

  g_mutex_lock (&transaction_mutex);
  transaction->id = transaction_counter++;
  g_mutex_unlock (&transaction_mutex);

  transaction->name = g_strdup_printf ("%" G_GUINT64_FORMAT, transaction->id);
  transaction->state = GZOCHID_TRANSACTION_STATE_ACTIVE;

  return transaction;
}

static void 
transaction_free (gzochid_transaction *transaction)
{
  g_free (transaction->name);
  g_hash_table_destroy (transaction->participants);
  free (transaction);
}

struct _gzochid_transaction_participant_registration
{
  gzochid_transaction_participant *participant;

  gpointer data;
  gboolean rollback;
  gboolean retryable;
};

typedef struct _gzochid_transaction_participant_registration
gzochid_transaction_participant_registration;

gzochid_transaction_participant *
gzochid_transaction_participant_new (char *name, 
				     gzochid_transaction_prepare prepare, 
				     gzochid_transaction_commit commit, 
				     gzochid_transaction_rollback rollback)
{
  gzochid_transaction_participant *participant = calloc 
    (1, sizeof (gzochid_transaction_participant));

  participant->name = name;
  participant->prepare = prepare;
  participant->commit = commit;

  return participant;
}

void 
gzochid_transaction_participant_free
(gzochid_transaction_participant *participant)
{
  free (participant);
}

void 
gzochid_transaction_join (gzochid_transaction_participant *participant, 
			  gpointer user_data)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);
  if (transaction == NULL)
    {
      transaction = transaction_new ();
      transaction->timing = g_private_get (&thread_transaction_timing_key);
      g_private_set (&thread_transaction_key, transaction);
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

      g_debug ("Participant '%s' joined transaction '%s'.", participant->name,
	       transaction->name);
    }
}

gpointer 
gzochid_transaction_context (gzochid_transaction_participant *participant)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);
  gzochid_transaction_participant_registration *registration = NULL;

  assert (transaction != NULL);

  registration = (gzochid_transaction_participant_registration *)
    g_hash_table_lookup (transaction->participants, participant->name);
  return registration == NULL ? NULL : registration->data;
}

static int 
prepare (gzochid_transaction *transaction)
{
  GList *participants = g_hash_table_get_values (transaction->participants);
  GList *participant_ptr = participants;

  transaction->state = GZOCHID_TRANSACTION_STATE_PREPARING;
  g_debug ("Preparing transaction '%s' for commit.", transaction->name);
  
  while (participant_ptr != NULL)
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) participant_ptr->data;
      if (!participant->participant->prepare (participant->data))
	{
	  g_message 
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

static void 
commit (gzochid_transaction *transaction)
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
  g_debug ("Committed transaction '%s'.", transaction->name);

  g_list_free (participants);
}

static gboolean 
rollback_only (gzochid_transaction *transaction)
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

static gboolean 
retryable (gzochid_transaction *transaction)
{
  GHashTableIter iter;
  gpointer key, value;
  
  g_hash_table_iter_init (&iter, transaction->participants);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      gzochid_transaction_participant_registration *participant = 
	(gzochid_transaction_participant_registration *) value;
      if (participant->rollback && !participant->retryable)
	return FALSE;
    }

  return TRUE;
}

static void 
rollback (gzochid_transaction *transaction)
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
  g_debug ("Rolled back transaction '%s'.", transaction->name);

  g_list_free (participants);
}

void 
gzochid_transaction_mark_for_rollback 
(gzochid_transaction_participant *participant, gboolean retryable)
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);  

  gzochid_transaction_participant_registration *registration =
    (gzochid_transaction_participant_registration *) 
    g_hash_table_lookup (transaction->participants, participant->name);

  assert (registration != NULL);
  registration->rollback = TRUE;
  registration->retryable = retryable;
}

static struct timeval 
time_remaining (gzochid_transaction_timing *timing)
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

struct timeval 
gzochid_transaction_time_remaining ()
{
  gzochid_transaction_timing *timing = g_private_get 
    (&thread_transaction_timing_key);

  assert (timing != NULL);

  return time_remaining (timing);  
}

void 
gzochid_transaction_begin (gzochid_transaction_timing *timing)
{  
 /* Set up the transaction timing information here, even before any
     participants have joined - the idea being that if a transaction does end
     up getting created because a participant requiring transactional semantics
     joins, then this we should consider it as having begun now. */
 
  gettimeofday (&timing->start_time, NULL);
  g_private_set (&thread_transaction_timing_key, timing);
}

gzochid_transaction_result
gzochid_transaction_end ()
{
  gboolean success = TRUE;
  gboolean should_retry = FALSE;
  int ms = 0;

  gzochid_transaction *transaction = NULL;
  gzochid_transaction_timing *timing = g_private_get 
    (&thread_transaction_timing_key);

  g_private_set (&thread_transaction_timing_key, NULL);

  transaction = g_private_get (&thread_transaction_key);
  if (transaction == NULL)
    return GZOCHID_TRANSACTION_SUCCESS;

  if (rollback_only (transaction) || !prepare (transaction))
    {
      rollback (transaction);
      success = FALSE;
    }
  else 
    {
      struct timeval tx_finish;
      struct timeval remaining = time_remaining (timing);

      if (timing->timeout != NULL 
	  && remaining.tv_sec <= 0 
	  && remaining.tv_usec <= 0)
	{
	  rollback (transaction);
	  success = FALSE;
	  should_retry = TRUE;
	}
      else commit (transaction);
	  
      gettimeofday (&tx_finish, NULL);
      ms = ((tx_finish.tv_sec - timing->start_time.tv_sec) * 1000)
	+ ((tx_finish.tv_usec - timing->start_time.tv_usec) / 1000);
      
      if (success)
	g_debug ("Transaction completed in %dms", ms);
      else g_warning ("Transaction failed after %dms", ms);
    }

  g_private_set (&thread_transaction_key, NULL);
  
  if (!success)
    should_retry |= retryable (transaction);
  transaction_free (transaction);
  
  if (success)
    return GZOCHID_TRANSACTION_SUCCESS;
  else return should_retry 
	 ? GZOCHID_TRANSACTION_SHOULD_RETRY
	 : GZOCHID_TRANSACTION_FAILURE;    
}

gzochid_transaction_result 
gzochid_transaction_execute (void (*func) (gpointer), gpointer data)
{
  gzochid_transaction_timing timing;
  timing.timeout = NULL;

  gzochid_transaction_begin (&timing);
  func (data);

  return gzochid_transaction_end ();
}

gzochid_transaction_result 
gzochid_transaction_execute_timed (void (*func) (gpointer), gpointer data, 
				   struct timeval timeout)
{
  gzochid_transaction_timing timing;
  timing.timeout = &timeout;

  gzochid_transaction_begin (&timing);
  func (data);

  return gzochid_transaction_end ();
}

gboolean 
gzochid_transaction_active ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);
  return transaction != NULL 
    && transaction->state == GZOCHID_TRANSACTION_STATE_ACTIVE;
}

gboolean 
gzochid_transaction_rollback_only ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);

  assert (transaction != NULL);

  return rollback_only (transaction);
}

gboolean 
gzochid_transaction_timed_out ()
{
  gzochid_transaction *transaction = (gzochid_transaction *) 
    g_private_get (&thread_transaction_key);

  assert (transaction != NULL);

  if (transaction->timing->timeout != NULL)
    {
      struct timeval remaining = gzochid_transaction_time_remaining ();
      return remaining.tv_sec <= 0 && remaining.tv_usec <= 0;
    }
  else return FALSE;
}

gboolean 
gzochid_transaction_timed ()
{
  gzochid_transaction_timing *timing = g_private_get 
    (&thread_transaction_timing_key);

  assert (timing != NULL);
  return timing->timeout != NULL;
}

