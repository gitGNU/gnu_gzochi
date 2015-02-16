/* tx.h: Prototypes and declarations for tx.c
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

#ifndef GZOCHID_TX_H
#define GZOCHID_TX_H

#include <glib.h>
#include <sys/time.h>

typedef int (*gzochid_transaction_prepare) (gpointer);
typedef void (*gzochid_transaction_commit) (gpointer);
typedef void (*gzochid_transaction_rollback) (gpointer);

/* Timing information for a transaction to be executed. Pass this structure to
   `gzochid_transaction_begin'. */

struct _gzochid_transaction_timing
{
  struct timeval start_time; /* Set by `gzochid_transaction_begin'. */
  struct timeval *timeout; /* The transaction timeout. May be NULL. */
};

typedef struct _gzochid_transaction_timing gzochid_transaction_timing;

enum _gzochid_transaction_result
  {
    GZOCHID_TRANSACTION_PENDING = 0,
    GZOCHID_TRANSACTION_SUCCESS = 1,
    GZOCHID_TRANSACTION_FAILURE = 2,
    GZOCHID_TRANSACTION_SHOULD_RETRY = 3
  };

typedef enum _gzochid_transaction_result gzochid_transaction_result;

struct _gzochid_transaction_participant
{
  char *name;

  gzochid_transaction_prepare prepare;
  gzochid_transaction_commit commit;
  gzochid_transaction_rollback rollback;
};

typedef struct _gzochid_transaction_participant gzochid_transaction_participant;

gzochid_transaction_participant *gzochid_transaction_participant_new 
(char *, gzochid_transaction_prepare, gzochid_transaction_commit,
 gzochid_transaction_rollback);

void gzochid_transaction_participant_free (gzochid_transaction_participant *);

void gzochid_transaction_join (gzochid_transaction_participant *, gpointer);
gpointer gzochid_transaction_context (gzochid_transaction_participant *);

/* Set up the thread-local dynamic state for a new transaction, using the
   specified `gzochid_transaction_timing' structure, on which this function sets
   the start time. 
*/
void gzochid_transaction_begin (gzochid_transaction_timing *);

/* Clean up the thread-local dynamic state for the current transaction, which
   must have initiated with a call to `gzochid_transaction_begin' above, and
   retrieves the transaction result (success, rollback, retry) from the
   thread-local transaction state.
*/
gzochid_transaction_result gzochid_transaction_end ();

/* Short-hand for calling `gzochid_transaction_begin' followed by invoking the
   specified function on the specified data followed by calling 
   `gzochid_transaction_end'. This is the easiest way of executing an untimed
   transaction.
*/
gzochid_transaction_result gzochid_transaction_execute 
(void (*) (gpointer), gpointer);

/* Short-hand for calling `gzochid_transaction_begin' with a timing argument
   followed by invoking the specified function on the specified data followed by
   calling `gzochid_transaction_end'. This is the easiest way of executing a
   timed transaction.
*/
gzochid_transaction_result gzochid_transaction_execute_timed 
(void (*) (gpointer), gpointer, struct timeval);

gboolean gzochid_transaction_active (void);
gboolean gzochid_transaction_rollback_only (void);

gboolean gzochid_transaction_timed (void);
gboolean gzochid_transaction_timed_out (void);
struct timeval gzochid_transaction_time_remaining (void);

void gzochid_transaction_mark_for_rollback 
(gzochid_transaction_participant *, gboolean);

#endif /* GZOCHID_TX_H */
