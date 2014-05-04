/* mock-tx.c: Test-time replacements for tx.c routines.
 * Copyright (C) 2014 Julian Graham
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
#include <sys/time.h>

#include "../tx.h"

static gboolean transaction_active = FALSE;
static gboolean rollback_only = FALSE;

static gzochid_transaction_participant *participant = NULL;
static gpointer context = NULL;

static gboolean timed = FALSE;
static struct timeval start_time;
static struct timeval timeout;

void
gzochid_transaction_join 
(gzochid_transaction_participant *p, gpointer data)
{
  participant = p;
  context = data;
}

void
gzochid_transaction_mark_for_rollback 
(gzochid_transaction_participant *p, gboolean retryable)
{
  rollback_only = TRUE;
}

gboolean 
gzochid_transaction_active ()
{
  return transaction_active;
}

gpointer 
gzochid_transaction_context (gzochid_transaction_participant *p)
{
  assert (participant == NULL || participant == p);

  return context;
}

gboolean 
gzochid_transaction_rollback_only ()
{
  return rollback_only;
}

gboolean 
gzochid_transaction_timed ()
{
  return timed;
}

struct timeval 
gzochid_transaction_time_remaining ()
{
  struct timeval now, ret, zero = { 0, 0 };

  assert (timed);

  gettimeofday (&now, NULL);
  timersub (&now, &start_time, &ret);
  if (timercmp (&ret, &timeout, >))
    ret = zero;
  else 
    {
      struct timeval ret2;
      timersub (&timeout, &ret, &ret2);
      ret = ret2;
    }

  return ret; 
}

static gzochid_transaction_result
transaction_execute (void (*func) (gpointer), gpointer data)
{
  gboolean ret = GZOCHID_TRANSACTION_SUCCESS;

  transaction_active = TRUE;
  rollback_only = FALSE;

  func (data);

  if (rollback_only)
    {
      participant->rollback (context);
      rollback_only = FALSE;
      ret = GZOCHID_TRANSACTION_FAILURE;
    }
  else 
    {
      participant->prepare (context);

      if (rollback_only)
	{
	  participant->rollback (context);
	  rollback_only = FALSE;
	  ret = GZOCHID_TRANSACTION_FAILURE;
	}
      else participant->commit (context);
    }

  transaction_active = FALSE;
  participant = NULL;
  context = NULL;

  return ret;
}

gzochid_transaction_result
gzochid_transaction_execute (void (*func) (gpointer), gpointer data)
{
  timed = FALSE;
  return transaction_execute (func, data);
}

gzochid_transaction_result
gzochid_transaction_execute_timed 
(void (*func) (gpointer), gpointer data, struct timeval tm)
{
  timed = TRUE;
  gettimeofday (&start_time, NULL);
  timeout = tm;

  return transaction_execute (func, data);
}
