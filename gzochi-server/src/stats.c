/* stats.c: Application statistics management routines for gzochid
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

#include <assert.h>
#include <sys/time.h>

#include "event.h"
#include "stats.h"

static void
update_from_data_event (gzochid_application_stats *stats,
			gzochid_application_event_type type,
			gzochid_application_data_event *event)
{
  switch (type)
    {
    case BYTES_READ: stats->bytes_read += event->bytes; break;
    case BYTES_WRITTEN: stats->bytes_written += event->bytes; break;
    default: assert (1 == 0);
    }
}

static void
update_from_event (gzochid_application_stats *stats,
		   gzochid_application_event_type type,
		   gzochid_application_event *event)
{
  switch (type)
    {
    case TRANSACTION_START: stats->num_transactions_started++; break;
    default: assert (1 == 0);
    }
}

static long
timeval_to_milliseconds (struct timeval tv)
{
  return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

static void
update_from_transaction_event (gzochid_application_stats *stats,
			       gzochid_application_event_type type,
			       gzochid_application_transaction_event *event)
{
  long transaction_ms = 0;

  switch (type)
    {
    case TRANSACTION_START: stats->num_transactions_started++; break;
    case TRANSACTION_COMMIT: 
      stats->num_transactions_committed++;
      
      transaction_ms = timeval_to_milliseconds (event->duration);

      if (stats->num_transactions_committed == 1
	  || transaction_ms > stats->max_transaction_duration)
	stats->max_transaction_duration = transaction_ms;
      if (stats->num_transactions_committed == 1
	  || transaction_ms < stats->min_transaction_duration)
	stats->min_transaction_duration = transaction_ms;

      if (stats->num_transactions_committed == 1)
	stats->average_transaction_duration = transaction_ms;
      else stats->average_transaction_duration = 
	     (transaction_ms + 
	      ((stats->num_transactions_committed - 1)
	       * stats->average_transaction_duration))
	     / stats->num_transactions_committed;
      
      break;
    case TRANSACTION_ROLLBACK: stats->num_transactions_rolled_back++; break;
    default: assert (1 == 0);
    }
}

void
gzochid_stats_update_from_event (gzochid_application_stats *stats,
				 gzochid_application_event *event)
{
  switch (event->type)
    {
    case BYTES_READ:
    case BYTES_WRITTEN:
      update_from_data_event
	(stats, event->type, (gzochid_application_data_event *) event);
      break;

    case TRANSACTION_START:
      update_from_event	(stats, event->type, event);
      break;
      
    case TRANSACTION_COMMIT:
    case TRANSACTION_ROLLBACK:
      update_from_transaction_event
	(stats, event->type, (gzochid_application_transaction_event *) event);
      break;
    }
}
