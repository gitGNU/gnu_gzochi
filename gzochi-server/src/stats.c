/* stats.c: Application statistics management routines for gzochid
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
#include <glib-object.h>
#include <sys/time.h>

#include "event.h"
#include "stats.h"

static void
update_from_data_event (gzochid_application_stats *stats,
			gzochid_data_event_type type, GzochidDataEvent *event)
{
  guint64 bytes = 0;

  g_object_get (event, "bytes", &bytes, NULL);
  
  switch (type)
    {
    case BYTES_READ: stats->bytes_read += bytes; break;
    case BYTES_WRITTEN: stats->bytes_written += bytes; break;
    default: assert (1 == 0);
    }
}

static void
update_from_event (gzochid_application_stats *stats,
		   gzochid_event_type type, GzochidEvent *event)
{
  switch (type)
    {
    case MESSAGE_RECEIVED: stats->num_messages_received++; break;
    case MESSAGE_SENT: stats->num_messages_sent++; break;
    case TRANSACTION_START: stats->num_transactions_started++; break;
    default: assert (1 == 0);
    }
}

static void
update_from_transaction_event (gzochid_application_stats *stats,
			       gzochid_transaction_event_type type,
			       GzochidTransactionEvent *event)
{
  guint64 duration_us = 0;
  guint64 duration_ms = 0;
  
  g_object_get (event, "duration-us", &duration_us, NULL);

  duration_ms = duration_us / 1000;
  
  switch (type)
    {
    case TRANSACTION_COMMIT: 
      stats->num_transactions_committed++;
      
      if (stats->num_transactions_committed == 1
	  || duration_ms > stats->max_transaction_duration)
	stats->max_transaction_duration = duration_ms;
      if (stats->num_transactions_committed == 1
	  || duration_ms < stats->min_transaction_duration)
	stats->min_transaction_duration = duration_ms;

      if (stats->num_transactions_committed == 1)
	stats->average_transaction_duration = duration_ms;
      else stats->average_transaction_duration = 
	     (duration_ms + ((stats->num_transactions_committed - 1)
			     * stats->average_transaction_duration))
	     / stats->num_transactions_committed;
      
      break;
    case TRANSACTION_ROLLBACK: stats->num_transactions_rolled_back++; break;
    default: assert (1 == 0);
    }
}

void
gzochid_stats_update_from_event (gzochid_application_stats *stats,
				 GzochidEvent *event)
{
  int type = 0;
  GType event_type = G_OBJECT_TYPE (event);

  g_object_get (event, "type", &type, NULL);
  
  if (event_type == GZOCHID_TYPE_DATA_EVENT)
    update_from_data_event (stats, type, GZOCHID_DATA_EVENT (event));
  else if (event_type == GZOCHID_TYPE_EVENT)
    update_from_event (stats, type, event);
  else if (event_type == GZOCHID_TYPE_TRANSACTION_EVENT)
    update_from_transaction_event
      (stats, type, GZOCHID_TRANSACTION_EVENT (event));
}
