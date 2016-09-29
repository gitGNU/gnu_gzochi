/* stats.h: Prototypes and declarations for stats.c
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

#ifndef GZOCHID_STATS_H
#define GZOCHID_STATS_H

#include "event.h"

struct _gzochid_application_stats
{
  unsigned int num_messages_received;
  unsigned int num_messages_sent;
  
  unsigned int num_transactions_started;
  unsigned int num_transactions_committed;
  unsigned int num_transactions_rolled_back;

  unsigned long max_transaction_duration;
  unsigned long min_transaction_duration;
  double average_transaction_duration;

  unsigned long bytes_read;
  unsigned long bytes_written;
};

typedef struct _gzochid_application_stats gzochid_application_stats;

void gzochid_stats_update_from_event
(gzochid_application_stats *, GzochidEvent *);

#endif /* GZOCHID_STATS_H */
