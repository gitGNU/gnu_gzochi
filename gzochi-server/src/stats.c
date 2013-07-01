/* stats.c: Application statistics management routines for gzochid
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

#include "event.h"
#include "stats.h"

void gzochid_stats_update_from_event 
(gzochid_application_stats *stats, gzochid_application_event *event)
{
  switch (event->type)
    {
    case TRANSACTION_START: stats->num_transactions_started++; break;
    case TRANSACTION_COMMIT: stats->num_transactions_committed++; break;
    case TRANSACTION_ROLLBACK: stats->num_transactions_rolled_back++; break;
    }
}
