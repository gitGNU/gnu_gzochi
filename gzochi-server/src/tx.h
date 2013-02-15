/* tx.h: Prototypes and declarations for tx.c
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

#ifndef GZOCHID_TX_H
#define GZOCHID_TX_H

#include <glib.h>
#include <sys/time.h>

typedef int (*gzochid_transaction_prepare) (gpointer);
typedef void (*gzochid_transaction_commit) (gpointer);
typedef void (*gzochid_transaction_rollback) (gpointer);

typedef struct _gzochid_transaction_participant
{
  char *name;

  gzochid_transaction_prepare prepare;
  gzochid_transaction_commit commit;
  gzochid_transaction_rollback rollback;
} gzochid_transaction_participant;

gzochid_transaction_participant *gzochid_transaction_participant_new 
(char *, gzochid_transaction_prepare, gzochid_transaction_commit,
 gzochid_transaction_rollback);
void gzochid_transaction_participant_free (gzochid_transaction_participant *);

void gzochid_transaction_join (gzochid_transaction_participant *, gpointer);
gpointer gzochid_transaction_context (gzochid_transaction_participant *);
int gzochid_transaction_execute (void (*) (gpointer), gpointer);
int gzochid_transaction_execute_timed 
(void (*) (gpointer), gpointer, struct timeval);

gboolean gzochid_transaction_active (void);
gboolean gzochid_transaction_rollback_only (void);

gboolean gzochid_transaction_timed (void);
gboolean gzochid_transaction_timed_out (void);
struct timeval gzochid_transaction_time_remaining (void);


#endif /* GZOCHID_TX_H */
