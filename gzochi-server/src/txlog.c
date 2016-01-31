/* txlog.c: Transactional log routines for gzochid
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

#include <glib.h>
#include <gzochi-common.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <syslog.h>

#include "app.h"
#include "tx.h"
#include "txlog.h"

struct _gzochid_log_message 
{
  GLogLevelFlags priority;
  char *msg;
};

typedef struct _gzochid_log_message gzochid_log_message;

struct _gzochid_log_transaction_context
{
  gzochid_application_context *context;
  GList *messages;
};

typedef struct _gzochid_log_transaction_context gzochid_log_transaction_context;

static gzochid_log_transaction_context *
create_transaction_context (gzochid_application_context *context)
{
  gzochid_log_transaction_context *tx_context =
    calloc (1, sizeof (gzochid_log_transaction_context));
  tx_context->context = context;
  return tx_context;
}

static gzochid_log_message *
gzochid_log_message_new (int priority, char *msg)
{
  gzochid_log_message *message = malloc (sizeof (gzochid_log_message));

  message->priority = priority;
  message->msg = msg;

  return message;
}

static void
gzochid_log_message_free (gzochid_log_message *message)
{
  g_free (message->msg);
  free (message);
}

static int
log_prepare (gpointer data)
{
  return TRUE;
}

static void
cleanup_transaction (gzochid_log_transaction_context *tx_context)
{
  g_list_free_full
    (tx_context->messages, (void (*) (gpointer)) gzochid_log_message_free);
  free (tx_context);
}

static void
commit_message (gpointer data, gpointer user_data)
{
  gzochid_log_message *message = data;
  g_log (G_LOG_DOMAIN, message->priority, "%s", message->msg);
}

static void
log_commit (gpointer data)
{
  gzochid_log_transaction_context *tx_context = data;
  
  g_list_foreach 
    (tx_context->messages, (void (*) (gpointer, gpointer)) commit_message, 
     NULL);
  cleanup_transaction (tx_context);
}

static void
log_rollback (gpointer data)
{
  gzochid_log_transaction_context *tx_context = data;

  cleanup_transaction (tx_context);
}

static gzochid_transaction_participant log_participant =
  { "log", log_prepare, log_commit, log_rollback };

static gzochid_log_transaction_context *
join_transaction (gzochid_application_context *context)
{
  if (!gzochid_transaction_active()
      || gzochid_transaction_context (&log_participant) == NULL)
    gzochid_transaction_join
      (&log_participant, create_transaction_context (context));
  return gzochid_transaction_context (&log_participant);
}

static void
gzochid_tx_vlog (gzochid_application_context *context, int priority, char *msg,
		 va_list ap)
{
  va_list app;
  va_copy (app, ap);
  gzochid_log_transaction_context *tx_context = join_transaction (context);

  tx_context->messages = g_list_append 
    (tx_context->messages, 
     gzochid_log_message_new (priority, g_strdup_vprintf (msg, app)));

  va_end (app);
}

void
gzochid_tx_log (gzochid_application_context *context, GLogLevelFlags priority,
		char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, priority, msg, args);
  va_end (args);
}

void
gzochid_tx_err (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, G_LOG_LEVEL_CRITICAL, msg, args);
  va_end (args);
}

void
gzochid_tx_warning (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, G_LOG_LEVEL_WARNING, msg, args);
  va_end (args);
}

void
gzochid_tx_notice (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, G_LOG_LEVEL_MESSAGE, msg, args);
  va_end (args);
}

void
gzochid_tx_info (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, G_LOG_LEVEL_INFO, msg, args);
  va_end (args);
}

void
gzochid_tx_debug (gzochid_application_context *context, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_tx_vlog (context, G_LOG_LEVEL_DEBUG, msg, args);
  va_end (args);
}
