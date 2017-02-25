/* log.c: Log configuration routines for gzochid
 * Copyright (C) 2017 Julian Graham
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

#include "log.h"

#define GZOCHID_LOG_LEVEL_MASK (G_LOG_LEVEL_MASK | GZOCHID_LOG_LEVEL_TRACE)

struct _priority_mask_holder
{
  GLogLevelFlags priority_mask;
};

typedef struct _priority_mask_holder priority_mask_holder;

static priority_mask_holder *default_holder;

static void
log_handler (const gchar *log_domain, GLogLevelFlags log_level,
	     const gchar *message, gpointer user_data)
{
  const char *severity = "";
  struct timeval tv;
  struct tm ltm;

  if (!gzochid_log_level_visible (log_domain, log_level))
    return;

  gettimeofday (&tv, NULL);
  localtime_r (&tv.tv_sec, &ltm);

  if (log_level & G_LOG_LEVEL_ERROR ||
      log_level & G_LOG_LEVEL_CRITICAL)
    severity = "ERR";
  else if (log_level & G_LOG_LEVEL_WARNING)
    severity = "WARNING";
  else if (log_level & G_LOG_LEVEL_MESSAGE)
    severity = "NOTICE";
  else if (log_level & G_LOG_LEVEL_INFO)
    severity = "INFO";
  else if (log_level & G_LOG_LEVEL_DEBUG)
    severity = "DEBUG";
  else if (log_level & GZOCHID_LOG_LEVEL_TRACE)
    severity = "TRACE";

  fprintf (stderr, "%d-%.2d-%.2dT%.2d:%.2d,%dZ %s %s\n", 1900 + ltm.tm_year, 
	   ltm.tm_mon + 1, ltm.tm_mday, ltm.tm_hour, ltm.tm_min, 
	   (int) (tv.tv_usec / 1000), severity, message);
}

void gzochid_install_log_handler (GLogLevelFlags log_level)
{
  GLogLevelFlags priority_mask = GZOCHID_LOG_LEVEL_MASK;
  
  assert (default_holder == NULL);
  
  if (log_level & G_LOG_LEVEL_DEBUG)
    priority_mask = priority_mask & ~GZOCHID_LOG_LEVEL_TRACE;
  if (log_level & G_LOG_LEVEL_INFO)
    priority_mask = priority_mask &
      ~(G_LOG_LEVEL_DEBUG | GZOCHID_LOG_LEVEL_TRACE);
  else if (log_level & G_LOG_LEVEL_MESSAGE)
    priority_mask = priority_mask &
      ~(G_LOG_LEVEL_INFO | G_LOG_LEVEL_DEBUG | GZOCHID_LOG_LEVEL_TRACE);
  else if (log_level & G_LOG_LEVEL_WARNING)
    priority_mask =
      G_LOG_LEVEL_ERROR | G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING;
  else if (log_level & G_LOG_LEVEL_CRITICAL)
    priority_mask = G_LOG_LEVEL_ERROR | G_LOG_LEVEL_CRITICAL;
  else if (log_level & G_LOG_LEVEL_ERROR)
    priority_mask = G_LOG_LEVEL_ERROR;

  default_holder = malloc (sizeof (priority_mask_holder));
  default_holder->priority_mask = priority_mask;
  
  g_log_set_default_handler (log_handler, NULL);
}

gboolean
gzochid_log_level_visible (const gchar *log_domain, GLogLevelFlags log_level)
{
  const gchar *domains = NULL;

  if (default_holder == NULL)
    return FALSE;

  /* Replicate behavior of default logger with respect to printing debug 
     messages. */
  
  domains = g_getenv ("G_MESSAGES_DEBUG");

  return log_level & GZOCHID_LOG_LEVEL_MASK & default_holder->priority_mask
    || (domains != NULL && ((log_domain != NULL && strstr (domains, log_domain))
			    || strcmp (domains, "all") == 0));
}
