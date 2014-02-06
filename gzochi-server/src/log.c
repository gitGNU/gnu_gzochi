/* log.c: Log-writing routines for gzochid
 * Copyright (C) 2012 Julian Graham
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
#include <stdarg.h>
#include <stdio.h>
#include <sys/time.h>
#include <syslog.h>
#include <time.h>

#include "log.h"

static GStaticMutex log_mutex = G_STATIC_MUTEX_INIT;

static int max_priority = LOG_INFO;

void gzochid_vlog (int priority, char *msg, va_list ap)
{
  va_list app;
  char *severity = NULL;
  struct timeval tv;
  struct tm ltm;

  if (priority > max_priority)
    return;

  va_copy (app, ap);
  gettimeofday (&tv, NULL);
  localtime_r (&tv.tv_sec, &ltm);

  switch (priority)
    {
    case LOG_ERR: severity = "ERR"; break;
    case LOG_WARNING: severity = "WARNING"; break;
    case LOG_NOTICE: severity = "NOTICE"; break;
    case LOG_INFO: severity = "INFO"; break;
    case LOG_DEBUG: severity = "DEBUG"; break;
    default: break;
    }

  vsyslog (priority, msg, app);
  va_end (app);
  va_copy (app, ap);

  g_static_mutex_lock (&log_mutex);
  fprintf (stderr, "%d-%.2d-%.2dT%.2d:%.2d,%dZ ", 1900 + ltm.tm_year, 
	   ltm.tm_mon + 1, ltm.tm_mday, ltm.tm_hour, ltm.tm_min, 
	   (int) (tv.tv_usec / 1000));
  fprintf (stderr, "%s ", severity);
  vfprintf (stderr, msg, app);
  va_end (app);
  fprintf (stderr, "\n");
  g_static_mutex_unlock (&log_mutex);
}

void gzochid_log (int priority, char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (priority, msg, args);
  va_end (args);
}

void gzochid_err (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (LOG_ERR, msg, args);
  va_end (args);
}

void gzochid_warning (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (LOG_WARNING, msg, args);
  va_end (args);
}

void gzochid_notice (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (LOG_NOTICE, msg, args);
  va_end (args);
}

void gzochid_info (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (LOG_INFO, msg, args);
  va_end (args);
}

void gzochid_debug (char *msg, ...)
{
  va_list args;
  va_start (args, msg);
  gzochid_vlog (LOG_DEBUG, msg, args);
  va_end (args);
}

void gzochid_set_log_threshold (int priority)
{
  max_priority = priority;
}
