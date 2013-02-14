/* log.c: Primitive functions for user-facing gzochid transactional log API
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

#include <libguile.h>
#include <string.h>
#include <syslog.h>

#include "../app.h"
#include "../scheme.h"
#include "../txlog.h"

#include "log.h"
#include "util.h"

SCM_DEFINE (primitive_log, "primitive-log", 2, 0, 0, (SCM priority, SCM msg), 
	    "Log a message for the current application")
{
  gzochid_application_context *context = 
    gzochid_api_ensure_current_application_context ();
  char *cpriority = scm_to_locale_string (scm_symbol_to_string (priority));
  char *cmsg = scm_to_locale_string (msg);

  if (strcmp (cpriority, "err") == 0)
    gzochid_tx_log (context, LOG_ERR, cmsg);
  else if (strcmp (cpriority, "warning") == 0)
    gzochid_tx_log (context, LOG_WARNING, cmsg);
  else if (strcmp (cpriority, "notice") == 0)
    gzochid_tx_log (context, LOG_NOTICE, cmsg);
  else if (strcmp (cpriority, "info") == 0)
    gzochid_tx_log (context, LOG_INFO, cmsg);
  else if (strcmp (cpriority, "debug") == 0)
    gzochid_tx_log (context, LOG_DEBUG, cmsg);

  free (cpriority);
  free (cmsg);

  gzochid_api_check_transaction ();
  
  return SCM_UNSPECIFIED;
}

void gzochid_api_log_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_log = scm_c_resolve_module ("gzochi private log");
  scm_set_current_module (gzochi_private_log);

  #include "log.x"

  scm_set_current_module (current_module);
}
