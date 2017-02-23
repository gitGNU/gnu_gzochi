/* scheme-task.h: Prototypes and declarations for scheme-task.c
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

#ifndef GZOCHID_SCHEME_TASK_H
#define GZOCHID_SCHEME_TASK_H

#include <glib.h>
#include <libguile.h>

#include "app.h"
#include "app-task.h"
#include "callback.h"
#include "durable-task.h"
#include "gzochid-auth.h"
#include "io.h"

void gzochid_scheme_application_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_scheme_application_task_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_scheme_application_initialized_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_scheme_application_logged_in_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_scheme_application_received_message_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);
void gzochid_scheme_application_disconnected_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

/* Transactional application worker to be used as a cleanup mechanism if 
   `gzochid_scheme_application_disconnected_worker' cannot commit its 
   transaction. Invokes `gzochid_client_session_disconnected_worker' and frees
   the session key. */

void gzochid_scheme_application_disconnected_cleanup_worker
(gzochid_application_context *, gzochid_auth_identity *, gpointer);

/* Called to invoke an application's optional "ready" lifecycle handler. */

void gzochid_scheme_application_ready (gzochid_application_context *, 
				       gzochid_auth_identity *, GError **);

extern gzochid_application_task_serialization
gzochid_scheme_task_serialization;

gzochid_application_task *gzochid_scheme_task_new
(gzochid_application_context *, gzochid_auth_identity *, char *, GList *, SCM);

void gzochid_scheme_task_initialize_bindings (void);

#endif /* GZOCHID_SCHEME_TASK_H */
