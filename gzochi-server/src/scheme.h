/* scheme.h: Prototypes and declarations for scheme.c
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

#ifndef GZOCHID_SCHEME_H
#define GZOCHID_SCHEME_H

#include <glib.h>
#include <gmp.h>
#include <libguile.h>

#include "app.h"
#include "auth.h"
#include "channel.h"
#include "data.h"
#include "io.h"
#include "session.h"
#include "task.h"

SCM gzochid_scheme_scm_module_gzochi_private_app;

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
gzochid_io_serialization gzochid_scheme_data_serialization;
gzochid_application_task_serialization gzochid_scheme_task_serialization;

SCM gzochid_scheme_invoke 
(gzochid_application_context *, gzochid_auth_identity *, char *, GList *, SCM, 
 SCM);

gzochid_application_task *gzochid_scheme_task_new
(gzochid_application_context *, gzochid_auth_identity *, char *, GList *, SCM);

SCM gzochid_scheme_glist_to_list (GList *, SCM (*) (gpointer));
GList *gzochid_scheme_list_to_glist (SCM, gpointer (*) (SCM));

SCM gzochid_scheme_string_hash;
SCM gzochid_scheme_string_equiv;
SCM gzochid_scheme_ghashtable_to_hashtable 
(GHashTable *, SCM, SCM, SCM (*) (gpointer), SCM (*) (gpointer));
GHashTable *gzochid_scheme_hashtable_to_ghashtable 
(SCM, GHashFunc, GEqualFunc, gpointer (*) (SCM), gpointer (*) (SCM));

SCM gzochid_scheme_create_callback (gzochid_application_callback *, ...);
char *gzochid_scheme_callback_procedure (SCM);
GList *gzochid_scheme_callback_module (SCM);
SCM gzochid_scheme_callback_data (SCM);

SCM gzochid_scheme_r6rs_raise (SCM);
SCM gzochid_scheme_make_name_exists_condition (char *);
SCM gzochid_scheme_make_name_not_bound_condition (char *);
SCM gzochid_scheme_make_object_removed_condition (void);

SCM gzochid_scheme_handler_received_message (SCM);
SCM gzochid_scheme_handler_disconnected (SCM);

SCM gzochid_scheme_create_bytevector (unsigned char *, size_t);
SCM gzochid_scheme_create_client_session (gzochid_client_session *, mpz_t);
SCM gzochid_scheme_create_managed_hashtable (GHashTable *);
SCM gzochid_scheme_create_managed_reference (gzochid_data_managed_reference *);
SCM gzochid_scheme_create_channel (gzochid_channel *, mpz_t);
SCM gzochid_scheme_create_periodic_task_handle (mpz_t);

void gzochid_scheme_managed_reference_oid (SCM, mpz_t);
void gzochid_scheme_client_session_oid (SCM, mpz_t);
void gzochid_scheme_channel_oid (SCM, mpz_t);
void gzochid_scheme_task_handle_oid (SCM, mpz_t);

void gzochid_scheme_initialize_bindings (void);

#endif /* GZOCHID_SCHEME_H */
