/* scheme-task.c: Scheme callback handlers and interface to gzochi Scheme API
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
#include <gmp.h>
#include <gzochi-common.h>
#include <libguile.h>
#include <stddef.h>
#include <stdlib.h>

#include "app.h"
#include "app-task.h"
#include "callback.h"
#include "data.h"
#include "durable-task.h"
#include "guile.h"
#include "gzochid-auth.h"
#include "io.h"
#include "log.h"
#include "reloc.h"
#include "scheme.h"
#include "session.h"
#include "tx.h"
#include "txlog.h"

#include "api/channel.h"
#include "api/data.h"
#include "api/log.h"
#include "api/session.h"
#include "api/task.h"
#include "api/tx.h"
#include "api/util.h"

static SCM gzochid_scheme_string_hash;
static SCM gzochid_scheme_string_equiv;

static SCM scm_client_session_listener_p;

static GList *gzochi_private_app;
static GList *gzochi_private_task;

static int scheme_prepare (gpointer data) { return TRUE; }
static void scheme_commit (gpointer data) { }
static void scheme_rollback (gpointer data) { }

static gzochid_transaction_participant scheme_participant = 
  { "scheme", scheme_prepare, scheme_commit, scheme_rollback };

void 
gzochid_scheme_application_worker (gzochid_application_context *context, 
				   gzochid_auth_identity *identity, 
				   gpointer ptr)
{
  void **data = ptr;

  SCM proc = data[0];
  SCM args = data[1];
  SCM exception_var = data[2];
  SCM *ret = data[3];

  gzochid_transaction_join (&scheme_participant, NULL);

  *ret = gzochid_scheme_invoke (context, identity, proc, args, exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)

    if (! gzochid_scheme_triggered_by_rollback 
	(scm_variable_ref (exception_var)))

      gzochid_transaction_mark_for_rollback 
	(&scheme_participant, 
	 gzochid_scheme_is_transaction_retry 
	 (scm_variable_ref (exception_var)));
}

void 
gzochid_scheme_application_task_worker (gzochid_application_context *context, 
					gzochid_auth_identity *identity, 
					gpointer task)
{
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  gzochid_transaction_join (&scheme_participant, NULL);

  gzochid_scheme_invoke_callback 
    (context, 
     identity,
     "gzochi:run-task", 
     gzochi_private_task,
     scm_list_1 (task), 
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)

    if (! gzochid_scheme_triggered_by_rollback 
	(scm_variable_ref (exception_var)))

      gzochid_transaction_mark_for_rollback 
	(&scheme_participant,
	 gzochid_scheme_is_transaction_retry 
	 (scm_variable_ref (exception_var)));
}

void 
gzochid_scheme_application_initialized_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  GHashTable *properties = data;
  gzochid_data_managed_reference *callback_reference = NULL;
  SCM callback = gzochid_scheme_create_callback 
    (context->descriptor->initialized, NULL);
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, callback, NULL);
  
  /* Creating a reference to a new object requires that the transaction be in a
     good state, but there's no reason it shouldn't be healthy during 
     initialization. */

  assert (callback_reference != NULL); 
  
  gzochid_transaction_join (&scheme_participant, NULL);

  gzochid_scheme_invoke_callback 
    (context, identity, "gzochi:execute-initialized", gzochi_private_app,
     scm_list_2 (callback,
		 gzochid_scheme_ghashtable_to_hashtable 
		 (properties, 
		  gzochid_scheme_string_hash,
		  gzochid_scheme_string_equiv,
		  (SCM (*) (gpointer)) scm_from_locale_string,
		  (SCM (*) (gpointer)) scm_from_locale_string)),
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! gzochid_scheme_triggered_by_rollback 
	  (scm_variable_ref (exception_var)))

	gzochid_transaction_mark_for_rollback 
	  (&scheme_participant,
	   gzochid_scheme_is_transaction_retry 
	   (scm_variable_ref (exception_var)));
    }
  else gzochid_data_set_binding_to_oid 
	 (context, "s.initializer", callback_reference->oid, NULL);
}

static gzochid_application_callback *
scm_to_callback (gzochid_application_context *context, SCM scm_callback)
{
  GList *module = gzochid_scheme_callback_module (scm_callback);
  char *procedure = gzochid_scheme_callback_procedure (scm_callback);
  gzochid_scm_location_info *scm_callback_reloc = gzochid_scm_location_get
    (context, scm_callback);
  
  gzochid_data_managed_reference *reference = gzochid_data_create_reference
    (context, &gzochid_scm_location_aware_serialization, scm_callback_reloc,
     NULL);

  if (reference == NULL) /* Can happen on transaction timeout. */
    return NULL;
  else return gzochid_application_callback_new
	 (procedure, module, reference->oid);
}

static gpointer 
unpack_handler (gpointer data)
{
  SCM handler = data;
  SCM received_message = gzochid_scheme_handler_received_message (handler);
  SCM disconnected = gzochid_scheme_handler_disconnected (handler);

  gzochid_application_context *context = 
    gzochid_get_current_application_context ();
  gzochid_client_session_handler *session_handler = 
    malloc (sizeof (gzochid_client_session_handler));

  scm_gc_protect_object (received_message);
  scm_gc_protect_object (disconnected);

  session_handler->received_message = scm_to_callback 
    (context, received_message);
  session_handler->disconnected = scm_to_callback (context, disconnected);

  return session_handler;
}

static gboolean
is_client_session_listener (SCM obj)
{
  return scm_is_true (scm_call_1 (scm_client_session_listener_p, obj));
}

void 
gzochid_scheme_application_logged_in_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  GError *err = NULL;
  mpz_t session_oid;
  char *session_oid_str = data;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;

  SCM scm_session = SCM_BOOL_F;

  SCM cb = SCM_BOOL_F;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM handler = SCM_BOOL_F;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
    
  mpz_init (session_oid);
  mpz_set_str (session_oid, session_oid_str, 16);
  session_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_client_session_serialization, session_oid);
  mpz_clear (session_oid);

  gzochid_data_dereference (session_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  session = session_reference->obj;

  scm_session = gzochid_scheme_create_client_session 
    (session, session_reference->oid);

  cb = gzochid_scheme_create_callback (context->descriptor->logged_in, NULL);
  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, cb, NULL);

  /* If the transaction fails here (or has already failed) just return. */
  
  if (callback_reference == NULL)
    return;
  
  gzochid_transaction_join (&scheme_participant, NULL);

  handler = gzochid_scheme_invoke_callback 
    (context, identity, "gzochi:execute-logged-in", gzochi_private_app,
     scm_list_2 (callback_reference->obj, scm_session), exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! gzochid_scheme_triggered_by_rollback 
	  (scm_variable_ref (exception_var)))

	gzochid_transaction_mark_for_rollback 
	  (&scheme_participant, 
	   gzochid_scheme_is_transaction_retry 
	   (scm_variable_ref (exception_var)));
    }
  else if (scm_is_false (handler))
    {
      gzochid_client_session_send_login_failure (context, session);
      gzochid_client_session_disconnect (context, session);
    }
  else if (! is_client_session_listener (handler))
    {
      g_warning ("Invalid type returned by logged-in callback.");
      gzochid_transaction_mark_for_rollback
	(&scheme_participant, FALSE);
    }
  else
    {
      gzochid_scm_location_info *scm_handler_reloc = 
	gzochid_scm_location_get (context, handler);
      gzochid_data_managed_reference *handler_reference = 
	gzochid_data_create_reference 
	(context, &gzochid_scm_location_aware_serialization, scm_handler_reloc,
	 NULL);

      scm_gc_protect_object (handler);
      
      gzochid_client_session_set_handler
	(session, gzochid_with_application_context 
	 (context, identity, unpack_handler, handler));
      gzochid_client_session_set_handler_scm_oid
	(session, handler_reference->oid);
      
      gzochid_data_mark 
	(context, &gzochid_client_session_serialization, session, &err);

      if (err == NULL)
	{
	  gzochid_scm_location_info *scm_session_reloc = 
	    gzochid_scm_location_get (context, scm_session);
	  gzochid_data_managed_reference *reloc_reference = 
	    gzochid_data_create_reference 
	    (context, &gzochid_scm_location_aware_serialization, 
	     scm_session_reloc, &err);

	  if (err == NULL)
	    {
	      gzochid_client_session_set_scm_oid
		(session, reloc_reference->oid);
	      gzochid_client_session_send_login_success (context, session);
	    }
	}
      
      if (err != NULL)
	g_error_free (err);
    }
}

void 
gzochid_scheme_application_received_message_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  GError *err = NULL;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM bv = SCM_BOOL_F;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  unsigned char *message = NULL;
  short *message_len_ptr = NULL;
  void **data = ptr;

  mpz_t session_oid;
  mpz_init (session_oid);
  mpz_set_str (session_oid, data[0], 16);

  message = data[1];
  message_len_ptr = data[2];

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);

  mpz_clear (session_oid);

  gzochid_data_dereference (session_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  session = session_reference->obj;

  bv = gzochid_scheme_create_bytevector (message, *message_len_ptr);
  callback_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization,
     gzochid_client_session_get_handler (session)->received_message->scm_oid);

  gzochid_data_dereference (callback_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }
      
  gzochid_scheme_invoke_callback
    (context, identity, "gzochi:execute-received-message", gzochi_private_app,
     scm_list_2 (callback_reference->obj, bv), exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! gzochid_scheme_triggered_by_rollback 
	  (scm_variable_ref (exception_var)))
	{
	  gzochid_transaction_join (&scheme_participant, NULL);
	  gzochid_transaction_mark_for_rollback 
	    (&scheme_participant, 
	     gzochid_scheme_is_transaction_retry 
	     (scm_variable_ref (exception_var)));
	}
    }
}

void 
gzochid_scheme_application_disconnected_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  GError *err = NULL;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;
  gzochid_client_session_handler *handler = NULL;
  
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  char *oid_str = ptr;
  mpz_t session_oid;

  mpz_init (session_oid);
  mpz_set_str (session_oid, oid_str, 16);

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);
  mpz_clear (session_oid);

  /* Obtain a preemptive write lock on the session object, as it's most likely 
     going to be removed later in the same transaction. */
  
  gzochid_data_dereference_for_update (session_reference, &err);

  if (err != NULL)
    {
      if (err->message != NULL)
	g_warning
	  ("Failed to dereference disconnecting session '%s': %s", 
	   oid_str, err->message);
      else g_warning 
	     ("Failed to dereference disconnecting session '%s'.", oid_str);

      g_error_free (err);
      return;
    }

  session = session_reference->obj;
  handler = gzochid_client_session_get_handler (session);
  
  /* A client may disconnect before the login process has completed or after
     it has failed enough times to stop being retried. */

  if (handler == NULL)
    {
      /* In that case, don't bother trying to dredge up its disconnect handler.
	 Just jump straight to removing the session from the data store. */
      
      gzochid_tx_warning 
	(context, "Session '%s' disconnected after incomplete login.", ptr);
      gzochid_client_session_disconnected_worker (context, identity, ptr);
      
      return;
    }

  callback_reference = gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization,
     handler->disconnected->scm_oid);

  /* Mark the callback for update; if all goes well, it'll get removed below. */
  
  gzochid_data_dereference_for_update (callback_reference, &err);
  
  if (err != NULL)
    {
      if (err->message != NULL)
	g_warning
	  ("Failed to retrieve disconnect callback for session '%s': %s", 
	   oid_str, err->message);
      else g_warning 
	     ("Failed to retrieve disconnect callback for session '%s'.", 
	      oid_str);

      g_error_free (err);      
      return;
    }

  gzochid_scheme_invoke_callback
    (context, identity, "gzochi:execute-disconnected", gzochi_private_app,
     scm_list_1 (callback_reference->obj), exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! gzochid_scheme_triggered_by_rollback 
	  (scm_variable_ref (exception_var)))
	{
	  gzochid_transaction_join (&scheme_participant, NULL);
	  gzochid_transaction_mark_for_rollback 
	    (&scheme_participant, 
	     gzochid_scheme_is_transaction_retry 
	     (scm_variable_ref (exception_var)));
	}
    }
  else
    {
      gzochid_data_remove_object (callback_reference, &err);

      if (err == NULL)
	{
	  callback_reference = gzochid_data_create_reference_to_oid
	    (context, &gzochid_scheme_data_serialization, 
	     handler->received_message->scm_oid);

	  gzochid_data_remove_object (callback_reference, &err);
	}
      
      if (err != NULL)
	{
	  g_warning
	    ("Failed to remove lifecycle handler for session '%s': %s", oid_str,
	     err->message);
	  g_error_free (err);
	}
      else gzochid_client_session_disconnected_worker (context, identity, ptr);
    }
}

void 
gzochid_scheme_application_disconnected_cleanup_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  free (ptr);
}

void
gzochid_scheme_application_ready (gzochid_application_context *context, 
				  gzochid_auth_identity *identity, GError **err)
{
  SCM callback = gzochid_scheme_create_callback 
    (context->descriptor->ready, NULL);
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);

  /* This handler is called with a "current application" and identity, but isn't
     called within a transaction and doesn't attempt to join one. */
     
  gzochid_scheme_invoke_callback 
    (context, identity, "gzochi:execute-ready", gzochi_private_app,
     scm_list_2 (callback,
		 gzochid_scheme_ghashtable_to_hashtable 
		 (context->descriptor->properties, 
		  gzochid_scheme_string_hash,
		  gzochid_scheme_string_equiv,
		  (SCM (*) (gpointer)) scm_from_locale_string,
		  (SCM (*) (gpointer)) scm_from_locale_string)),
     exception_var);

  /* Bridge any exception raised to the `GError' argument. */

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED,
		 "Failed to invoke ready handler.");
}

static void 
scheme_worker_serializer (gzochid_application_context *context, 
			  gzochid_application_worker worker, GString *out)
{
}

static gzochid_application_worker 
scheme_worker_deserializer (gzochid_application_context *context, GString *in)
{
  return gzochid_scheme_application_task_worker;
}

static gzochid_application_worker_serialization scheme_worker_serialization =
  { scheme_worker_serializer, scheme_worker_deserializer };

gzochid_application_task_serialization gzochid_scheme_task_serialization = 
  { 
    "scheme", 
    &scheme_worker_serialization, 
    &gzochid_scheme_data_serialization 
  };

gzochid_application_task *
gzochid_scheme_task_new (gzochid_application_context *context, 
			 gzochid_auth_identity *identity, char *procedure, 
			 GList *module_name, SCM data)
{
  SCM scm_data = gzochid_scheme_invoke_callback 
    (context,
     NULL,
     "gzochi:make-callback",
     g_list_append (g_list_append (NULL, "gzochi"), "app"), 
     scm_cons 
     (scm_from_locale_symbol (procedure),
      scm_cons (gzochid_scheme_glist_to_list 
		(module_name, (SCM (*) (gpointer)) scm_from_locale_symbol),
		data)), 
     SCM_BOOL_F);

  gzochid_application_task *task = NULL; 

  scm_gc_protect_object (scm_data);
  task = gzochid_application_task_new
    (context, identity, gzochid_scheme_application_task_worker, scm_data);
  
  return task;
}

static void 
bind_scm (char *module, SCM *binding, char *name)
{
  SCM var = scm_module_variable
    (scm_c_resolve_module (module), scm_from_locale_symbol (name));

  if (scm_is_false (var))
    g_error ("Missing Scheme binding for `%s'. Aborting...", name);

  *binding = scm_variable_ref (var);
  scm_gc_protect_object (*binding);
}

static void *
initialize_bindings (void *ptr)
{
  bind_scm ("gzochi session", &scm_client_session_listener_p, 
	    "gzochi:client-session-listener?");

  bind_scm ("rnrs base", &gzochid_scheme_string_equiv, "equal?");
  bind_scm ("rnrs hashtables", &gzochid_scheme_string_hash, "string-hash");

  gzochid_api_channel_init ();
  gzochid_api_data_init ();
  gzochid_api_log_init ();
  gzochid_api_session_init ();
  gzochid_api_task_init ();
  gzochid_api_tx_init ();
  gzochid_api_util_init ();
  
  return NULL;
}

void 
gzochid_scheme_task_initialize_bindings (void)
{
  gzochi_private_app = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");
  gzochi_private_task = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "task");

  scm_with_guile (initialize_bindings, NULL);
}
