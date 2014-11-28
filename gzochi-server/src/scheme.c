/* scheme.c: Supplementary Scheme routines for gzochid
 * Copyright (C) 2014 Julian Graham
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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "auth.h"
#include "data.h"
#include "guile.h"
#include "io.h"
#include "log.h"
#include "reloc.h"
#include "scheme.h"
#include "session.h"
#include "task.h"
#include "tx.h"
#include "txlog.h"

#include "api/channel.h"
#include "api/data.h"
#include "api/log.h"
#include "api/session.h"
#include "api/task.h"
#include "api/util.h"

static SCM scm_make_callback;
static SCM scm_callback_module;
static SCM scm_callback_procedure;
static SCM scm_callback_data;

static SCM scm_make_managed_hashtable;

static SCM scm_hashtable_keys;
static SCM scm_hashtable_set_x;
static SCM scm_hashtable_ref;
static SCM scm_make_hashtable;

static SCM scm_make_managed_reference;
static SCM scm_managed_record_p;
static SCM scm_managed_record_serialize;
static SCM scm_managed_record_deserialize;
static SCM scm_managed_reference_oid;

static SCM scm_handler_received_message;
static SCM scm_handler_disconnected;

static SCM scm_make_client_session;
static SCM scm_client_session_oid;

static SCM scm_make_channel;
static SCM scm_channel_oid;

static SCM scm_make_task_handle;
static SCM scm_task_handle_oid;

static SCM scm_r6rs_raise;
static SCM scm_r6rs_raise_continuable;

static SCM scm_make_object_removed_condition;
static SCM scm_make_name_exists_condition;
static SCM scm_make_name_not_bound_condition;
static SCM scm_transaction_retry_condition_p;
static SCM scm_transaction_aborted_condition_p;

static SCM resolve_procedure (char *procedure, GList *module)
{
  SCM scm_procedure = scm_from_locale_symbol (procedure);
  SCM scm_module = gzochid_scheme_glist_to_list 
    (module, (SCM (*) (gpointer)) scm_from_locale_symbol);
  return scm_public_ref (scm_module, scm_procedure);
}

static gpointer scheme_invoke_inner (gpointer data)
{
  void **ptr = (void **) data;

  SCM procedure = (SCM) ptr[0];
  SCM args = (SCM) ptr[1];
  SCM exception_var = (SCM) ptr[2];

  return gzochid_guile_invoke (procedure, args, exception_var);
}

static SCM scheme_invoke 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 SCM proc, SCM args, SCM exception_var)
{
  void *data[3];
  SCM ret = SCM_EOL;

  data[0] = proc;
  data[1] = args;
  data[2] = exception_var;

  ret = (SCM) gzochid_with_application_context 
    (context, identity, scheme_invoke_inner, data);

  scm_remember_upto_here_1 (proc);
  scm_remember_upto_here_1 (args);
  scm_remember_upto_here_1 (exception_var);

  return ret;
}

SCM gzochid_scheme_invoke
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 char *procedure, GList *module, SCM args, SCM exception_var)
{
  return scheme_invoke 
    (context, identity, resolve_procedure (procedure, module), args, 
     exception_var);
}

static int scheme_prepare (gpointer data) { return TRUE; }
static void scheme_commit (gpointer data) { }
static void scheme_rollback (gpointer data) { }

static gzochid_transaction_participant scheme_participant = 
  { "scheme", scheme_prepare, scheme_commit, scheme_rollback };

static gboolean is_transaction_retry (SCM cond)
{
  return scm_is_true (scm_call_1 (scm_transaction_retry_condition_p, cond));
}

static gboolean is_transaction_aborted (SCM cond)
{
  return scm_is_true (scm_call_1 (scm_transaction_aborted_condition_p, cond));
}

static gboolean triggered_by_rollback (SCM cond)
{
  return is_transaction_aborted (cond) && gzochid_transaction_rollback_only ();
}

void gzochid_scheme_application_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  void **data = (void **) ptr;

  SCM proc = (SCM) data[0];
  SCM args = (SCM) data[1];
  SCM exception_var = (SCM) data[2];
  SCM *ret = (SCM *) data[3];

  gzochid_transaction_join (&scheme_participant, NULL);

  *ret = scheme_invoke (context, identity, proc, args, exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    if (! triggered_by_rollback (scm_variable_ref (exception_var)))
      gzochid_transaction_mark_for_rollback 
	(&scheme_participant, 
	 is_transaction_retry (scm_variable_ref (exception_var)));
}

void gzochid_scheme_application_task_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer task)
{
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *args = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "task");

  gzochid_transaction_join (&scheme_participant, NULL);

  gzochid_scheme_invoke 
    (context, 
     identity,
     "gzochi:run-task", 
     args,
     scm_list_1 ((SCM) task), 
     exception_var);

  g_list_free (args);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    if (! triggered_by_rollback (scm_variable_ref (exception_var)))
      gzochid_transaction_mark_for_rollback 
	(&scheme_participant,
	 is_transaction_retry (scm_variable_ref (exception_var)));
}

static gzochid_application_callback *scm_to_callback 
(gzochid_application_context *context, SCM scm_callback)
{
  GList *module = gzochid_scheme_callback_module (scm_callback);
  char *procedure = gzochid_scheme_callback_procedure (scm_callback);
  gzochid_data_managed_reference *reference = 
    gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, scm_callback);

  mpz_t oid;
  
  mpz_init (oid);
  mpz_set (oid, reference->oid);      
    
  return gzochid_application_callback_new (procedure, module, oid);
}

static SCM gzochid_scheme_string_hash;
static SCM gzochid_scheme_string_equiv;

void gzochid_scheme_application_initialized_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  GHashTable *properties = (GHashTable *) data;
  gzochid_data_managed_reference *callback_reference = NULL;
  SCM callback = gzochid_scheme_create_callback 
    (context->descriptor->initialized, NULL);
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *args = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, callback);

  gzochid_transaction_join (&scheme_participant, NULL);

  gzochid_scheme_invoke 
    (context,
     identity,
     "gzochi:execute-initialized",
     args,
     scm_list_2 (callback,
		 gzochid_scheme_ghashtable_to_hashtable 
		 (properties, 
		  gzochid_scheme_string_hash,
		  gzochid_scheme_string_equiv,
		  (SCM (*) (gpointer)) scm_from_locale_string,
		  (SCM (*) (gpointer)) scm_from_locale_string)),
     exception_var);

  g_list_free (args);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! triggered_by_rollback (scm_variable_ref (exception_var)))
	gzochid_transaction_mark_for_rollback 
	  (&scheme_participant,
	   is_transaction_retry (scm_variable_ref (exception_var)));
    }
  else gzochid_data_set_binding_to_oid 
	 (context, "s.initializer", callback_reference->oid, NULL);
}

static gpointer unpack_handler (gpointer data)
{
  SCM handler = (SCM) data;
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

void gzochid_scheme_application_logged_in_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer data)
{
  GError *err = NULL;
  mpz_t session_oid;
  char *session_oid_str = (char *) data;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;

  SCM scm_session = SCM_BOOL_F;

  SCM cb = SCM_BOOL_F;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM handler = SCM_BOOL_F;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *args = NULL;
    
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

  args = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  session = (gzochid_client_session *) session_reference->obj;

  scm_session = gzochid_scheme_create_client_session 
    (session, session_reference->oid);

  cb = gzochid_scheme_create_callback (context->descriptor->logged_in, NULL);
  callback_reference = gzochid_data_create_reference 
    (context, &gzochid_scheme_data_serialization, cb);

  gzochid_transaction_join (&scheme_participant, NULL);

  handler = gzochid_scheme_invoke 
    (context,
     identity,
     "gzochi:execute-logged-in",
     args,
     scm_list_2 ((SCM) callback_reference->obj, (SCM) scm_session), 
     exception_var);
  
  g_list_free (args);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! triggered_by_rollback (scm_variable_ref (exception_var)))
	gzochid_transaction_mark_for_rollback 
	  (&scheme_participant, 
	   is_transaction_retry (scm_variable_ref (exception_var)));
    }
  else if (scm_is_false (handler))
    {
      gzochid_client_session_send_login_failure (context, session);
      gzochid_client_session_disconnect (context, session);
    }
  else
    {
      session->handler = (gzochid_client_session_handler *) 
	gzochid_with_application_context 
	(context, identity, unpack_handler, handler);

      gzochid_data_mark 
	(context, &gzochid_client_session_serialization, session, &err);
      if (err == NULL)
	{
	  gzochid_scm_location_info *scm_session_reloc = 
	    gzochid_scm_location_get (context, scm_session);
	  gzochid_data_managed_reference *reloc_reference = 
	    gzochid_data_create_reference 
	    (context, &gzochid_scm_location_aware_serialization, 
	     scm_session_reloc);

	  mpz_set (session->scm_oid, reloc_reference->oid);
	  gzochid_client_session_send_login_success (context, session);
	}
      else g_error_free (err);
    }
}

void gzochid_scheme_application_received_message_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  GError *err = NULL;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM bv = SCM_BOOL_F;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *gpa = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  unsigned char *message = NULL;
  short *message_len_ptr = NULL;
  void **data = (void **) ptr;

  mpz_t session_oid;
  mpz_init (session_oid);
  mpz_set_str (session_oid, (char *) data[0], 16);

  message = (unsigned char *) data[1];
  message_len_ptr = (short *) data[2];

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);

  mpz_clear (session_oid);

  gzochid_data_dereference (session_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }

  session = (gzochid_client_session *) session_reference->obj;

  bv = gzochid_scheme_create_bytevector (message, *message_len_ptr);
  callback_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization, 
     session->handler->received_message->scm_oid);

  gzochid_data_dereference (callback_reference, &err);

  if (err != NULL)
    {
      g_error_free (err);
      return;
    }
      
  gzochid_scheme_invoke
    (context, 
     identity,
     "gzochi:execute-received-message", gpa,
     scm_list_2 ((SCM) callback_reference->obj, bv),
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! triggered_by_rollback (scm_variable_ref (exception_var)))
	{
	  gzochid_transaction_join (&scheme_participant, NULL);
	  gzochid_transaction_mark_for_rollback 
	    (&scheme_participant, 
	     is_transaction_retry (scm_variable_ref (exception_var)));
	}
    }

  g_list_free (gpa);
}

void gzochid_scheme_application_disconnected_worker 
(gzochid_application_context *context, gzochid_auth_identity *identity, 
 gpointer ptr)
{
  GError *err = NULL;
  gzochid_client_session *session = NULL;
  gzochid_data_managed_reference *session_reference = NULL;
  gzochid_data_managed_reference *callback_reference = NULL;

  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *gpa = NULL;
  char *oid_str = (char *) ptr;
  mpz_t session_oid;

  mpz_init (session_oid);
  mpz_set_str (session_oid, oid_str, 16);

  session_reference = gzochid_data_create_reference_to_oid 
    (context, &gzochid_client_session_serialization, session_oid);
  mpz_clear (session_oid);

  gzochid_data_dereference (session_reference, &err);

  if (err != NULL)
    {
      if (err->message != NULL)
	gzochid_warning
	  ("Failed to dereference disconnecting session '%s': %s", 
	   oid_str, err->message);
      else gzochid_warning 
	     ("Failed to dereference disconnecting session '%s'.", oid_str);

      g_error_free (err);
      return;
    }

  session = (gzochid_client_session *) session_reference->obj;

  /* A client may disconnect before the login process has completed or after
     it has failed enough times to stop being retried. */

  if (session->handler == NULL)
    {
      gzochid_tx_warning 
	(context, "Session '%s' disconnected after incomplete login.", ptr);

      g_list_free (gpa);

      gzochid_client_session_disconnected_worker (context, identity, oid_str);
      return;
    }

  callback_reference =
    gzochid_data_create_reference_to_oid
    (context, &gzochid_scheme_data_serialization, 
     session->handler->disconnected->scm_oid);

  gzochid_data_dereference (callback_reference, &err);
  
  if (err != NULL)
    {
      if (err->message != NULL)
	gzochid_warning
	  ("Failed to retrieve disconnect callback for session '%s': %s", 
	   oid_str, err->message);
      else gzochid_warning 
	     ("Failed to retrieve disconnect callback for session '%s'.", 
	      oid_str);

      g_error_free (err);      
      gzochid_client_session_disconnected_worker (context, identity, oid_str);
      return;
    }

  gpa = g_list_append
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "app");

  gzochid_scheme_invoke
    (context, 
     identity,
     "gzochi:execute-disconnected", gpa,
     scm_list_1 ((SCM) callback_reference->obj),
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! triggered_by_rollback (scm_variable_ref (exception_var)))
	{
	  gzochid_transaction_join (&scheme_participant, NULL);
	  gzochid_transaction_mark_for_rollback 
	    (&scheme_participant, 
	     is_transaction_retry (scm_variable_ref (exception_var)));
	}
    }

  gzochid_client_session_disconnected_worker (context, identity, oid_str);
  g_list_free (gpa);
}

static void scheme_worker_serializer 
(gzochid_application_context *context, gzochid_application_worker worker, 
 GString *out)
{
}

static gzochid_application_worker scheme_worker_deserializer
(gzochid_application_context *context, GString *in)
{
  return gzochid_scheme_application_task_worker;
}

gboolean is_managed_record (SCM obj)
{
  return scm_is_true (scm_call_1 (scm_managed_record_p, obj));
}

gboolean for_all (SCM lst, gboolean (*pred) (SCM))
{
  SCM ptr = lst;
  while (ptr != SCM_EOL)
    {
      if (!pred (SCM_CAR (ptr)))
	return FALSE;
      ptr = SCM_CDR (ptr);
    }

  return TRUE;
}

static void 
scheme_managed_record_serializer
(gzochid_application_context *context, SCM arg, GString *out, GError **err)
{
  unsigned char vec_len_str[4];
  GList *gpd = g_list_append 
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "data");
  SCM vec = SCM_EOL, port = SCM_EOL, proc = SCM_EOL;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  SCM port_and_proc = 
    scm_struct_ref (scm_open_bytevector_output_port (SCM_BOOL_F), SCM_INUM0);
  int vec_len = 0;

  port = SCM_CAR (port_and_proc);
  proc = SCM_CADR (port_and_proc);

  gzochid_scheme_invoke 
    (context,
     NULL,
     "gzochi:serialize-managed-record", gpd,
     scm_list_2 (port, arg), 
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      /* Don't join the transaction here as the serializer should only be
	 called during the PREPARING phase, which is too late to join. */

      gzochid_transaction_mark_for_rollback 
	(&scheme_participant, 
	 is_transaction_retry (scm_variable_ref (exception_var)));
      g_set_error (err, GZOCHID_IO_ERROR, GZOCHID_IO_ERROR_SERIALIZATION,
		   "Failed to serialize managed record.");
      g_list_free (gpd);
      return;
    }

  vec = scm_call_0 (proc);

  vec_len = SCM_BYTEVECTOR_LENGTH (vec);

  gzochi_common_io_write_int (vec_len, vec_len_str, 0);

  g_string_append_len (out, (char *) vec_len_str, 4);
  g_string_append_len (out, (char *) SCM_BYTEVECTOR_CONTENTS (vec), vec_len);

  g_list_free (gpd);
}

static void *
scheme_managed_record_deserializer
(gzochid_application_context *context, GString *in, GError **err)
{
  int vec_len = gzochi_common_io_read_int ((unsigned char *) in->str, 0);
  SCM vec = SCM_EOL, port = SCM_EOL, record = SCM_EOL;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *args = g_list_append 
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "data");
     
  g_string_erase (in, 0, 4);
  vec = scm_take_u8vector ((unsigned char *) in->str, vec_len);

  port = scm_open_bytevector_input_port (vec, SCM_BOOL_F);

  record = gzochid_scheme_invoke 
    (context, 
     NULL,
     "gzochi:deserialize-managed-record", 
     args,
     scm_list_1 (port),
     exception_var);

  g_list_free (args);
  g_string_erase (in, 0, vec_len);
  
  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (! triggered_by_rollback (scm_variable_ref (exception_var))) 
	{
	  gzochid_transaction_join (&scheme_participant, NULL);
	  gzochid_transaction_mark_for_rollback 
	    (&scheme_participant, 
	     is_transaction_retry (scm_variable_ref (exception_var)));
	}

      g_set_error (err, GZOCHID_IO_ERROR, GZOCHID_IO_ERROR_SERIALIZATION,
		   "Failed to deserialize managed record.");
    }
  else 
    {
      /* If the bytevector port hasn't been fully consumed, it could mean the 
	 serializer and deserializer aren't idempotent. */

      if (scm_eof_object_p (scm_lookahead_u8 (port)) == SCM_BOOL_F)
	gzochid_warning ("Deserialization failed to consume all bytes.");

      scm_gc_protect_object (record);
    }

  return record;
}

static void *scheme_serializer_inner (void *data)
{
  void **ptr = (void **) data;
  gzochid_application_context *context = (gzochid_application_context *) ptr[0];
  SCM task = (SCM) ptr[1];
  GString *out = (GString *) ptr[2];
  GError **err = (GError **) ptr[3];

  scheme_managed_record_serializer (context, task, out, err);

  return NULL;
}

static void 
scheme_serializer
(gzochid_application_context *context, void *ptr, GString *out, GError **err)
{
  void *args[4];

  args[0] = context;
  args[1] = ptr;
  args[2] = out;
  args[3] = err;

  scm_with_guile (scheme_serializer_inner, args);
}

static void *scheme_deserializer_inner (void *data)
{
  void **ptr = (void **) data;
  gzochid_application_context *context = (gzochid_application_context *) ptr[0];
  GString *in = (GString *) ptr[1];
  GError **err = (GError **) ptr[2];

  return scheme_managed_record_deserializer (context, in, err);
}

static void *scheme_deserializer
(gzochid_application_context *context, GString *in, GError **err)
{
  void *args[3];

  args[0] = context;
  args[1] = in;
  args[2] = err;

  return scm_with_guile (scheme_deserializer_inner, args);
}

static void *scheme_finalizer_inner (void *data)
{
  scm_gc_unprotect_object ((SCM) data);
  return NULL;
}

static void scheme_finalizer 
(gzochid_application_context *context, gpointer data)
{
  scm_with_guile (scheme_finalizer_inner, data);
}

static gzochid_application_worker_serialization scheme_worker_serialization =
  { scheme_worker_serializer, scheme_worker_deserializer };

gzochid_io_serialization gzochid_scheme_data_serialization =
  { scheme_serializer, scheme_deserializer, scheme_finalizer };

gzochid_application_task_serialization gzochid_scheme_task_serialization = 
  { 
    "scheme", 
    &scheme_worker_serialization, 
    &gzochid_scheme_data_serialization 
  };

gzochid_application_task *gzochid_scheme_task_new 
(gzochid_application_context *context, gzochid_auth_identity *identity,
 char *procedure, GList *module_name, SCM data)
{
  SCM scm_data = gzochid_scheme_invoke 
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

static gpointer scm_symbol_to_locale_string (SCM sym)
{
  return scm_to_locale_string (scm_symbol_to_string (sym));
}

SCM gzochid_scheme_glist_to_list (GList *lst, SCM (*transformer) (gpointer))
{
  SCM ret = SCM_EOL;
  GList *lst_ptr = lst;

  while (lst_ptr != NULL)
    {
      ret = scm_cons (transformer (lst_ptr->data), ret);
      lst_ptr = lst_ptr->next;
    }

  return scm_reverse (ret);
}

GList *gzochid_scheme_list_to_glist (SCM lst, gpointer (*transformer) (SCM))
{
  GList *ret = NULL;
  SCM lst_ptr = lst;

  while (lst_ptr != SCM_EOL)
    {
      ret = g_list_append (ret, transformer (SCM_CAR (lst_ptr)));
      lst_ptr = SCM_CDR (lst_ptr);
    }

  return ret;
}

SCM gzochid_scheme_ghashtable_to_hashtable 
(GHashTable *ht, SCM hash_func, SCM key_equal_func,
 SCM (*key_transformer) (gpointer), SCM (*value_transformer) (gpointer))
{
  SCM ret = scm_call_2 (scm_make_hashtable, hash_func, key_equal_func);
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init (&iter, ht);
  while (g_hash_table_iter_next (&iter, &key, &value)) 
    scm_call_3 (scm_hashtable_set_x, ret, key_transformer (key), 
		value_transformer (value));

  return ret;
}

GHashTable *gzochid_scheme_hashtable_to_ghashtable
(SCM ht, GHashFunc hash_func, GEqualFunc key_equal_func, 
 gpointer (*key_transformer) (SCM), gpointer (*value_transformer) (SCM))
{
  GHashTable *ret = g_hash_table_new (hash_func, key_equal_func);
  SCM key_list = scm_vector_to_list (scm_call_1 (scm_hashtable_keys, ht));
  
  while (key_list != SCM_EOL)
    {
      SCM key = SCM_CAR (key_list);
      SCM value = scm_call_2 (scm_hashtable_ref, ht, key);

      g_hash_table_insert 
	(ret, key_transformer (key), value_transformer (value));

      key_list = SCM_CDR (key_list);
    }

  return ret;
}

char *gzochid_scheme_callback_procedure (SCM callback)
{
  return scm_symbol_to_locale_string 
    (scm_call_1 (scm_callback_procedure, callback));
}

GList *gzochid_scheme_callback_module (SCM callback)
{
  return gzochid_scheme_list_to_glist
    (scm_call_1 (scm_callback_module, callback), scm_symbol_to_locale_string);
}

SCM gzochid_scheme_callback_data (SCM callback)
{
  return scm_call_1 (scm_callback_data, callback);
}

SCM gzochid_scheme_create_callback (gzochid_application_callback *callback, ...)
{
  SCM cb = SCM_BOOL_F;
  SCM lst_args = SCM_EOL, arg = SCM_BOOL_F;
  va_list args;
  
  va_start (args, callback);
  while ((arg = va_arg (args, SCM)) != NULL)
    lst_args = scm_cons (arg, lst_args);  
  va_end (args);

  cb = scm_apply_2 
    (scm_make_callback, 
     scm_from_locale_symbol (callback->procedure), 
     gzochid_scheme_glist_to_list 
     (callback->module, (SCM (*) (gpointer)) scm_from_locale_symbol), lst_args);
  
  scm_gc_protect_object (cb);

  return cb;
}

SCM gzochid_scheme_create_bytevector (unsigned char *bytes, size_t len)
{
  SCM bv = scm_take_u8vector (bytes, len);
  
  scm_gc_protect_object (bv);

  return bv;
}

SCM gzochid_scheme_create_managed_hashtable (GHashTable *properties)
{
  SCM ht = scm_call_0 (scm_make_managed_hashtable);
  scm_gc_protect_object (ht);
  return ht;
}

SCM gzochid_scheme_r6rs_raise (SCM cond)
{
  return scm_call_1 (scm_r6rs_raise, cond);
}

SCM gzochid_scheme_r6rs_raise_continuable (SCM cond)
{
  return scm_call_1 (scm_r6rs_raise_continuable, cond);
}

SCM gzochid_scheme_make_object_removed_condition ()
{
  return scm_call_0 (scm_make_object_removed_condition);
}

SCM gzochid_scheme_make_name_exists_condition (char *name)
{
  return scm_call_1 
    (scm_make_name_exists_condition, scm_from_locale_string (name));
}

SCM gzochid_scheme_make_name_not_bound_condition (char *name)
{
  return scm_call_1
    (scm_make_name_not_bound_condition, scm_from_locale_string (name));
}

SCM gzochid_scheme_handler_received_message (SCM handler)
{
  return scm_call_1 (scm_handler_received_message, handler);
}

SCM gzochid_scheme_handler_disconnected (SCM handler)
{
  return scm_call_1 (scm_handler_disconnected, handler);
}

SCM gzochid_scheme_create_client_session 
(gzochid_client_session *session, mpz_t oid)
{
  char *oid_str = mpz_get_str (NULL, 16, oid);

  SCM scm_oid = scm_c_locale_stringn_to_number (oid_str, strlen (oid_str), 16);
  SCM ret = scm_call_2 (scm_make_client_session, 
			scm_from_locale_string (session->identity->name),
			scm_oid);

  scm_gc_protect_object (ret);
  free (oid_str);

  return ret;
}

void gzochid_scheme_client_session_oid (SCM session, mpz_t oid)
{
  SCM scm_oid = scm_call_1 (scm_client_session_oid, session);
  char *oid_str = scm_to_locale_string 
    (scm_number_to_string (scm_oid, scm_from_short (16)));

  mpz_set_str (oid, oid_str, 16);
  free (oid_str);
}

SCM gzochid_scheme_create_managed_reference 
(gzochid_data_managed_reference *reference)
{
  char *oid_str = mpz_get_str (NULL, 16, reference->oid);

  SCM scm_oid = scm_string_to_number 
    (scm_from_locale_string (oid_str), scm_from_short (16));
  SCM data = SCM_BOOL_F;
  SCM ret = SCM_BOOL_F;

  if (reference->obj != NULL)
    data = gzochid_scm_location_resolve 
      (reference->context, (gzochid_scm_location_info *) reference->obj);
  
  ret = scm_call_2 (scm_make_managed_reference, scm_oid, data);

  free (oid_str);
  scm_gc_protect_object (ret);

  return ret;
}

SCM gzochid_scheme_create_channel (gzochid_channel *channel, mpz_t oid)
{
  char *oid_str = mpz_get_str (NULL, 16, oid);
  
  SCM scm_oid = scm_string_to_number
    (scm_from_locale_string (oid_str), scm_from_short (16));
  SCM ret = scm_call_2 
    (scm_make_channel, scm_oid, scm_from_locale_string (channel->name));

  free (oid_str);
  scm_gc_protect_object (ret);

  return ret;
}

SCM gzochid_scheme_create_periodic_task_handle (mpz_t oid)
{
  char *oid_str = mpz_get_str (NULL, 16, oid);
  
  SCM scm_oid = scm_string_to_number
    (scm_from_locale_string (oid_str), scm_from_short (16));
  SCM ret = scm_call_1 (scm_make_task_handle, scm_oid);

  free (oid_str);
  scm_gc_protect_object (ret);

  return ret;
}

static void bind_scm (char *module, SCM *binding, char *name)
{
  SCM var = scm_c_public_variable (module, name);

  if (scm_is_false (var))
    g_error ("Missing Scheme binding for `%s'. Aborting...", name);

  *binding = scm_variable_ref (var);
  scm_gc_protect_object (*binding);
}

static void *initialize_bindings (void *ptr)
{
  bind_scm ("rnrs exceptions", &scm_r6rs_raise, "raise");
  bind_scm ("rnrs exceptions", &scm_r6rs_raise_continuable, 
	    "raise-continuable");

  bind_scm ("rnrs hashtables", &gzochid_scheme_string_hash, "string-hash");
  bind_scm ("rnrs base", &gzochid_scheme_string_equiv, "equal?");

  bind_scm ("rnrs hashtables", &scm_hashtable_keys, "hashtable-keys");
  bind_scm ("rnrs hashtables", &scm_hashtable_ref, "hashtable-ref");
  bind_scm ("rnrs hashtables", &scm_hashtable_set_x, "hashtable-set!");
  bind_scm ("rnrs hashtables", &scm_make_hashtable, "make-hashtable");

  bind_scm ("gzochi app", &scm_make_callback, "gzochi:make-callback");
  bind_scm ("gzochi app", &scm_callback_module, "gzochi:callback-module");
  bind_scm ("gzochi app", &scm_callback_procedure, "gzochi:callback-procedure");
  bind_scm ("gzochi app", &scm_callback_data, "gzochi:callback-data");

  bind_scm ("gzochi private channel", &scm_make_channel, 
	    "gzochi:make-channel");
  bind_scm ("gzochi private channel", &scm_channel_oid, "gzochi:channel-oid");

  bind_scm ("gzochi session", &scm_handler_received_message,
	    "gzochi:client-session-listener-received-message");
  bind_scm ("gzochi session", &scm_handler_disconnected,
	    "gzochi:client-session-listener-disconnected");

  bind_scm ("gzochi conditions", &scm_make_name_exists_condition,
	    "gzochi:make-name-exists-condition");
  bind_scm ("gzochi conditions", &scm_make_name_not_bound_condition,
	    "gzochi:make-name-not-bound-condition");
  bind_scm ("gzochi conditions", &scm_make_object_removed_condition,
	    "gzochi:make-object-removed-condition");
  bind_scm ("gzochi conditions", &scm_transaction_retry_condition_p,
	    "gzochi:transaction-retry-condition?");
  bind_scm ("gzochi conditions", &scm_transaction_aborted_condition_p,
	    "gzochi:transaction-aborted-condition?");

  bind_scm ("gzochi data", &scm_make_managed_hashtable, 
	    "gzochi:make-managed-hashtable");
  bind_scm ("gzochi data", &scm_managed_record_p, "gzochi:managed-record?");

  bind_scm ("gzochi private data", &scm_managed_record_serialize, 
	    "gzochi:serialize-managed-record");
  bind_scm ("gzochi private data", &scm_managed_record_deserialize,
	    "gzochi:deserialize-managed-record");
  bind_scm ("gzochi private data", &scm_managed_reference_oid, 
	    "gzochi:managed-reference-oid");

  bind_scm ("gzochi private session", &scm_make_client_session, 
	    "gzochi:make-client-session");
  bind_scm ("gzochi private session", &scm_client_session_oid, 
	    "gzochi:client-session-oid");

  bind_scm ("gzochi private data", &scm_make_managed_reference, 
	    "gzochi:make-managed-reference");
 
  bind_scm ("gzochi private task", &scm_make_task_handle, 
	    "gzochi:make-task-handle");
  bind_scm ("gzochi private task", &scm_task_handle_oid, 
	    "gzochi:task-handle-oid");
 
  gzochid_api_channel_init ();
  gzochid_api_data_init ();
  gzochid_api_log_init ();
  gzochid_api_session_init ();
  gzochid_api_task_init ();
  gzochid_api_util_init ();
  
  return NULL;
}

void gzochid_scheme_managed_reference_oid (SCM reference, mpz_t oid)
{
  SCM num = scm_call_1 (scm_managed_reference_oid, reference);
  char *oid_str = scm_to_locale_string 
    (scm_number_to_string (num, scm_from_short (16)));
  
  mpz_set_str (oid, oid_str, 16);

  free (oid_str);
}

void gzochid_scheme_channel_oid (SCM channel, mpz_t oid)
{
  SCM num = scm_call_1 (scm_channel_oid, channel);
  char *oid_str = scm_to_locale_string
    (scm_number_to_string (num, scm_from_short (16)));
  
  mpz_set_str (oid, oid_str, 16);

  free (oid_str);
}

void gzochid_scheme_task_handle_oid (SCM handle, mpz_t oid)
{
  SCM num = scm_call_1 (scm_task_handle_oid, handle);
  char *oid_str = scm_to_locale_string
    (scm_number_to_string (num, scm_from_short (16)));
  
  mpz_set_str (oid, oid_str, 16);

  free (oid_str);
}

void gzochid_scheme_initialize_bindings (void)
{
  scm_with_guile (initialize_bindings, NULL);
}
