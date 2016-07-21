/* scheme.c: Supplementary Scheme routines for gzochid
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
#include <gzochi-common.h>
#include <libguile.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "data.h"
#include "guile.h"
#include "gzochid-auth.h"
#include "io.h"
#include "reloc.h"
#include "scheme.h"
#include "tx.h"

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

static SCM scm_transaction_retry_condition_p;
static SCM scm_transaction_aborted_condition_p;

static SCM scm_make_object_removed_condition;
static SCM scm_make_name_exists_condition;
static SCM scm_make_name_not_bound_condition;

GQuark
gzochid_scheme_error_quark (void)
{
  return g_quark_from_static_string ("gzochid-scheme-error-quark");
}

static gpointer 
scheme_invoke_inner (gpointer data)
{
  void **ptr = data;

  SCM procedure = ptr[0];
  SCM args = ptr[1];
  SCM exception_var = ptr[2];

  return gzochid_guile_invoke (procedure, args, exception_var);
}

SCM
gzochid_scheme_invoke (gzochid_application_context *context, 
		       gzochid_auth_identity *identity, SCM proc, SCM args, 
		       SCM exception_var)
{
  void *data[3];
  SCM ret = SCM_EOL;

  data[0] = proc;
  data[1] = args;
  data[2] = exception_var;

  ret = gzochid_with_application_context 
    (context, identity, scheme_invoke_inner, data);

  scm_remember_upto_here_1 (proc);
  scm_remember_upto_here_1 (args);
  scm_remember_upto_here_1 (exception_var);

  return ret;
}

static SCM 
resolve_procedure (char *procedure, GList *module)
{
  SCM scm_procedure;
  SCM scm_module;

  assert (procedure != NULL);
  assert (module != NULL);

  scm_procedure = scm_from_locale_symbol (procedure);
  scm_module = gzochid_scheme_glist_to_list 
    (module, (SCM (*) (gpointer)) scm_from_locale_symbol);
  
  return scm_public_ref (scm_module, scm_procedure);
}

SCM 
gzochid_scheme_invoke_callback (gzochid_application_context *context,
				gzochid_auth_identity *identity, 
				char *procedure, GList *module, SCM args, 
				SCM exception_var)
{
  return gzochid_scheme_invoke 
    (context, identity, resolve_procedure (procedure, module), args, 
     exception_var);
}

static void 
scheme_managed_record_serializer (gzochid_application_context *context, SCM arg,
				  GByteArray *out, GError **err)
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

  gzochid_scheme_invoke_callback
    (context,
     NULL,
     "gzochi:serialize-managed-record", gpd,
     scm_list_2 (port, arg), 
     exception_var);

  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (gzochid_scheme_is_transaction_retry 
	  (scm_variable_ref (exception_var)))
	g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_RETRY,
		     "Failed to serialize managed record.");
      else g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED,
			"Failed toserialize managed record.");

      g_list_free (gpd);
      return;
    }

  vec = scm_call_0 (proc);

  vec_len = SCM_BYTEVECTOR_LENGTH (vec);

  gzochi_common_io_write_int (vec_len, vec_len_str, 0);

  g_byte_array_append (out, vec_len_str, 4);
  g_byte_array_append
    (out, (unsigned char *) SCM_BYTEVECTOR_CONTENTS (vec), vec_len);

  g_list_free (gpd);
}

static void *
scheme_managed_record_deserializer (gzochid_application_context *context, 
				    GByteArray *in, GError **err)
{
  int vec_len = gzochi_common_io_read_int (in->data, 0);
  SCM vec = SCM_EOL, port = SCM_EOL, record = SCM_EOL;
  SCM exception_var = scm_make_variable (SCM_UNSPECIFIED);
  GList *args = g_list_append 
    (g_list_append (g_list_append (NULL, "gzochi"), "private"), "data");

  g_byte_array_remove_range (in, 0, 4);

  if (vec_len > in->len)
    {
      g_set_error
	(err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_SERIAL,
	 "Incorrect length prefix for managed record; possibly corrupt store.");
      return SCM_BOOL_F;
    }
  
  vec = scm_take_u8vector (in->data, vec_len);
  port = scm_open_bytevector_input_port (vec, SCM_BOOL_F);

  record = gzochid_scheme_invoke_callback
    (context, 
     NULL,
     "gzochi:deserialize-managed-record", 
     args,
     scm_list_1 (port),
     exception_var);

  g_list_free (args);
  g_byte_array_remove_range (in, 0, vec_len);
  
  if (scm_variable_ref (exception_var) != SCM_UNSPECIFIED)
    {
      if (gzochid_scheme_is_transaction_retry 
	  (scm_variable_ref (exception_var)))
	g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_RETRY,
		     "Failed to deserialize managed record.");
      else g_set_error (err, GZOCHID_SCHEME_ERROR, GZOCHID_SCHEME_ERROR_FAILED,
			"Failed to deserialize managed record.");
    }
  else 
    {
      /* If the bytevector port hasn't been fully consumed, it could mean the 
	 serializer and deserializer aren't idempotent. */

      if (scm_eof_object_p (scm_lookahead_u8 (port)) == SCM_BOOL_F)
	g_warning ("Deserialization failed to consume all bytes.");

      scm_gc_protect_object (record);
    }

  return record;
}

static void *
scheme_serializer_inner (void *data)
{
  void **ptr = data;
  gzochid_application_context *context = ptr[0];
  SCM task = ptr[1];
  GByteArray *out = ptr[2];
  GError **err = ptr[3];

  scheme_managed_record_serializer (context, task, out, err);

  return NULL;
}

static void 
scheme_serializer (gzochid_application_context *context, void *ptr, 
		   GByteArray *out, GError **err)
{
  void *args[4];

  args[0] = context;
  args[1] = ptr;
  args[2] = out;
  args[3] = err;

  scm_with_guile (scheme_serializer_inner, args);
}

static void *
scheme_deserializer_inner (void *data)
{
  void **ptr = data;
  gzochid_application_context *context = ptr[0];
  GByteArray *in = ptr[1];
  GError **err = ptr[2];

  return scheme_managed_record_deserializer (context, in, err);
}

static void *
scheme_deserializer (gzochid_application_context *context, GByteArray *in, 
		     GError **err)
{
  void *args[3];

  args[0] = context;
  args[1] = in;
  args[2] = err;

  return scm_with_guile (scheme_deserializer_inner, args);
}

static void *
scheme_finalizer_inner (void *data)
{
  scm_gc_unprotect_object (data);
  return NULL;
}

static void 
scheme_finalizer (gzochid_application_context *context, gpointer data)
{
  scm_with_guile (scheme_finalizer_inner, data);
}

gzochid_io_serialization gzochid_scheme_data_serialization =
  { scheme_serializer, scheme_deserializer, scheme_finalizer };

static gpointer 
scm_symbol_to_locale_string (SCM sym)
{
  return scm_to_locale_string (scm_symbol_to_string (sym));
}

SCM 
gzochid_scheme_glist_to_list (GList *lst, SCM (*transformer) (gpointer))
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

GList *
gzochid_scheme_list_to_glist (SCM lst, gpointer (*transformer) (SCM))
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

SCM 
gzochid_scheme_ghashtable_to_hashtable 
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

GHashTable *
gzochid_scheme_hashtable_to_ghashtable
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

char *
gzochid_scheme_callback_procedure (SCM callback)
{
  return scm_symbol_to_locale_string 
    (scm_call_1 (scm_callback_procedure, callback));
}

GList *
gzochid_scheme_callback_module (SCM callback)
{
  return gzochid_scheme_list_to_glist
    (scm_call_1 (scm_callback_module, callback), scm_symbol_to_locale_string);
}

SCM 
gzochid_scheme_callback_data (SCM callback)
{
  return scm_call_1 (scm_callback_data, callback);
}

SCM 
gzochid_scheme_create_callback (gzochid_application_callback *callback, ...)
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

SCM 
gzochid_scheme_create_bytevector (unsigned char *bytes, size_t len)
{
  SCM bv = scm_take_u8vector (bytes, len);
  
  scm_gc_protect_object (bv);

  return bv;
}

SCM 
gzochid_scheme_create_managed_hashtable (GHashTable *properties)
{
  SCM ht = scm_call_0 (scm_make_managed_hashtable);
  scm_gc_protect_object (ht);
  return ht;
}

gboolean 
gzochid_scheme_is_transaction_retry (SCM cond)
{
  return scm_is_true (scm_call_1 (scm_transaction_retry_condition_p, cond));
}

gboolean 
gzochid_scheme_is_transaction_aborted (SCM cond)
{
  return scm_is_true (scm_call_1 (scm_transaction_aborted_condition_p, cond));
}

gboolean 
gzochid_scheme_triggered_by_rollback (SCM cond)
{
  return gzochid_scheme_is_transaction_aborted (cond) 
    && gzochid_transaction_rollback_only ();
}

SCM 
gzochid_scheme_make_object_removed_condition ()
{
  return scm_call_0 (scm_make_object_removed_condition);
}

SCM 
gzochid_scheme_make_name_exists_condition (char *name)
{
  return scm_call_1 
    (scm_make_name_exists_condition, scm_from_locale_string (name));
}

SCM 
gzochid_scheme_make_name_not_bound_condition (char *name)
{
  return scm_call_1
    (scm_make_name_not_bound_condition, scm_from_locale_string (name));
}

SCM 
gzochid_scheme_handler_received_message (SCM handler)
{
  return scm_call_1 (scm_handler_received_message, handler);
}

SCM 
gzochid_scheme_handler_disconnected (SCM handler)
{
  return scm_call_1 (scm_handler_disconnected, handler);
}

SCM 
gzochid_scheme_create_client_session (gzochid_client_session *session, 
				      guint64 oid)
{
  SCM name = scm_from_locale_string
    (gzochid_auth_identity_name (gzochid_client_session_identity (session)));
  
  SCM scm_oid = scm_from_unsigned_integer (oid);
  SCM ret = scm_call_2 (scm_make_client_session, name, scm_oid);

  scm_gc_protect_object (ret);

  return ret;
}

guint64
gzochid_scheme_client_session_oid (SCM session)
{
  SCM scm_oid = scm_call_1 (scm_client_session_oid, session);
  return scm_to_uint64 (scm_oid);
}

SCM 
gzochid_scheme_create_managed_reference 
(gzochid_data_managed_reference *reference)
{
  SCM scm_oid = scm_from_unsigned_integer (reference->oid);
  SCM data = SCM_BOOL_F;
  SCM ret = SCM_BOOL_F;

  if (reference->obj != NULL)
    data = gzochid_scm_location_resolve (reference->context, reference->obj);
  
  ret = scm_call_2 (scm_make_managed_reference, scm_oid, data);

  scm_gc_protect_object (ret);

  return ret;
}

SCM 
gzochid_scheme_create_channel (gzochid_channel *channel, guint64 oid)
{
  SCM scm_oid = scm_from_unsigned_integer (oid);
  SCM ret = scm_call_2 
    (scm_make_channel, scm_oid, scm_from_locale_string
     (gzochid_channel_name (channel)));

  scm_gc_protect_object (ret);

  return ret;
}

SCM 
gzochid_scheme_create_periodic_task_handle (guint64 oid)
{
  SCM scm_oid = scm_from_unsigned_integer (oid);
  SCM ret = scm_call_1 (scm_make_task_handle, scm_oid);

  scm_gc_protect_object (ret);

  return ret;
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
  bind_scm ("rnrs hashtables", &scm_hashtable_keys, "hashtable-keys");
  bind_scm ("rnrs hashtables", &scm_hashtable_ref, "hashtable-ref");
  bind_scm ("rnrs hashtables", &scm_hashtable_set_x, "hashtable-set!");
  bind_scm ("rnrs hashtables", &scm_make_hashtable, "make-hashtable");

  bind_scm ("gzochi app", &scm_make_callback, "gzochi:make-callback");
  bind_scm ("gzochi app", &scm_callback_module, "gzochi:callback-module");
  bind_scm ("gzochi app", &scm_callback_procedure, "gzochi:callback-procedure");
  bind_scm ("gzochi app", &scm_callback_data, "gzochi:callback-data");

  bind_scm ("gzochi conditions", &scm_transaction_retry_condition_p,
	    "gzochi:transaction-retry-condition?");
  bind_scm ("gzochi conditions", &scm_transaction_aborted_condition_p,
	    "gzochi:transaction-aborted-condition?");

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
 
  return NULL;
}

guint64
gzochid_scheme_managed_reference_oid (SCM reference)
{
  SCM num = scm_call_1 (scm_managed_reference_oid, reference);
  return scm_to_uint64 (num);
}

guint64
gzochid_scheme_channel_oid (SCM channel)
{
  SCM num = scm_call_1 (scm_channel_oid, channel);
  return scm_to_uint64 (num);
}

guint64
gzochid_scheme_task_handle_oid (SCM handle)
{
  SCM num = scm_call_1 (scm_task_handle_oid, handle);
  return scm_to_uint64 (num);
}

void 
gzochid_scheme_initialize_bindings (void)
{
  scm_with_guile (initialize_bindings, NULL);
}
