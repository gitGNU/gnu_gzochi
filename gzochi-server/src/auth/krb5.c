/* krb5.c: Kerberos 5 authentication plugin for gzochid
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

#include <glib.h>
#include <krb5.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "../gzochid-auth.h"

#define SERVICE_NAME_PROPERTY "service_name"
#define KEYTAB_FILE_PROPERTY "keytab_file"

struct _gzochid_auth_krb5_context
{
  krb5_context context;
  krb5_principal principal;
  krb5_keytab keytab;
};

typedef struct _gzochid_auth_krb5_context gzochid_auth_krb5_context;

static gpointer 
initialize (GHashTable *properties, GError **error)
{
  char *service_name = g_hash_table_lookup (properties, SERVICE_NAME_PROPERTY);
  krb5_error_code error_code;

  gzochid_auth_krb5_context *context = 
    malloc (sizeof (gzochid_auth_krb5_context));  
  context->keytab = NULL;

  krb5_init_context (&context->context);

  if (service_name == NULL)
    service_name = "gzochi";

  error_code = krb5_sname_to_principal
    (context->context, NULL, service_name, KRB5_NT_SRV_HST, 
     &context->principal);
  if (error_code != 0)
    {
      g_set_error (error, 
		   GZOCHID_AUTH_PLUGIN_ERROR, 
		   GZOCHID_AUTH_PLUGIN_ERROR_INIT, 
		   "Failed to initialize: %s", 
		   krb5_get_error_message (context->context, error_code));

      free (context);
      return NULL;
    }
  
  if (g_hash_table_contains (properties, KEYTAB_FILE_PROPERTY))
    {
      GString *path = g_string_append 
	(g_string_new ("FILE:"), 
	 g_hash_table_lookup (properties, KEYTAB_FILE_PROPERTY));

      error_code = krb5_kt_resolve 
	(context->context, path->str, &context->keytab);
      g_string_free (path, FALSE);

      if (error_code != 0)
	{
	  g_set_error (error, 
		       GZOCHID_AUTH_PLUGIN_ERROR, 
		       GZOCHID_AUTH_PLUGIN_ERROR_INIT, 
		       "Failed to initialize: %s", 
		       krb5_get_error_message (context->context, error_code));
	  
	  free (context);
	  return NULL;
	}
    }

  return context;
}

static gzochid_auth_identity *
authenticate (unsigned char *credentials, short len, gpointer auth_data, 
	      GError **error)
{
  gzochid_auth_krb5_context *context = (gzochid_auth_krb5_context *) auth_data;
  gzochid_auth_identity *identity = NULL;
  char *name = NULL;

  krb5_auth_context auth_context = NULL;
  krb5_error_code error_code;
  krb5_data packet;
  krb5_ticket *ticket = NULL;

  packet.length = len;
  packet.data = (krb5_pointer) credentials;

  error_code = krb5_rd_req
    (context->context, &auth_context, &packet, context->principal, 
     context->keytab, NULL, &ticket);
  if (error_code != 0)
    {
      g_set_error (error, 
		   GZOCHID_AUTH_PLUGIN_ERROR, 
		   GZOCHID_AUTH_PLUGIN_ERROR_AUTH, 
		   "Authentication failure: %s", 
		   krb5_get_error_message (context->context, error_code));
      return NULL;
    }

  krb5_auth_con_free (context->context, auth_context);

  error_code = krb5_unparse_name 
    (context->context, ticket->enc_part2->client, &name);
  krb5_free_ticket (context->context, ticket);
  
  if (error_code != 0)
    {
      g_set_error (error, 
		   GZOCHID_AUTH_PLUGIN_ERROR, 
		   GZOCHID_AUTH_PLUGIN_ERROR_AUTH, 
		   "Failed to decode identity: %s", 
		   krb5_get_error_message (context->context, error_code));
      return NULL;
    }
  else
    {
      identity = gzochid_auth_identity_new (name);
      krb5_free_unparsed_name (context->context, name);
      return identity;
    }
}

static gzochid_auth_plugin_info info = { "krb5", initialize, authenticate };

GZOCHID_AUTH_INIT_PLUGIN (info)
