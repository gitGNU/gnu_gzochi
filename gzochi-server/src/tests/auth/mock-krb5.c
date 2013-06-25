/* mock-krb5.c: Mock implementations of selected routines from krb5.h.
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

#include <glib.h>
#include <krb5.h>
#include <stdlib.h>
#include <string.h>

krb5_error_code krb5_init_context (krb5_context *context)
{
  return 0;
}

krb5_error_code krb5_sname_to_principal 
(krb5_context context, const char *hostname, const char *sname, 
 krb5_int32 type, krb5_principal *ret_princ)
{
  return 0;
}

krb5_error_code krb5_kt_resolve 
(krb5_context context, const char *name, krb5_keytab *ktid)
{
  return 0;
}

krb5_error_code krb5_rd_req
(krb5_context context, krb5_auth_context *auth_context, 
 const krb5_data *packet, krb5_const_principal server, krb5_keytab keytab, 
 krb5_flags *flags, krb5_ticket **ticket)
{
  int i = 0;
  krb5_ticket *t = malloc (sizeof (krb5_ticket));
  krb5_data *d = malloc (sizeof (krb5_data));

  t->enc_part2 = malloc (sizeof (krb5_enc_tkt_part));
  t->enc_part2->client = malloc (sizeof (krb5_principal_data));

  d->data = strndup (packet->data, packet->length);
  d->length = packet->length;

  for (; i < packet->length - 1; i++) 
    d->data[i] = d->data[i] + 3;

  t->enc_part2->client->data = d;

  *ticket = t;

  return 0;
}

krb5_error_code krb5_auth_con_free 
(krb5_context context, krb5_auth_context auth_context)
{
  free (auth_context);
  return 0;
}

krb5_error_code krb5_unparse_name
(krb5_context context, krb5_const_principal principal, char **name)
{
  *name = strndup (principal->data[0].data, principal->data[0].length);
  return 0;
}

void krb5_free_ticket (krb5_context context, krb5_ticket *val)
{
  free (val->enc_part2->client->data);
  free (val->enc_part2->client);
  free (val->enc_part2);
  free (val);
}

void krb5_free_unparsed_name (krb5_context context, char *val)
{
  free (val);
}

const char *krb5_get_error_message (krb5_context ctx, krb5_error_code code)
{
  return "An error occurred during testing.";
}
