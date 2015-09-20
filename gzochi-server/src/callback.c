/* callback.c: Constructors and serialization routines for application callbacks
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
#include <gmp.h>
#include <stdlib.h>

#include "app.h"
#include "callback.h"
#include "io.h"
#include "util.h"

static void 
serialize_callback (gzochid_application_context *context, gpointer data, 
		    GString *out, GError **err)
{
  gzochid_application_callback *callback = data;

  gzochid_util_serialize_string (callback->procedure, out);
  gzochid_util_serialize_list 
    (callback->module, 
     (void (*) (gpointer, GString *)) gzochid_util_serialize_string, out);
  gzochid_util_serialize_mpz (callback->scm_oid, out);
}

static gpointer 
deserialize_callback (gzochid_application_context *context, GString *in, 
		      GError **err)
{
  gzochid_application_callback *callback = 
    malloc (sizeof (gzochid_application_callback));

  callback->procedure = gzochid_util_deserialize_string (in);
  callback->module = gzochid_util_deserialize_list 
    (in, (gpointer (*) (GString *)) gzochid_util_deserialize_string);

  mpz_init (callback->scm_oid);
  gzochid_util_deserialize_mpz (in, callback->scm_oid);
  
  return callback;
}

static void
finalize_callback (gzochid_application_context *context, gpointer data)
{
  gzochid_application_callback *callback = data;

  free (callback->procedure);
  g_list_free_full (callback->module, free);
  mpz_clear (callback->scm_oid);

  free (callback);
}

gzochid_io_serialization 
gzochid_application_callback_serialization = 
  { serialize_callback, deserialize_callback, finalize_callback };

gzochid_application_callback *
gzochid_application_callback_new (char *procedure, GList *module, mpz_t scm_oid)
{
  gzochid_application_callback *callback = calloc
    (1, sizeof (gzochid_application_callback));

  callback->module = module;
  callback->procedure = procedure;

  mpz_init (callback->scm_oid);
  mpz_set (callback->scm_oid, scm_oid);

  return callback;
}

void 
gzochid_application_callback_free (gzochid_application_callback *callback)
{
  mpz_clear (callback->scm_oid);
  free (callback);
}

