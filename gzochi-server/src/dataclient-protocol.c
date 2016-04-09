/* dataclient-protocol.c: Implementation of dataclient protocol.
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

#include <glib.h>
#include <gzochi-common.h>

#include "dataclient.h"
#include "dataclient-protocol.h"

static gboolean
client_can_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return FALSE;
}

static unsigned int
client_dispatch (const GByteArray *buffer, gpointer user_data)
{
  return 0;
}

static void
client_error (gpointer user_data)
{
  gzochid_dataclient_nullify_connection (user_data);
}

static void
client_free (gpointer user_data)
{
}

gzochid_client_protocol gzochid_dataclient_client_protocol =
  { client_can_dispatch, client_dispatch, client_error, client_free };
