/* test-session.c: Test routines for session.c in libgzochi-client-common.
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

#include "../session.h"

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

static void test_gzochi_client_common_session_is_dispatchable ()
{
  gzochi_client_common_session *session = gzochi_client_common_session_new ();
  
  /* Not connected and disconnect not acknowledged -> Dispatchable. */

  assert (gzochi_client_common_session_is_dispatchable (session));  

  /* Not connected and disconnect acknowledged -> Not dispatchable. */

  session->disconnect_acknowledged = TRUE;
  assert (! gzochi_client_common_session_is_dispatchable (session));

  /* Connected and disconnect not acknowledged -> Not dispatchable. */
  
  session->connected = TRUE;
  session->disconnect_acknowledged = FALSE;
  assert (! gzochi_client_common_session_is_dispatchable (session));

  /* Buffer length less than length prefix plus opcode -> Not dispatchable. */

  session->buffer_length = 2;
  assert (! gzochi_client_common_session_is_dispatchable (session));

  /* Buffer length less than message length plus opcode -> Not dispatchable. */

  session->buffer_length = 3;
  session->buffer[1] = 1;
  assert (! gzochi_client_common_session_is_dispatchable (session));

  /* Buffer length equal to message length plus opcode -> Dispatchable. */
  
  session->buffer_length = 4;
  assert (gzochi_client_common_session_is_dispatchable (session));

  gzochi_client_common_session_free (session);
}

int main (int argc, char *argv[])
{
  test_gzochi_client_common_session_is_dispatchable ();

  return 0;
}
