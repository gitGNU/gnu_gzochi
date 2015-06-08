/* test-fsm.c: Test routines for fsm.c in gzochid.
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
#include <stddef.h>

#include "fsm.h"
#include "log.h"

static int from_state = 0;
static int to_state = 0;

static void
simple_transition (int from, int to, gpointer data)
{
  from_state = from;
  to_state = to;
}

static void
reentrant_transition (int from, int to, gpointer data)
{
  gpointer *args = data;
  gzochid_fsm *fsm = args[0];
  int *new_state = args[1];

  gzochid_fsm_to_state (fsm, *new_state);
}

static void
test_start_simple ()
{
  gzochid_fsm *fsm = gzochid_fsm_new ("test", 1, "start");

  gzochid_fsm_on_enter (fsm, 1, simple_transition, NULL);
  
  gzochid_fsm_start (fsm);
  g_assert_cmpint (to_state, ==, 1);  
  gzochid_fsm_free (fsm);
}

static void
test_start_reentrant ()
{
  gpointer args[2];
  int final_state = 1;
  gzochid_fsm *fsm = gzochid_fsm_new ("test", 0, "start");

  args[0] = fsm;
  args[1] = &final_state;
  
  gzochid_fsm_add_state (fsm, 1, "end");
  
  gzochid_fsm_add_transition (fsm, 0, 1);
  
  gzochid_fsm_on_enter (fsm, 0, reentrant_transition, args);
  gzochid_fsm_on_enter (fsm, 0, simple_transition, NULL);
  gzochid_fsm_on_enter (fsm, 1, simple_transition, NULL);
  
  gzochid_fsm_start (fsm);

  g_assert_cmpint (from_state, ==, 0);
  g_assert_cmpint (to_state, ==, 1);
  
  gzochid_fsm_free (fsm);
}

static void
test_to_state_simple ()
{
  gzochid_fsm *fsm = gzochid_fsm_new ("test", 0, "start");

  gzochid_fsm_add_state (fsm, 1, "end");
  gzochid_fsm_add_transition (fsm, 0, 1);
  gzochid_fsm_on_enter (fsm, 1, simple_transition, NULL);
  
  gzochid_fsm_start (fsm);
  gzochid_fsm_to_state (fsm, 1);

  g_assert_cmpint (from_state, ==, 0);
  g_assert_cmpint (to_state, ==, 1);
  
  gzochid_fsm_free (fsm);
}

static void
test_to_state_reentrant ()
{
  gpointer args[2];
  int final_state = 2;
  gzochid_fsm *fsm = gzochid_fsm_new ("test", 0, "start");

  args[0] = fsm;
  args[1] = &final_state;
  
  gzochid_fsm_add_state (fsm, 1, "middle");
  gzochid_fsm_add_state (fsm, 2, "end");
  
  gzochid_fsm_add_transition (fsm, 0, 1);
  gzochid_fsm_add_transition (fsm, 1, 2);
  
  gzochid_fsm_on_enter (fsm, 1, reentrant_transition, args);
  gzochid_fsm_on_enter (fsm, 1, simple_transition, NULL);
  gzochid_fsm_on_enter (fsm, 2, simple_transition, NULL);
  
  gzochid_fsm_start (fsm);
  gzochid_fsm_to_state (fsm, 1);

  g_assert_cmpint (from_state, ==, 1);
  g_assert_cmpint (to_state, ==, 2);
  
  gzochid_fsm_free (fsm);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);
  g_test_add_func ("/fsm/start/simple", test_start_simple);
  g_test_add_func ("/fsm/start/reentrant", test_start_reentrant);
  g_test_add_func ("/fsm/to_state/simple", test_to_state_simple);
  g_test_add_func ("/fsm/to_state/reentrant", test_to_state_reentrant);

  return g_test_run ();
}
