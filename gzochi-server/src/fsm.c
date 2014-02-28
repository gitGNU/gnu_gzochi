/* fsm.c: Routines for implementing a rudimentary finite-state machine
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
#include <stdlib.h>

#include "fsm.h"
#include "log.h"

typedef struct _gzochid_fsm_state 
{
  int state;
  char *description;

  GList *enter_functions;
  GList *exit_functions;
} gzochid_fsm_state;

typedef struct _gzochid_fsm_enter_function_registration 
{
  gzochid_fsm_enter_function function;
  gpointer data;
} gzochid_fsm_enter_function_registration;

typedef struct _gzochid_fsm_exit_function_registration 
{
  gzochid_fsm_enter_function function;
  gpointer data;
} gzochid_fsm_exit_function_registration;

static gzochid_fsm_state *gzochid_fsm_state_new (int state, char *description)
{
  gzochid_fsm_state *s = calloc (1, sizeof (gzochid_fsm_state));

  s->state = state;
  s->description = description;

  return s;
}

static void gzochid_fsm_state_free (gzochid_fsm_state *state)
{
  g_list_free_full (state->enter_functions, free);
  g_list_free_full (state->exit_functions, free);
  free (state);
}

void gzochid_fsm_on_enter
(gzochid_fsm *fsm, int state, gzochid_fsm_enter_function func, gpointer data)
{
  gzochid_fsm_state *s = g_hash_table_lookup (fsm->states, &state);
  gzochid_fsm_enter_function_registration *registration = calloc 
    (1, sizeof (gzochid_fsm_enter_function_registration));

  assert (s != NULL);

  registration->function = func;
  registration->data = data;
  
  s->enter_functions = g_list_append (s->enter_functions, registration);
}

void gzochid_fsm_on_exit
(gzochid_fsm *fsm, int state, gzochid_fsm_exit_function func, gpointer data)
{
  gzochid_fsm_state *s = g_hash_table_lookup (fsm->states, &state);
  gzochid_fsm_exit_function_registration *registration = calloc 
    (1, sizeof (gzochid_fsm_exit_function_registration));

  assert (s != NULL);

  registration->function = func;
  registration->data = data;
  
  s->exit_functions = g_list_append (s->exit_functions, registration);
}

gzochid_fsm *gzochid_fsm_new (char *name, int state, char *description)
{
  gzochid_fsm *fsm = calloc (1, sizeof (gzochid_fsm));
  gzochid_fsm_state *s = gzochid_fsm_state_new (state, description);

  fsm->name = name;
  fsm->states = g_hash_table_new_full 
    (g_int_hash, g_int_equal, NULL, (GDestroyNotify) gzochid_fsm_state_free);
  fsm->transitions = g_hash_table_new_full 
    (g_int_hash, g_int_equal, NULL, (GDestroyNotify) g_list_free);
  fsm->current_state = state;

  g_mutex_init (&fsm->mutex);
  g_cond_init (&fsm->cond);

  g_hash_table_insert (fsm->states, &s->state, s);

  return fsm;
}

void gzochid_fsm_add_state (gzochid_fsm *fsm, int state, char *description)
{
  gzochid_fsm_state *s = NULL;

  assert (!fsm->started);

  s = gzochid_fsm_state_new (state, description);
  g_hash_table_insert (fsm->states, &s->state, s);
}

void gzochid_fsm_add_transition 
(gzochid_fsm *fsm, int from_state, int to_state)
{
  gzochid_fsm_state *from_s = NULL;
  gzochid_fsm_state *to_s = NULL;
  GList *transitions = NULL;

  assert (!fsm->started);
  
  from_s = g_hash_table_lookup (fsm->states, &from_state);
  assert (from_s != NULL);
  to_s = g_hash_table_lookup (fsm->states, &to_state);
  assert (to_s != NULL);
  transitions = g_hash_table_lookup (fsm->transitions, &from_state);
  
  if (transitions == NULL)
    g_hash_table_insert 
      (fsm->transitions, &from_s->state, g_list_append (transitions, to_s));
  else transitions = g_list_append (transitions, to_s);
}

static void call_enter_function (gpointer d, gpointer ud)
{
  gzochid_fsm_enter_function_registration *registration = 
    (gzochid_fsm_enter_function_registration *) d;
  int *transition = (int *) ud;

  registration->function (transition[0], transition[1], registration->data);
}

static void call_exit_function (gpointer d, gpointer ud)
{
  gzochid_fsm_exit_function_registration *registration = 
    (gzochid_fsm_exit_function_registration *) d;
  int *transition = (int *) ud;

  registration->function (transition[0], transition[1], registration->data);
}

void gzochid_fsm_start (gzochid_fsm *fsm)
{
  gzochid_fsm_state *state = g_hash_table_lookup 
    (fsm->states, &fsm->current_state);
  int transition[2];

  transition[0] = fsm->current_state;
  transition[1] = fsm->current_state;

  assert (state != NULL);

  g_mutex_lock (&fsm->mutex);

  assert (!fsm->started);
  fsm->started = TRUE;
  gzochid_debug ("[%s] entering state %s", fsm->name, state->description); 
  g_list_foreach (state->enter_functions, call_enter_function, transition);

  g_cond_broadcast (&fsm->cond);
  g_mutex_unlock (&fsm->mutex);
}

void gzochid_fsm_to_state (gzochid_fsm *fsm, int state)
{
  gzochid_fsm_state *old_state = NULL;
  gzochid_fsm_state *new_state = NULL;
  GList *transitions = NULL;
  int transition[2];

  transition[0] = fsm->current_state;
  transition[1] = state;

  g_mutex_lock (&fsm->mutex);

  assert (fsm->started);

  if (fsm->current_state == state)
    {
      g_mutex_unlock (&fsm->mutex);
      return;
    }

  old_state = g_hash_table_lookup (fsm->states, &fsm->current_state);
  new_state = g_hash_table_lookup (fsm->states, &state);

  assert (new_state != NULL);
  transitions = g_hash_table_lookup (fsm->transitions, &fsm->current_state);
  assert (g_list_find (transitions, new_state) != NULL);  

  gzochid_debug ("[%s] exiting state %s", fsm->name, old_state->description);
  g_list_foreach (old_state->exit_functions, call_exit_function, transition);
  gzochid_debug ("[%s] entering state %s", fsm->name, new_state->description); 
  g_list_foreach (new_state->enter_functions, call_enter_function, transition);

  fsm->current_state = state;

  g_cond_broadcast (&fsm->cond);
  g_mutex_unlock (&fsm->mutex);
}

void gzochid_fsm_until (gzochid_fsm *fsm, int state)
{
  g_mutex_lock (&fsm->mutex);
  assert (fsm->started);

  while (fsm->current_state != state)
    g_cond_wait (&fsm->cond, &fsm->mutex);

  g_mutex_unlock (&fsm->mutex);
}

void gzochid_fsm_free (gzochid_fsm *fsm)
{
  g_cond_clear (&fsm->cond);
  g_mutex_clear (&fsm->mutex);
  g_hash_table_destroy (fsm->states);
  g_hash_table_destroy (fsm->transitions);
  free (fsm);
}
