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
#include <string.h>

#include "fsm.h"

/* The FSM struct. */

struct _gzochid_fsm
{
  char *name;
  
  GHashTable *states;
  GHashTable *transitions;

  GArray *pending_states; /* The re-entrant "queue" of state changes. */
  
  int current_state;
  int started;

  GRecMutex mutex; /* Allows re-entrant access to FSM state. */
  
  GMutex signal_mutex; /* Protects the state-change condition variable. */
  GCond cond; /* The state-change condition variable. */
};

struct _gzochid_fsm_state 
{
  int state;
  char *description;

  GList *enter_functions;
  GList *exit_functions;
};

typedef struct _gzochid_fsm_state gzochid_fsm_state;

struct _gzochid_fsm_enter_function_registration 
{
  gzochid_fsm_enter_function function;
  gpointer data;
};

typedef struct _gzochid_fsm_enter_function_registration
gzochid_fsm_enter_function_registration;

struct _gzochid_fsm_exit_function_registration 
{
  gzochid_fsm_enter_function function;
  gpointer data;
};

typedef struct _gzochid_fsm_exit_function_registration
gzochid_fsm_exit_function_registration;

static gzochid_fsm_state *
gzochid_fsm_state_new (int state, char *description)
{
  gzochid_fsm_state *s = calloc (1, sizeof (gzochid_fsm_state));

  assert (description != NULL);
  
  s->state = state;
  s->description = strdup (description);

  return s;
}

static void
gzochid_fsm_state_free (gzochid_fsm_state *state)
{
  g_list_free_full (state->enter_functions, free);
  g_list_free_full (state->exit_functions, free);
  free (state->description);
  free (state);
}

void
gzochid_fsm_on_enter (gzochid_fsm *fsm, int state,
		      gzochid_fsm_enter_function func, gpointer data)
{
  gzochid_fsm_state *s = NULL; 
  gzochid_fsm_enter_function_registration *registration = calloc 
    (1, sizeof (gzochid_fsm_enter_function_registration));

  g_rec_mutex_lock (&fsm->mutex);
  s = g_hash_table_lookup (fsm->states, &state);
  
  assert (s != NULL);

  registration->function = func;
  registration->data = data;
  
  s->enter_functions = g_list_append (s->enter_functions, registration);
  g_rec_mutex_unlock (&fsm->mutex);
}

void
gzochid_fsm_on_exit (gzochid_fsm *fsm, int state,
		     gzochid_fsm_exit_function func, gpointer data)
{
  gzochid_fsm_state *s = NULL;
  gzochid_fsm_exit_function_registration *registration = calloc 
    (1, sizeof (gzochid_fsm_exit_function_registration));

  g_rec_mutex_lock (&fsm->mutex);
  s = g_hash_table_lookup (fsm->states, &state);
  
  assert (s != NULL);

  registration->function = func;
  registration->data = data;
  
  s->exit_functions = g_list_append (s->exit_functions, registration);
  g_rec_mutex_unlock (&fsm->mutex);
}

gzochid_fsm *
gzochid_fsm_new (char *name, int state, char *description)
{
  gzochid_fsm *fsm = calloc (1, sizeof (gzochid_fsm));
  gzochid_fsm_state *s = gzochid_fsm_state_new (state, description);

  assert (name != NULL);

  fsm->name = strdup (name);
  fsm->states = g_hash_table_new_full 
    (g_int_hash, g_int_equal, NULL, (GDestroyNotify) gzochid_fsm_state_free);
  fsm->transitions = g_hash_table_new_full 
    (g_int_hash, g_int_equal, NULL, (GDestroyNotify) g_list_free);
  fsm->current_state = state;
  fsm->pending_states = g_array_new (FALSE, FALSE, sizeof (int));

  
  g_rec_mutex_init (&fsm->mutex);
  g_mutex_init (&fsm->signal_mutex);
  g_cond_init (&fsm->cond);

  g_hash_table_insert (fsm->states, &s->state, s);

  return fsm;
}

void
gzochid_fsm_add_state (gzochid_fsm *fsm, int state, char *description)
{
  gzochid_fsm_state *s = NULL;

  g_rec_mutex_lock (&fsm->mutex);  
  assert (!fsm->started);

  s = gzochid_fsm_state_new (state, description);
  g_hash_table_insert (fsm->states, &s->state, s);

  g_rec_mutex_unlock (&fsm->mutex);
}

void
gzochid_fsm_add_transition (gzochid_fsm *fsm, int from_state, int to_state)
{
  gzochid_fsm_state *from_s = NULL;
  gzochid_fsm_state *to_s = NULL;
  GList *transitions = NULL;

  g_rec_mutex_lock (&fsm->mutex);  

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

  g_rec_mutex_unlock (&fsm->mutex);
}

static void
call_enter_function (gpointer d, gpointer ud)
{
  gzochid_fsm_enter_function_registration *registration = d;
  int *transition = ud;

  registration->function (transition[0], transition[1], registration->data);
}

static void
call_exit_function (gpointer d, gpointer ud)
{
  gzochid_fsm_exit_function_registration *registration = d;
  int *transition = ud;

  registration->function (transition[0], transition[1], registration->data);
}

/* The guts of the inner loop of `gzochid_fsm_to_state'. Invokes the exit 
   handlers, entry handlers, and notifies listeners of the state change. 

   This function expects that the recursive mutex on the specified FSM is held
   by the calling thread. */

static void to_state (gzochid_fsm *fsm, int state)
{
  GList *transitions = NULL;
  gzochid_fsm_state *old_state = NULL;
  gzochid_fsm_state *new_state = NULL;
  GList *exit_functions_copy = NULL;
  GList *enter_functions_copy = NULL;
  
  int transition[2];

  assert (fsm->current_state != state);
  
  transition[0] = fsm->current_state;
  transition[1] = state;

  old_state = g_hash_table_lookup (fsm->states, &fsm->current_state);
  new_state = g_hash_table_lookup (fsm->states, &state);
  
  assert (new_state != NULL);
  transitions = g_hash_table_lookup (fsm->transitions, &fsm->current_state);
  assert (g_list_find (transitions, new_state) != NULL);  

  /* The state exit and entry functions could change as a result of a re-entrant
     call, so make a copy of both lists before iterating. */

  g_debug ("[%s] exiting state %s", fsm->name, old_state->description);
  exit_functions_copy = g_list_copy (old_state->exit_functions);
  g_list_foreach (exit_functions_copy, call_exit_function, transition);
  g_list_free (exit_functions_copy);

  g_debug ("[%s] entering state %s", fsm->name, new_state->description);
  enter_functions_copy = g_list_copy (new_state->enter_functions);
  g_list_foreach (enter_functions_copy, call_enter_function, transition);
  g_list_free (enter_functions_copy);
  
  g_mutex_lock (&fsm->signal_mutex);
  fsm->current_state = state;
  g_cond_broadcast (&fsm->cond);
  g_mutex_unlock (&fsm->signal_mutex);
}

void
gzochid_fsm_start (gzochid_fsm *fsm)
{
  GList *enter_functions_copy = NULL;
  gzochid_fsm_state *state = NULL;
  int transition[2];

  g_rec_mutex_lock (&fsm->mutex);

  state = g_hash_table_lookup 
    (fsm->states, &fsm->current_state);
  
  transition[0] = fsm->current_state;
  transition[1] = fsm->current_state;

  assert (state != NULL);
  assert (!fsm->started);
  
  fsm->started = TRUE;
  fsm->pending_states = g_array_append_val (fsm->pending_states, state);
  
  g_debug ("[%s] entering state %s", fsm->name, state->description);

  /* The state entry functions could change as a result of a re-entrant call,
     so make a copy of the list before iterating. */
  
  enter_functions_copy = g_list_copy (state->enter_functions);
  g_list_foreach (enter_functions_copy, call_enter_function, transition);
  g_list_free (enter_functions_copy);

  g_mutex_lock (&fsm->signal_mutex);
  g_cond_broadcast (&fsm->cond);
  g_mutex_unlock (&fsm->signal_mutex);

  fsm->pending_states = g_array_remove_index (fsm->pending_states, 0);

  while (fsm->pending_states->len > 0)
    {
      int new_state = g_array_index (fsm->pending_states, int, 0);
	
      if (new_state != fsm->current_state)
	to_state (fsm, new_state);
      
      fsm->pending_states = g_array_remove_index (fsm->pending_states, 0);
    }
  
  g_rec_mutex_unlock (&fsm->mutex);
}

void
gzochid_fsm_to_state (gzochid_fsm *fsm, int state)
{
  gboolean reentering = FALSE;

  g_rec_mutex_lock (&fsm->mutex);

  reentering = fsm->pending_states->len > 0;
  fsm->pending_states = g_array_append_val (fsm->pending_states, state);
  assert (fsm->started);
  
  if (! reentering)
    while (fsm->pending_states->len > 0)
      {
	int new_state = g_array_index (fsm->pending_states, int, 0);
	
	if (new_state != fsm->current_state)
	  to_state (fsm, new_state);

	fsm->pending_states = g_array_remove_index (fsm->pending_states, 0);
      }

  g_rec_mutex_unlock (&fsm->mutex);
}

void
gzochid_fsm_until (gzochid_fsm *fsm, int state)
{
  assert (fsm->started);
  g_mutex_lock (&fsm->signal_mutex);

  while (fsm->current_state != state)
    g_cond_wait (&fsm->cond, &fsm->signal_mutex);

  g_mutex_unlock (&fsm->signal_mutex);
}

void
gzochid_fsm_free (gzochid_fsm *fsm)
{
  free (fsm->name);
  g_cond_clear (&fsm->cond);
  g_mutex_clear (&fsm->signal_mutex);
  g_rec_mutex_clear (&fsm->mutex);
  g_hash_table_destroy (fsm->states);
  g_hash_table_destroy (fsm->transitions);
  g_array_free (fsm->pending_states, TRUE);
  free (fsm);
}
