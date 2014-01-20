/* fsm.h: Prototypes and declarations for fsm.c
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

#ifndef GZOCHID_FSM_H
#define GZOCHID_FSM_H

#include <glib.h>

typedef void (*gzochid_fsm_enter_function)(int, int, gpointer);
typedef void (*gzochid_fsm_exit_function)(int, int, gpointer);

typedef struct _gzochid_fsm {
  char *name;
  
  GHashTable *states;
  GHashTable *transitions;

  int current_state;
  int started;

  GMutex mutex;
  GCond cond;
} gzochid_fsm;

void gzochid_fsm_on_enter 
(gzochid_fsm *, int, gzochid_fsm_enter_function, gpointer);
void gzochid_fsm_on_exit 
(gzochid_fsm *, int, gzochid_fsm_exit_function, gpointer);

gzochid_fsm *gzochid_fsm_new (char *, int, char *);
void gzochid_fsm_add_state (gzochid_fsm *fsm, int, char *);
void gzochid_fsm_add_transition (gzochid_fsm *, int, int);
void gzochid_fsm_start (gzochid_fsm *);
void gzochid_fsm_to_state (gzochid_fsm *, int);
void gzochid_fsm_until (gzochid_fsm *, int);
void gzochid_fsm_free (gzochid_fsm *);

#endif /* GZOCHID_FSM_H */
