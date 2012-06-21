/* guile.c: GNU Guile interface routines for gzochid
 * Copyright (C) 2012 Julian Graham
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
#include <libguile.h>
#include <stdlib.h>

#include "guile.h"
#include "threads.h"

typedef struct _gzochid_guile_thread_work 
{
  gzochid_thread_worker worker;
  gpointer data;
  gpointer user_data;
} gzochid_guile_thread_work;

static SCM guile_catch_body (void *data)
{
  void **ptr = (void **) data;

  SCM procedure = (SCM) ptr[0];
  SCM args = (SCM) ptr[1];
  
  return scm_apply_0 (procedure, args);
}

static SCM guile_catch_handler (void *data, SCM tag, SCM throw_args)
{
  SCM exception_variable = (SCM) data;

  if (scm_variable_p (exception_variable) == SCM_BOOL_T)
    scm_variable_set_x (exception_variable, throw_args);

  return SCM_BOOL_F;
}

static SCM guile_pre_unwind_handler (void *data, SCM tag, SCM throw_args)
{
  scm_display_backtrace 
    (scm_make_stack (SCM_BOOL_T, SCM_EOL),
     scm_current_output_port (), 
     SCM_BOOL_F, 
     SCM_BOOL_F);

  scm_print_exception (scm_current_output_port (), SCM_BOOL_F, tag, throw_args);

  return SCM_BOOL_F;
}

SCM gzochid_guile_invoke (SCM procedure, SCM args, SCM exception)
{
  void *sub_data[2];

  sub_data[0] = procedure;
  sub_data[1] = args;

  return scm_c_catch 
    (SCM_BOOL_T,
     guile_catch_body, sub_data, 
     guile_catch_handler, exception, 
     guile_pre_unwind_handler, NULL);
}

static void *guile_unwrapper (gpointer data)
{
  gzochid_guile_thread_work *work = (gzochid_guile_thread_work *) data;
  work->worker (work->data, work->user_data);
  free (work);

  return NULL;
}

static void guile_dispatch (gpointer data, gpointer user_data)
{
  gzochid_guile_thread_work *work = (gzochid_guile_thread_work *) data;
  work->user_data = user_data;
  scm_with_guile (guile_unwrapper, work);
}

void gzochid_guile_run (gzochid_thread_worker worker, gpointer data)
{
  gzochid_guile_thread_work *work = 
    calloc (1, sizeof (gzochid_guile_thread_work));
  
  work->worker = worker;
  work->data = data;
  
  guile_dispatch (work, NULL);
}

void gzochid_guile_thread_pool_push
(GThreadPool *pool, gzochid_thread_worker worker, gpointer data, GError **error)
{ 
  gzochid_guile_thread_work *work = calloc 
    (1, sizeof (gzochid_guile_thread_work));

  work->worker = worker;
  work->data = data;

  gzochid_thread_pool_push (pool, guile_dispatch, work, error);
}
