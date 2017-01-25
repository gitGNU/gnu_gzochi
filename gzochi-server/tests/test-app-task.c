/* test-app-task.c: Test routines for app-task.c in gzochid.
 * Copyright (C) 2017 Julian Graham
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
#include <stdlib.h>

#include "app.h"
#include "app-task.h"
#include "event.h"
#include "game.h"
#include "gzochid-auth.h"
#include "schedule.h"
#include "task.h"
#include "tx.h"

/* Fake implementations to avoid having to pull in `app.o'. */

gzochid_application_context *
gzochid_application_context_new ()
{
  gzochid_application_context *app_context = calloc
    (1, sizeof (gzochid_application_context));
  
  app_context->event_source = gzochid_event_source_new ();

  return app_context;
}

void
gzochid_application_context_free (gzochid_application_context *app_context)
{
  g_source_unref ((GSource *) app_context->event_source);
  free (app_context);
}

static void
null_worker (gzochid_application_context *context,
	     gzochid_auth_identity *identity, gpointer data)
{
}
  
static void
test_application_task_ref ()
{
  gzochid_application_context *context = gzochid_application_context_new ();
  gzochid_auth_identity *identity = gzochid_auth_identity_new ("test");
  gzochid_application_task *task =
    gzochid_application_task_new (context, identity, null_worker, NULL);

  g_assert (task == gzochid_application_task_ref (task));

  gzochid_application_task_unref (task);
  gzochid_application_task_unref (task);
  gzochid_auth_identity_unref (identity);

  gzochid_application_context_free (context);
}

void
gzochid_schedule_submit_task (gzochid_task_queue *task_queue,
			      gzochid_task *task)
{
  task->worker (task->data, NULL);
}

void
gzochid_schedule_execute_task (gzochid_task *task)
{
  task->worker (task->data, NULL);
}

struct _app_task_fixture
{
  int task_attempts;
  int catch_invocations;
  int cleanup_invocations;

  gzochid_application_task *success_task;
  gzochid_application_task *failure_task;
  gzochid_application_task *catch_task;
  gzochid_application_task *cleanup_task;

  gzochid_application_context *context;
  gzochid_auth_identity *identity;
};

typedef struct _app_task_fixture app_task_fixture;

static gboolean
prepare (gpointer data)
{
  return TRUE;
}

static void
commit (gpointer data)
{
}

static void
rollback (gpointer data)
{
}

static gzochid_transaction_participant test_participant =
  { "test-participant", prepare, commit, rollback };

static void
execute_success (gzochid_application_context *context,
		 gzochid_auth_identity *identity, gpointer data)
{
  app_task_fixture *fixture = data;

  fixture->task_attempts++;
}

static void
execute_failure (gzochid_application_context *context,
		 gzochid_auth_identity *identity, gpointer data)
{
  app_task_fixture *fixture = data;

  fixture->task_attempts++;
  gzochid_transaction_join (&test_participant, NULL);
  gzochid_transaction_mark_for_rollback (&test_participant, TRUE);
}

static void
catch (gzochid_application_context *context, gzochid_auth_identity *identity,
       gpointer data)
{
  app_task_fixture *fixture = data;

  fixture->catch_invocations++;
}

static void
cleanup (gzochid_application_context *context, gzochid_auth_identity *identity,
	 gpointer data)
{
  app_task_fixture *fixture = data;

  fixture->cleanup_invocations++;
}

static void
app_task_fixture_set_up (app_task_fixture *fixture, gconstpointer user_data)
{
  fixture->task_attempts = 0;
  fixture->catch_invocations = 0;
  fixture->cleanup_invocations = 0;

  fixture->context = gzochid_application_context_new ();
  fixture->identity = gzochid_auth_identity_new ("[TEST]");
  
  fixture->success_task = gzochid_application_task_new
    (fixture->context, fixture->identity, execute_success, fixture);
  fixture->failure_task = gzochid_application_task_new
    (fixture->context, fixture->identity, execute_failure, fixture);
  fixture->catch_task = gzochid_application_task_new
    (fixture->context, fixture->identity, catch, fixture);
  fixture->cleanup_task = gzochid_application_task_new
    (fixture->context, fixture->identity, cleanup, fixture);
}

static void
app_task_fixture_tear_down (app_task_fixture *fixture, gconstpointer user_data)
{
  gzochid_application_task_free (fixture->success_task);
  gzochid_application_task_free (fixture->failure_task);
  gzochid_application_task_free (fixture->catch_task);
  gzochid_application_task_free (fixture->cleanup_task);

  gzochid_auth_identity_unref (fixture->identity);
  gzochid_application_context_free (fixture->context);
}

static void
test_task_execution_reexecute_success (app_task_fixture *fixture,
				       gconstpointer user_data)
{
  gzochid_transactional_application_task_execution *execution =
    gzochid_transactional_application_task_execution_new
    (fixture->success_task, fixture->catch_task, fixture->cleanup_task);
  
  gzochid_application_reexecuting_transactional_task_worker
    (fixture->context, fixture->identity, execution);

  g_assert_cmpint (fixture->task_attempts, ==, 1);
  g_assert_cmpint (fixture->catch_invocations, ==, 0);
  g_assert_cmpint (fixture->cleanup_invocations, ==, 1);
}

static void
test_task_execution_reexecute_failure (app_task_fixture *fixture,
				       gconstpointer user_data)
{
  gzochid_transactional_application_task_execution *execution =
    gzochid_transactional_application_task_execution_new
    (fixture->failure_task, fixture->catch_task, fixture->cleanup_task);
  
  gzochid_application_reexecuting_transactional_task_worker
    (fixture->context, fixture->identity, execution);

  g_assert_cmpint (fixture->task_attempts, ==, 3);
  g_assert_cmpint (fixture->catch_invocations, ==, 1);
  g_assert_cmpint (fixture->cleanup_invocations, ==, 1);
}

static void
test_task_execution_resubmit_success (app_task_fixture *fixture,
				      gconstpointer user_data)
{
  gzochid_transactional_application_task_execution *execution =
    gzochid_transactional_application_task_execution_new
    (fixture->success_task, fixture->catch_task, fixture->cleanup_task);
  
  gzochid_application_resubmitting_transactional_task_worker
    (fixture->context, fixture->identity, execution);

  g_assert_cmpint (fixture->task_attempts, ==, 1);
  g_assert_cmpint (fixture->catch_invocations, ==, 0);
  g_assert_cmpint (fixture->cleanup_invocations, ==, 1);  
}

static void
test_task_execution_resubmit_failure (app_task_fixture *fixture,
				      gconstpointer user_data)
{
  gzochid_transactional_application_task_execution *execution =
    gzochid_transactional_application_task_execution_new
    (fixture->failure_task, fixture->catch_task, fixture->cleanup_task);
  
  gzochid_application_resubmitting_transactional_task_worker
    (fixture->context, fixture->identity, execution);

  g_assert_cmpint (fixture->task_attempts, ==, 3);
  g_assert_cmpint (fixture->catch_invocations, ==, 1);
  g_assert_cmpint (fixture->cleanup_invocations, ==, 1);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/application-task/ref", test_application_task_ref);
  
  g_test_add ("/task-execution/reexecute/success", app_task_fixture, NULL,
	      app_task_fixture_set_up, test_task_execution_reexecute_success,
	      app_task_fixture_tear_down);
  g_test_add ("/task-execution/reexecute/failure", app_task_fixture, NULL,
	      app_task_fixture_set_up, test_task_execution_reexecute_failure,
	      app_task_fixture_tear_down);
  g_test_add ("/task-execution/resubmit/success", app_task_fixture, NULL,
	      app_task_fixture_set_up, test_task_execution_resubmit_success,
	      app_task_fixture_tear_down);
  g_test_add ("/task-execution/resubmit/failure", app_task_fixture, NULL,
	      app_task_fixture_set_up, test_task_execution_resubmit_failure,
	      app_task_fixture_tear_down);
  
  return g_test_run ();
}
