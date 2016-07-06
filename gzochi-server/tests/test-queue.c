/* test-queue.c: Test routines for queue.c in gzochid.
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
#include <gmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "app.h"
#include "data.h"
#include "io.h"
#include "mock-data.h"
#include "queue.h"
#include "util.h"

/* Some toy serialization routines for strings. */

static void
serialize_string (gzochid_application_context *app_context, gpointer data,
		  GString *out, GError **err)
{
  gzochid_util_serialize_string (data, out);
}

static gpointer
deserialize_string (gzochid_application_context *app_context, GString *in,
		    GError **err)
{
  return gzochid_util_deserialize_string (in);
}

static void
finalize_string (gzochid_application_context *app_context, gpointer data)
{
  free (data);
}

static gzochid_io_serialization string_serialization =
  { serialize_string, deserialize_string, finalize_string };

static void
test_queue_offer ()
{
  char *data = NULL;
  GError *err = NULL;
  gzochid_durable_queue *queue = gzochid_durable_queue_new (NULL);
  
  gzochid_durable_queue_offer
    (queue, &string_serialization, strdup ("foo"), &err);
  g_assert_no_error (err);
  gzochid_durable_queue_offer
    (queue, &string_serialization, strdup ("bar"), &err);
  g_assert_no_error (err);

  data = gzochid_durable_queue_pop (queue, &string_serialization, NULL);
  g_assert_cmpstr (data, ==, "foo");
  data = gzochid_durable_queue_pop (queue, &string_serialization, NULL);
  g_assert_cmpstr (data, ==, "bar");
  
  gzochid_durable_queue_free (queue);
}

static void
test_queue_peek ()
{
  char *data = NULL;
  GError *err = NULL;
  gzochid_durable_queue *queue = gzochid_durable_queue_new (NULL);
  
  gzochid_durable_queue_offer
    (queue, &string_serialization, strdup ("foo"), NULL);

  data = gzochid_durable_queue_peek (queue, &string_serialization, NULL);
  g_assert_cmpstr (data, ==, "foo");
  
  data = gzochid_durable_queue_pop (queue, &string_serialization, NULL);
  g_assert_cmpstr (data, ==, "foo");

  g_assert_null
    (gzochid_durable_queue_peek (queue, &string_serialization, &err));
  g_assert_no_error (err);
  
  gzochid_durable_queue_free (queue);
}

static void
test_queue_pop ()
{
  GError *err = NULL;
  gzochid_durable_queue *queue = gzochid_durable_queue_new (NULL);
  
  gzochid_durable_queue_offer
    (queue, &string_serialization, strdup ("foo"), NULL);

  gzochid_durable_queue_pop (queue, &string_serialization, &err);
  g_assert_no_error (err);
  
  g_assert_null
    (gzochid_durable_queue_pop (queue, &string_serialization, &err));
  g_assert_no_error (err);
  
  gzochid_durable_queue_free (queue);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/queue/offer", test_queue_offer);
  g_test_add_func ("/queue/peek", test_queue_peek);
  g_test_add_func ("/queue/pop", test_queue_pop);

  gzochid_test_mock_data_initialize ();
  
  return g_test_run ();
}
