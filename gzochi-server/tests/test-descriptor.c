/* test-descriptor.c: Test routines for descriptor.c in gzochid.
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
#include <string.h>

#include "app.h"
#include "descriptor.h"

#ifndef HAVE_FMEMOPEN
#include "fmemopen.h"
#endif /* HAVE_FMEMOPEN */

static gboolean
ignore_warnings (const gchar *log_domain, GLogLevelFlags log_level,
		 const gchar *message, gpointer user_data)
{
  if (log_level & G_LOG_LEVEL_CRITICAL
      || log_level & G_LOG_LEVEL_WARNING)
    return FALSE;
  else return log_level & G_LOG_FLAG_FATAL;
}

static gboolean
list_equal (const GList *l1, const GList *l2, GCompareFunc f)
{
  const GList *lp1 = l1;
  const GList *lp2 = l2;

  while (lp1 != NULL)
    {
      if (f (lp1->data, lp2->data) != 0)
	return FALSE;

      lp1 = lp1->next;
      lp2 = lp2->next;
    }
  
  return lp2 == NULL;
}

static void 
test_descriptor_parse_ready ()
{
  char *descriptor_text = "<?xml version=\"1.0\" ?>\n\
<game name=\"test\">\n\
  <description>Test</description>\n\
  <load-paths />\n\
  <initialized>\n\
    <callback module=\"test\" procedure=\"initialized\" />\n\
  </initialized>\n\
  <logged-in><callback module=\"test\" procedure=\"logged-in\" /></logged-in>\n\
  <ready><callback module=\"test\" procedure=\"ready\" /></ready>\n\
</game>";

  FILE *descriptor_file = 
    fmemopen (descriptor_text, strlen (descriptor_text), "r");
  gzochid_application_descriptor *descriptor =
    gzochid_config_parse_application_descriptor (descriptor_file);
  GList *module = g_list_append (NULL, "test");

  g_assert_nonnull (descriptor->ready);
  g_assert_cmpstr (descriptor->ready->procedure, ==, "ready");
  g_assert (list_equal (module, descriptor->ready->module, 
			(GCompareFunc) strcmp));

  fclose (descriptor_file);
}

static void 
test_descriptor_parse_error ()
{
  char *descriptor_text = "<?xml version=\"1.0\" ?>\n\
<game name=\"test\">\n\
  <description>Test</description>\n\
  <load-paths />\n\
</game>";

  FILE *descriptor_file =
    fmemopen (descriptor_text, strlen (descriptor_text), "r");

  g_test_log_set_fatal_handler (ignore_warnings, NULL);
  
  g_assert_null (gzochid_config_parse_application_descriptor (descriptor_file));
  fclose (descriptor_file);
}

int 
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/descriptor/parse/ready", test_descriptor_parse_ready);
  g_test_add_func ("/descriptor/parse/error", test_descriptor_parse_error);

  return g_test_run ();
}
