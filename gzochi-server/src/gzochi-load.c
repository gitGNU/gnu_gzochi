/* gzochi-load.c: Utility for importing data from game application databases
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

#include <config.h>
#include <errno.h>
#include <getopt.h>
#include <glib.h>
#include <locale.h>
#include <libintl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "storage.h"
#include "toollib.h"

#define _(String) gettext (String)

struct load_context
{
  gzochid_storage_context *storage_context;
  gzochid_storage_store *store;
};

struct datum
{
  char *data;
  size_t data_len;
};

static struct load_context *
setup_load_context (char *data_dir, char *db, gboolean force)
{
  unsigned int flags = GZOCHID_STORAGE_CREATE | GZOCHID_STORAGE_EXCL;
  struct load_context *context = calloc (1, sizeof (struct load_context));
  char *path = g_strconcat (data_dir, "/", db, NULL);
 
  context->storage_context = gzochid_storage_initialize (data_dir);

  if (context->storage_context == NULL)
    {
      g_critical ("Failed to initialize store in %s", data_dir);
      exit (EXIT_FAILURE);
    }

  if (force)
    flags ^= GZOCHID_STORAGE_EXCL;

  context->store = gzochid_storage_open (context->storage_context, path, flags);

  if (context->store == NULL)
    {
      g_critical ("Failed to open store in %s", data_dir);
      exit (EXIT_FAILURE);
    }

  free (path);
  
  return context;
}

static void
load_data_header (char *line, size_t line_len, int line_num)
{
}

static char *
pack_line (char *line, size_t len, int line_num)
{
  size_t i = 0;
  char *packed_line = calloc (len / 2, sizeof (char));

  for (; i < len; i += 2)
    if (sscanf (line + i, "%2hhx", &packed_line[i / 2]) != 1)
      {
	g_critical ("Malformed line at row %d.", line_num);
	exit (EXIT_FAILURE);
      }

  return packed_line;
}

static struct datum
read_line (char *line, size_t line_len, int line_num)
{
  struct datum data;

  line_len -= 2;
  
  if (line_len <= 0 || line_len % 2 != 0)
    {
      g_critical ("Malformed line at row %d.", line_num);
      exit (EXIT_FAILURE);
    }
  
  data.data = pack_line (line + 1, line_len, line_num);
  data.data_len = line_len / 2;
      
  return data;
}

static void
cleanup_load_context (struct load_context *context)
{
  gzochid_storage_close (context->store);
  gzochid_storage_context_close (context->storage_context);
  
  free (context);
}

static void
load_data (char *data_dir, char *db, gboolean force)
{
  int line_num = 1;
  struct datum line = { NULL, 0 }, key, value;
  gboolean reading_key = TRUE;
  struct load_context *context = setup_load_context (data_dir, db, force);

  while ((line.data_len = getline (&line.data, &line.data_len, stdin)) != -1 
	 && strcmp (line.data, "HEADER=END\n") != 0)
    {
      load_data_header (line.data, line.data_len, line_num++);
      
      free (line.data);
      line.data = NULL;
      line.data_len = 0;
    }

  if (line.data_len == -1)
    {
      g_critical ("While reading header data: %s", strerror (errno));
      exit (EXIT_FAILURE);
    }

  line_num++;

  while ((line.data_len = getline (&line.data, &line.data_len, stdin)) != -1 
	 && strcmp (line.data, "DATA=END\n") != 0)
    {
      if (reading_key)
	{
	  key = read_line (line.data, line.data_len, line_num++);
	  reading_key = FALSE;
	}
      else 
	{
	  value = read_line (line.data, line.data_len, line_num++);
	  gzochid_storage_put 
	    (context->store, key.data, key.data_len, value.data, 
	     value.data_len);

	  free (key.data);
	  free (value.data);

	  reading_key = TRUE;
	}

      free (line.data);
      line.data = NULL;
      line.data_len = 0;
    }

  cleanup_load_context (context);
}

static const struct option longopts[] =
  {
    { "config", required_argument, NULL, 'c' },
    { "force", no_argument, NULL, 'f' },
    { "help", no_argument, NULL, 'h' },
    { "version", no_argument, NULL, 'v' },
    { NULL, 0, NULL, 0 }
  };

static void
print_version (void)
{
  fprintf (stderr, "gzochi-load (gzochi) %s\n", VERSION);

  fputs ("", stderr);
  fprintf (stderr, _("\
Copyright (C) %s Julian Graham\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
This is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"),
          "2014");
}

static void
print_help (const char *program_name)
{
  fprintf (stderr, _("\
Usage: %s [-c <CONF>] [-f] <APP_NAME or DATA_DIRECTORY>:<DB>\n\
       %s [-h | -v]\n"), program_name, program_name);

  fputs ("", stderr);
  fputs (_("\
  -c, --config        full path to gzochid.conf\n\
  -f, --force         force an import, even when the target already exists\n\
  -h, --help          display this help and exit\n\
  -v, --version       display version information and exit\n"), stderr);

  fputs ("", stderr);
  fprintf (stderr, _("\
Report bugs to: %s\n"), PACKAGE_BUGREPORT);
#ifdef PACKAGE_PACKAGER_BUG_REPORTS
  fprintf (stderr, _("Report %s bugs to: %s\n"), PACKAGE_PACKAGER,
          PACKAGE_PACKAGER_BUG_REPORTS);
#endif /* PACKAGE_PACKAGER_BUG_REPORTS */

#ifdef PACKAGE_URL
  fprintf (stderr, _("%s home page: <%s>\n"), PACKAGE_NAME, PACKAGE_URL);
#else
  fprintf (stderr, _("%s home page: <http://www.nongnu.org/%s/>\n"),
	   PACKAGE_NAME, PACKAGE);
#endif /* PACKAGE_URL */
}

int 
main (int argc, char *argv[])
{
  const char *program_name = argv[0];
  char *gzochid_conf_path = NULL;
  gboolean force = FALSE;
  int optc = 0;
  
  setlocale (LC_ALL, "");
  
  while ((optc = getopt_long (argc, argv, "+c:fhv", longopts, NULL)) != -1)
    switch (optc)
      {
      case 'c':
	gzochid_conf_path = strdup (optarg);
	break;
      case 'f':
	force = TRUE;
	break;
	
      case 'v':
	print_version ();
	exit (EXIT_SUCCESS);
	break;
      case 'h':
	print_help (program_name);
	exit (EXIT_SUCCESS);
	break;

      default:
	print_help (program_name);
	exit (EXIT_FAILURE);
      }

  if (optind != argc - 1)
    {
      print_help (program_name);
      exit (EXIT_FAILURE);
    }
  else
    {
      char **targets = gzochid_tool_parse_targets (argv[optind]);
      char *data_dir = gzochid_tool_probe_data_dir 
	(gzochid_conf_path, targets[0], TRUE);
      char *db = targets[1];

      if (db == NULL)
	{
	  print_help (program_name);
	  exit (EXIT_FAILURE);
	}
      else load_data (data_dir, db, force);
      g_strfreev (targets);
    }

  return 0;
}
