/* gzochi-dump.c: Utility for exporting data from game application databases
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

static void 
dump_header (FILE *to)
{
  fprintf (to, "VERSION=3\n");
  fprintf (to, "format=bytevalue\n");
  fprintf (to, "type=btree\n");
  fprintf (to, "HEADER=END\n");
}

static void
dump_key_value (char *k, size_t k_len, char *v, size_t v_len, FILE *to)
{
  int i = 0;

  assert (k != NULL);
  assert (v != NULL);
  
  fprintf (to, " ");
  for (; i < k_len; i++)
    fprintf (to, "%.2hhx", k[i]);
  fprintf (to, "\n ");
  for (i = 0; i < v_len; i++)
    fprintf (to, "%.2hhx", v[i]);
  fprintf (to, "\n");	  
}

static void
dump_store 
(gzochid_storage_transaction *tx, gzochid_storage_store *store, FILE *to) 
{
  size_t key_len = 0;
  char *key = NULL;
  
  dump_header (to);
  key = gzochid_storage_transaction_first_key (tx, store, &key_len);
  
  while (key != NULL)
    {
      char *old_key = key;
      size_t old_key_len = key_len;

      size_t value_len = 0;
      char *value = gzochid_storage_transaction_get 
	(tx, store, key, key_len, &value_len);

      dump_key_value (key, key_len, value, value_len, to);
      
      free (value);
      
      key = gzochid_storage_transaction_next_key 
	(tx, store, key, old_key_len, &key_len);

      free (old_key);
    }
  
  fprintf (to, "DATA=END\n");
}

static void
dump_single_inner 
(gzochid_storage_context *context, gzochid_storage_transaction *tx, 
 char *data_dir, char *db, FILE *to)
{
  char *db_path = g_strconcat (data_dir, "/", db, NULL);
  gzochid_storage_store *store = gzochid_tool_open_store (context, db_path);

  dump_store (tx, store, to);

  gzochid_storage_close (store);
  g_free (db_path);
}

static void
dump_single (char *data_dir, char *db, FILE *to)
{
  gzochid_storage_context *context = gzochid_storage_initialize (data_dir);

  if (context == NULL)
    {
      g_critical ("Failed to initialize store in %s", data_dir);
      exit (EXIT_FAILURE);
    }
  else 
    {
      gzochid_storage_transaction *tx = 
	gzochid_storage_transaction_begin (context);
      dump_single_inner (context, tx, data_dir, db, to);
      gzochid_storage_transaction_rollback (tx);
      gzochid_storage_context_close (context);
    }
}

static FILE *
open_dump_output_file (char *output_dir, char *file)
{
  FILE *output_file = NULL;
  char *filename = g_strconcat (output_dir, "/", file, NULL);
  
  output_file = fopen (filename, "w");
  if (output_file == NULL)
    {
      g_critical 
	("Failed to open file %s for writing: %s", filename, strerror (errno));
      exit (EXIT_FAILURE);
    }
  
  g_free (filename);
  return output_file;
}

static void
dump_all (char *data_dir, char *output_dir)
{
  gzochid_storage_context *context = gzochid_storage_initialize (data_dir);

  if (context == NULL)
    {
      g_critical ("Failed to initialize store in %s", data_dir);
      exit (EXIT_FAILURE);
    }
  else
    {
      FILE *meta_dump = open_dump_output_file (output_dir, "meta.dump");
      FILE *oids_dump = open_dump_output_file (output_dir, "oids.dump");
      FILE *names_dump = open_dump_output_file (output_dir, "names.dump");

      gzochid_storage_transaction *tx = 
	gzochid_storage_transaction_begin (context);

      dump_single_inner (context, tx, data_dir, "meta", meta_dump);
      dump_single_inner (context, tx, data_dir, "oids", oids_dump);
      dump_single_inner (context, tx, data_dir, "names", names_dump);

      fclose (meta_dump);
      fclose (oids_dump);
      fclose (names_dump);

      gzochid_storage_transaction_rollback (tx);
      gzochid_storage_context_close (context);
    }
}

static const struct option longopts[] =
  {
    { "config", required_argument, NULL, 'c' },
    { "output", required_argument, NULL, 'o' },
    { "help", no_argument, NULL, 'h' },
    { "version", no_argument, NULL, 'v' },
    { NULL, 0, NULL, 0 }
  };

static void
print_version (void)
{
  fprintf (stderr, "gzochi-dump (gzochi) %s\n", VERSION);

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
Usage: %s [-c <CONF>] [-o <OUTPUT_DIR>] <APP_NAME or DATA_DIR>[:<DB>]\n\
       %s [-h | -v]\n"), program_name, program_name);
  
  fputs ("", stderr);
  fputs (_("\
  -c, --config        full path to gzochid.conf\n\
  -o, --output        the output directory, if dumping all databases; or\n\
                      the output file, if dumping a single database\n\
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
  char *output = NULL;
  int optc = 0;
  
  setlocale (LC_ALL, "");
  
  while ((optc = getopt_long (argc, argv, "+c:o:hv", longopts, NULL)) != -1)
    switch (optc)
      {
      case 'c':
	gzochid_conf_path = strdup (optarg);
	break;
      case 'o':
	output = strdup (optarg);
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
	(gzochid_conf_path, targets[0], FALSE);
      char *db = targets[1];

      if (db == NULL)
	{
	  if (output == NULL)
	    output = g_get_current_dir ();
	  dump_all (data_dir, output);
	  free (output);
	}
      else 
	{
	  FILE *output_file = NULL;

	  if (output == NULL)
	    output_file = stdout;
	  else 
	    {
	      if (g_file_test (output, G_FILE_TEST_IS_DIR))
		{
		  g_critical ("%s is a directory.", output);
		  exit (EXIT_FAILURE);
		}
	      else output_file = open_dump_output_file (output, db);

	      free (output);
	    }

	  dump_single (data_dir, db, output_file);
	}

      g_strfreev (targets);
    }

  return 0;
}
