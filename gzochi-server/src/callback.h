/* callback.h: Prototypes and declarations for callback.c
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

#ifndef GZOCHID_CALLBACK_H
#define GZOCHID_CALLBACK_H

#include <glib.h>
#include <gmp.h>

#include "io.h"

struct _gzochid_application_callback
{
  GList *module;
  char *procedure;
  mpz_t scm_oid;
};

typedef struct _gzochid_application_callback gzochid_application_callback;

gzochid_io_serialization gzochid_application_callback_serialization;

gzochid_application_callback *gzochid_application_callback_new 
(char *, GList *, mpz_t);
void gzochid_application_callback_free (gzochid_application_callback *);

#endif /* GZOCHID_CALLBACK_H */
