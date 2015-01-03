/* mock-data.h: Prototypes and declarations for mock-data.c
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

#ifndef GZOCHID_TEST_MOCK_DATA_H
#define GZOCHID_TEST_MOCK_DATA_H

#include <glib.h>
#include <gmp.h>

#include "app.h"
#include "io.h"

void 
gzochid_test_mock_data_store 
(gzochid_application_context *, gzochid_io_serialization *, gpointer, mpz_t);

void 
gzochid_test_mock_data_initialize (void);

#endif /* GZOCHID_TEST_MOCK_DATA_H */