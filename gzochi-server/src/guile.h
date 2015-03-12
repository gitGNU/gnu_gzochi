/* guile.h: Prototypes and declarations for guile.c
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

#ifndef GZOCHID_GUILE_H
#define GZOCHID_GUILE_H

#include <libguile.h>

SCM gzochid_guile_r6rs_raise (SCM);
SCM gzochid_guile_r6rs_raise_continuable (SCM);

SCM gzochid_guile_invoke (SCM, SCM, SCM);

void gzochid_guile_add_to_load_path (char *);
void gzochid_guile_init (void);

#endif /* GZOCHID_GUILE_H */
