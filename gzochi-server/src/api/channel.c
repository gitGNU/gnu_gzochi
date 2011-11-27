/* channel.c: Primitive functions for user-facing gzochid channel API
 * Copyright (C) 2011 Julian Graham
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

#include <libguile.h>

#include "channel.h"

void gzochid_api_channel_init (void)
{
  SCM current_module = scm_current_module ();
  SCM gzochi_private_channel = scm_c_resolve_module ("gzochi private channel");
  scm_set_current_module (gzochi_private_channel);

  #include "channel.x"

  scm_set_current_module (current_module);
}
