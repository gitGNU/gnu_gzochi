/* tx.h: Prototypes and declarations for tx.c
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

#ifndef GZOCHID_API_TX_H
#define GZOCHID_API_TX_H

/* The public initializer function for the `(gzochi tx)' API. This function
   must be called as part of the "boot" process for the gzochid Scheme API.
*/

void gzochid_api_tx_init (void);

#endif /* GZOCHID_API_TX_H */
