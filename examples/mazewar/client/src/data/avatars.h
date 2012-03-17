/* avatars.h: Prototypes and declarations for avatars.c
 * Copyright (C) 2012 Julian Graham
 *
 * This is free software: you can redistribute it and/or modify it
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

#ifndef MAZEWAR_AVATARS_H
#define MAZEWAR_AVATARS_H

#define MAZEWAR_AVATAR_BITS_WIDTH 384 /* The sprite sheet width in bits. */
#define MAZEWAR_AVATAR_BITS_HEIGHT 64 /* The sprite sheet height in bits. */

/* The avatar sprite data, in the form of a single bitmap in a packed PBM-like 
   format. */

unsigned char mazewar_avatar_bits[3072];

#endif /* MAZEWAR_AVATARS_H */
