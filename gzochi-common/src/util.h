/* util.h: General-purpose preprocessor definitions for libgzochi-common
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

#ifndef GZOCHI_COMMON_UTIL_H
#define GZOCHI_COMMON_UTIL_H

#ifndef MAX
#define MAX(a,b)	      \
  ({ __typeof__ (a) _a = (a); \
    __typeof__ (b) _b = (b);  \
    _a > _b ? _a : _b; })
#endif /* MAX */

#ifndef MIN
#define MIN(a,b)	      \
  ({ __typeof__ (a) _a = (a); \
    __typeof__ (b) _b = (b);  \
    _a < _b ? _a : _b; })
#endif /* MIN */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#endif /* GZOCHI_COMMON_UTIL_H */
