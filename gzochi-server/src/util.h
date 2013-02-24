/* util.h: Prototypes and declarations for util.c
 * Copyright (C) 2013 Julian Graham
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

#ifndef GZOCHID_UTIL_H
#define GZOCHID_UTIL_H

#include <glib.h>
#include <gmp.h>
#include <sys/time.h>

void gzochid_util_serialize_boolean (gboolean, GString *);
void gzochid_util_serialize_int (int, GString *);
void gzochid_util_serialize_mpz (mpz_t, GString *);
void gzochid_util_serialize_bytes (unsigned char *, int, GString *);
void gzochid_util_serialize_string (char *, GString *);
void gzochid_util_serialize_list 
(GList *, void (*) (gpointer, GString *), GString *);
void gzochid_util_serialize_sequence
(GSequence *, void (*) (gpointer, GString *), GString *);
void gzochid_util_serialize_hash_table
(GHashTable *, 
 void (*) (gpointer, GString *), 
 void (*) (gpointer, GString *), 
 GString *);
void gzochid_util_serialize_timeval (struct timeval, GString *);

gboolean gzochid_util_deserialize_boolean (GString *);
int gzochid_util_deserialize_int (GString *);
void gzochid_util_deserialize_mpz (GString *, mpz_t);
unsigned char *gzochid_util_deserialize_bytes (GString *, int *);
char *gzochid_util_deserialize_string (GString *);
GList *gzochid_util_deserialize_list (GString *, gpointer (*) (GString *));
GSequence *gzochid_util_deserialize_sequence 
(GString *, gpointer (*) (GString *));
GHashTable *gzochid_util_deserialize_hash_table
(GString *, 
 GHashFunc, 
 GEqualFunc, 
 gpointer (*) (GString *), 
 gpointer (*) (GString *));
struct timeval gzochid_util_deserialize_timeval (GString *);

gint gzochid_util_string_data_compare (gconstpointer, gconstpointer, gpointer);

#endif /* GZOCHID_UTIL_H */
