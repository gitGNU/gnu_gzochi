/* util.h: Prototypes and declarations for util.c
 * Copyright (C) 2016 Julian Graham
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

void gzochid_util_serialize_boolean (gboolean, GByteArray *);
void gzochid_util_serialize_int (int, GByteArray *);

/* Writes the big-endian representation of the specified 64-bit unsigned long to
   the specified byte array. */

void gzochid_util_serialize_uint64 (guint64, GByteArray *);

/* A more intentional alias for `gzochid_util_serialize_uint64'. */

void gzochid_util_serialize_oid (guint64, GByteArray *);

void gzochid_util_serialize_mpz (mpz_t, GByteArray *);
void gzochid_util_serialize_bytes (unsigned char *, int, GByteArray *);
void gzochid_util_serialize_string (char *, GByteArray *);
void gzochid_util_serialize_list 
(GList *, void (*) (gpointer, GByteArray *), GByteArray *);
void gzochid_util_serialize_sequence
(GSequence *, void (*) (gpointer, GByteArray *), GByteArray *);
void gzochid_util_serialize_hash_table
(GHashTable *, 
 void (*) (gpointer, GByteArray *), 
 void (*) (gpointer, GByteArray *), 
 GByteArray *);
void gzochid_util_serialize_timeval (struct timeval, GByteArray *);

gboolean gzochid_util_deserialize_boolean (GByteArray *);
int gzochid_util_deserialize_int (GByteArray *);

/* Reads the big-endian representation of a 64-bit unsigned long from the 
   specified byte array, erasing eight bytes from the array (which is assumed to
   have at least eight bytes to spare. */

guint64 gzochid_util_deserialize_uint64 (GByteArray *);

/* A more intentional alias for `gzochid_util_deserialize_uint64'. */

guint64 gzochid_util_deserialize_oid (GByteArray *);

void gzochid_util_deserialize_mpz (GByteArray *, mpz_t);
unsigned char *gzochid_util_deserialize_bytes (GByteArray *, int *);
char *gzochid_util_deserialize_string (GByteArray *);
GList *gzochid_util_deserialize_list
(GByteArray *, gpointer (*) (GByteArray *));
GSequence *gzochid_util_deserialize_sequence 
(GByteArray *, gpointer (*) (GByteArray *), GDestroyNotify);
GHashTable *gzochid_util_deserialize_hash_table
(GByteArray *, 
 GHashFunc, 
 GEqualFunc, 
 gpointer (*) (GByteArray *), 
 gpointer (*) (GByteArray *));
struct timeval gzochid_util_deserialize_timeval (GByteArray *);

gint gzochid_util_string_data_compare (gconstpointer, gconstpointer, gpointer);

/* Comparison function that sorts `NULL' before all non-`NULL' values. Use for
   comparing the lower bounds of range locks. */

gint gzochid_util_bytes_compare_null_first (gconstpointer, gconstpointer);

/* Comparison function that sorts `NULL' after all non-`NULL' values. Use for
   comparing the upper bounds of range locks. */

gint gzochid_util_bytes_compare_null_last (gconstpointer, gconstpointer);

#endif /* GZOCHID_UTIL_H */
