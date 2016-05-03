/* data-protocol.c: Data structures, formats used by the gzochi meta dataserver
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

#include <assert.h>
#include <glib.h>
#include <gmp.h>
#include <gzochi-common.h>
#include <stdlib.h>
#include <string.h>

#include "data-protocol.h"
#include "oids.h"

/* Finds the bounds of the `NULL'-terminated string that begins at `bytes', 
   returning a pointer to that string and setting `str_len' appropriately.
   Returns `NULL' if the string is not `NULL'-terminated. */

static char *
read_str (const unsigned char *bytes, const size_t bytes_len, size_t *str_len)
{
  unsigned char *end = memchr (bytes, 0, bytes_len);

  if (end == NULL)
    return NULL;
  else
    {
      if (str_len != NULL)
	*str_len = end - bytes + 1;
      return (char *) bytes;
    }
}

/* Reads the run length encoded (via a two-byte big-endian prefix) byte buffer
   and returns it. */

static GBytes *
read_bytes (const unsigned char *bytes, const size_t bytes_len)
{
  short prefix = 0;
  
  if (bytes_len < 2)
    return NULL;
  
  prefix = gzochi_common_io_read_short (bytes, 0);

  if (prefix > bytes_len - 2)
    return NULL;

  return g_bytes_new (bytes + 2, prefix);
}

/* Serializes the specified byte buffer to the specified byte array, writing a
   two-byte big-endian prefix indicating the length of the buffer. */

static void
write_bytes (GBytes *data, GByteArray *arr)
{
  size_t len = 0, arr_len = arr->len;
  const unsigned char *bytes = g_bytes_get_data (data, &len);

  /* Grow the array by two bytes. */
  
  g_byte_array_set_size (arr, arr_len + 2);

  /* Write the prefix directly to the buffer. */

  gzochi_common_io_write_short (len, arr->data, arr_len);
  
  if (bytes != NULL)  
    g_byte_array_append (arr, bytes, len);
}

/* Reads two four-byte big-endian integer representations into a `struct 
   timeval', as the `tv_sec' and `tv_usec' fields, respectively. */

static gboolean
read_timeval (const unsigned char *bytes, const size_t bytes_len,
	      struct timeval *ts)
{
  assert (ts != NULL);

  if (bytes_len < 8)
    return FALSE;
  else
    {
      ts->tv_sec = gzochi_common_io_read_int (bytes, 0);
      ts->tv_usec = gzochi_common_io_read_int (bytes, 4);
      return TRUE;
    }
}

/* Serializes the specified `struct timeval' to the specified byte array, in the
   form of two four-byte big-endian integer representations taken from, 
   respectively, the `tv_sec' and `tv_usec' fields. */

static void
write_timeval (const struct timeval *tv, GByteArray *arr)
{
  size_t len = arr->len;

  /* Grow the array by eight bytes. */
  
  g_byte_array_set_size (arr, len + 8);

  /* Write the ints directly to the buffer. */
  
  gzochi_common_io_write_int (tv->tv_sec, arr->data, len);
  gzochi_common_io_write_int (tv->tv_usec, arr->data, len + 4);
}

/* Serializes the specified oid to the specified byte array by converting the
   oid to its `NULL'-terminated, hexadecimal string representation. */

static void
write_oid (mpz_t oid, GByteArray *arr)
{
  size_t len = arr->len;

  /* Grow the array by the expected size of the string. */
  
  g_byte_array_set_size (arr, arr->len + mpz_sizeinbase (oid, 16) + 1);
  mpz_get_str ((char *) arr->data + len, 16, oid);
}

gzochid_data_reserve_oids_response *
gzochid_data_reserve_oids_response_new (char *app,
					gzochid_data_oids_block *block)
{
  gzochid_data_reserve_oids_response *response =
    g_slice_alloc (sizeof (gzochid_data_reserve_oids_response));

  response->app = strdup (app);
  
  mpz_init_set (response->block.block_start, block->block_start);
  response->block.block_size = block->block_size;
  
  return response;
}

void
gzochid_data_reserve_oids_response_free
(gzochid_data_reserve_oids_response *response)
{
  free (response->app);
  mpz_clear (response->block.block_start);
  g_slice_free (gzochid_data_reserve_oids_response, response);
}

void
gzochid_data_protocol_reserve_oids_response_write
(gzochid_data_reserve_oids_response *response, GByteArray *arr)
{
  size_t len = 0;
  g_byte_array_append
    (arr, (unsigned char *) response->app, strlen (response->app) + 1);
  write_oid (response->block.block_start, arr);

  len = arr->len;

  /* Grow the array by two bytes. */
  
  g_byte_array_set_size (arr, len + 2);

  /* Write the block size directly to the buffer. */
  
  gzochi_common_io_write_short (response->block.block_size, arr->data, len);
}

gzochid_data_reserve_oids_response *
gzochid_data_protocol_reserve_oids_response_read (GBytes *data)
{
  size_t len = 0, str_len = 0;
  const unsigned char *bytes = g_bytes_get_data (data, &len);
  char *app = read_str (bytes, len, &str_len);
  char *oid_str = NULL;

  if (app == NULL || str_len == 0)
    return NULL;

  len -= str_len;

  oid_str = read_str (bytes + str_len, len, &str_len);

  if (oid_str == NULL)
    return NULL;  
  else
    {
      len -= str_len;
      
      if (str_len > 1 && len >= 2)
	{
	  gzochid_data_oids_block oids_block;	  
	  gzochid_data_reserve_oids_response *response = NULL;

	  if (mpz_init_set_str (oids_block.block_start, oid_str, 16) < 0)
	    return NULL;
	  
	  oids_block.block_size = gzochi_common_io_read_short
	    ((unsigned char *) oid_str + str_len, 0);
	  response = gzochid_data_reserve_oids_response_new (app, &oids_block);
	  mpz_clear (oids_block.block_start);

	  return response;
	}
      else return NULL;
    }
}

gzochid_data_response *
gzochid_data_response_new (char *app, char *store, gboolean success,
			   GBytes *data)
{
  gzochid_data_response *response = g_slice_alloc
    (sizeof (gzochid_data_response));

  response->app = strdup (app);
  response->store = strdup (store);
  response->success = success;

  if (data != NULL)
    {
      assert (success);
      response->data = g_bytes_ref (data);
    }
  else response->data = NULL;

  return response;
}

void
gzochid_data_response_free (gzochid_data_response *response)
{
  free (response->app);
  free (response->store);
  
  if (response->data != NULL)
    g_bytes_unref (response->data);

  g_slice_free (gzochid_data_response, response);
}

void
gzochid_data_protocol_response_write
(gzochid_data_response *response, GByteArray *arr)
{
  g_byte_array_append
    (arr, (unsigned char *) response->app, strlen (response->app) + 1);
  g_byte_array_append
    (arr, (unsigned char *) response->store, strlen (response->store) + 1);
  g_byte_array_append (arr, (unsigned char *) &response->success, 1);
  
  if (response->success)
    {
      if (response->data != NULL)
	write_bytes (response->data, arr);

      /* Write two-byte prefix indicating an empty buffer. */
      
      else g_byte_array_append
	     (arr, (unsigned char *) &(unsigned char[]) { 0, 0 }, 2);

    }
  else write_timeval (&response->timeout, arr);
}

gzochid_data_response *
gzochid_data_protocol_response_read (GBytes *data)
{
  size_t len = 0, offset = 0, str_len = 0;
  const unsigned char *bytes = g_bytes_get_data (data, &len);
  char *app = read_str (bytes, len, &str_len);
  char *store = NULL;
  
  if (app == NULL || str_len == 0)
    return NULL;
  
  len -= str_len;
  offset += str_len;

  store = read_str (bytes + offset, len, &str_len);

  if (store == NULL || str_len == 0)
    return NULL;

  len -= str_len;
  offset += str_len;
  
  if (len-- <= 0)
    return NULL;

  if (bytes[offset])
    {
      GBytes *obj_bytes = read_bytes (bytes + offset + 1, len);
      
      if (obj_bytes == NULL)
	return NULL;
      else if (g_bytes_get_size (obj_bytes) == 0)
	{
	  /* If the buffer was empty, free the wrapper. */
	  
	  g_bytes_unref (obj_bytes);
	  return gzochid_data_response_new (app, store, TRUE, NULL);
	}
      else
	{
	  gzochid_data_response *response =
	    gzochid_data_response_new (app, store, TRUE, obj_bytes);

	  /* Unref the buffer to give exclusive ownership to the response
	     object. */

	  g_bytes_unref (obj_bytes);
	  
	  return response;
	}
    }
  else
    {
      struct timeval timeout;
      
      if (len < 8 || !read_timeval (bytes + offset + 1, len, &timeout))
	return NULL;
      else
	{
	  gzochid_data_response *response =
	    gzochid_data_response_new (app, store, FALSE, NULL);
	  response->timeout = timeout;

	  return response;
	}      
    }
}

gzochid_data_changeset *
gzochid_data_changeset_new (char *app, GArray *changes)
{
  gzochid_data_changeset *changeset =
    g_slice_alloc (sizeof (gzochid_data_changeset));

  changeset->app = strdup (app);
  changeset->changes = g_array_ref (changes);
  
  return changeset;
}

/* Frees the memory held by the internal pointers for a change record. The 
   record itself is part of the change array and will be cleaned up when the 
   array is destroyed. */

static void
change_clear (gzochid_data_change *change)
{
  free (change->store);
  g_bytes_unref (change->key);
  
  if (!change->delete)
    g_bytes_unref (change->data);
}

/* Clears all the change records in the array up to the specified index; useful
   when not every record may have been initialized. */

static void
changes_clear (GArray *arr, int up_to)
{
  int i = 0;

  for (; i < up_to; i++)
    change_clear (&g_array_index (arr, gzochid_data_change, i));
}

void
gzochid_data_changeset_free (gzochid_data_changeset *changeset)
{
  free (changeset->app);

  changes_clear (changeset->changes, changeset->changes->len);
  g_array_unref (changeset->changes);
  
  g_slice_free (gzochid_data_changeset, changeset);
}

/* Initializes the specified object change record from the specified buffer. On
   success, updates the size pointer to reflect the bytes consumed from the
   buffer, and returns `TRUE'; on failure, returns `FALSE' and leaves the 
   pointer untouched. */

gboolean
change_read (const unsigned char *bytes, size_t len,
	     gzochid_data_change *change, size_t *ret)
{
  size_t size = 0, str_len = 0;
  char *store = read_str (bytes, len, &str_len);
  GBytes *data = NULL;
  
  if (store == NULL || str_len <= 1)
    return FALSE;

  change->store = strdup (store);
  
  len -= str_len;
  size += str_len;

  change->key = read_bytes (bytes + size, len);
  if (change->key == NULL)
    {
      free (change->store);
      return FALSE;
    }
  else 
    {
      size_t key_size = g_bytes_get_size (change->key);

      if (key_size == 0)
	{
	  free (change->store);
	  g_bytes_unref (change->key);
	  return FALSE;
	}
      else
	{
	  size += key_size + 2;
	  len -= key_size + 2;

	  data = read_bytes (bytes + size, len);

	  if (data == NULL)
	    {
	      free (change->store);
	      g_bytes_unref (change->key);
	      return FALSE;
	    }
	  else
	    {
	      size_t data_size = g_bytes_get_size (data);

	      if (data_size == 0)
		{
		  g_bytes_unref (data);
		  change->delete = TRUE;
		}
	      else change->data = data;

	      *ret = size + 2 + data_size;
	      return TRUE;
	    }
	}
    }
}

/* Serializes the specified change record to the specified byte array. */

static void
change_write (gzochid_data_change *change, GByteArray *arr)
{
  g_byte_array_append
    (arr, (unsigned char *) change->store, strlen (change->store) + 1);
  write_bytes (change->key, arr);
  
  if (change->delete)

    /* Write a two-byte zero prefix to indicate an object deletion. */
    
    g_byte_array_append (arr, (unsigned char *) &(unsigned char[]) { 0, 0 }, 2);
  else write_bytes (change->data, arr);
}

void
gzochid_data_protocol_changeset_write (gzochid_data_changeset *changeset,
				       GByteArray *arr)
{
  int i = 0;
  size_t len = 0;
  
  g_byte_array_append
    (arr, (unsigned char *) changeset->app, strlen (changeset->app) + 1);

  len = arr->len;

  /* Grow the array by two bytes. */
  
  g_byte_array_set_size (arr, len + 2);

  /* Write the object change count directly to the buffer. */
  
  gzochi_common_io_write_short (changeset->changes->len, arr->data, len);
  
  for (; i < changeset->changes->len; i++)
    {
      gzochid_data_change *change = &g_array_index
	(changeset->changes, gzochid_data_change, i);
      change_write (change, arr);
    }
}

gzochid_data_changeset *
gzochid_data_protocol_changeset_read (GBytes *data)
{
  int i = 0;
  size_t len = 0, offset = 0;
  short num_changes;
  const unsigned char *bytes = g_bytes_get_data (data, &len);
  GArray *changes = NULL;
  gzochid_data_changeset *changeset = NULL;

  char *app = read_str (bytes, len, &offset);
  
  if (app == NULL || offset == 0)
    return NULL;

  len -= offset;

  if (len <= 2)
    return NULL;

  num_changes = gzochi_common_io_read_short (bytes, offset);

  offset += 2;
  len -= 2;
  
  if (num_changes > 0)
    {
      changes = g_array_sized_new
	(FALSE, TRUE, sizeof (gzochid_data_change), num_changes);
      g_array_set_size (changes, num_changes);

      for (; i < num_changes && len > 0; i++)
	{
	  size_t size = 0;
	  gzochid_data_change *change = &g_array_index
	    (changes, gzochid_data_change, i);
	  
	  if (change_read (bytes + offset, len, change, &size))
	    {
	      offset += size;
	      len -= size;
	    }
	  else
	    {
	      /* If a bad change record is detected, clean up any 
		 previously-initialized records. */
	      
	      changes_clear (changes, i);
	      g_array_unref (changes);
	      return NULL;
	    }
	}
    }
  else changes = g_array_new (FALSE, FALSE, sizeof (gzochid_data_change));
  
  changeset = gzochid_data_changeset_new (app, changes);

  /* Transfer ownership to the changeset record. */
  
  g_array_unref (changes);
  return changeset;
}
