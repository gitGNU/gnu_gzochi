/* data-protocol.h: Prototypes and declarations for data-protocol.c
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

#include <glib.h>
#include <sys/time.h>

#include "oids.h"

#ifndef GZOCHID_DATA_PROTOCOL_H
#define GZOCHID_DATA_PROTOCOL_H

/* A response to a request to reserve a block of oids. */

struct _gzochid_data_reserve_oids_response
{
  char *app; /* The requesting application name. */
  gzochid_data_oids_block block; /* The reserved block of oids. */
};

typedef struct _gzochid_data_reserve_oids_response
gzochid_data_reserve_oids_response;

/* A response to a request. */

struct _gzochid_data_response
{
  char *app; /* The requesting application name. */
  char *store; /* The target store name. */
  gboolean success; /* Whether the request was successful. */

  union
  {
    /* If the request was successful, the requested data, or `NULL' if the no
       such data exists. */

    GBytes *data; 

    /* If the request was unsuccessful, the amount of time to wait before 
       retrying. */

    struct timeval timeout; 
  };
};

typedef struct _gzochid_data_response gzochid_data_response;

/* A change to the key-value binding to be made as part of a changeset. */

struct _gzochid_data_change
{
  char *store; /* The target store name. */
  gboolean delete;  /* Whether the binding should be deleted. */
  GBytes *key; /* The target key. */
  GBytes *data; /* The new data for the key, if not deleting, else `NULL'. */
};

typedef struct _gzochid_data_change gzochid_data_change;

/* A set of changes to objects and bindings to be persisted. */

struct _gzochid_data_changeset
{
  char *app; /* The requesting application name. */

  /* An array of `gzochid_data_change' structs. */
  
  GArray *changes;

  GDestroyNotify free_func; /* Change cleanup function, optionally `NULL'. */
};

typedef struct _gzochid_data_changeset gzochid_data_changeset;

/*
  Construct and return a new oid reservation response with the specified 
  application name and oid block.

  The pointer returned by this function should be freed with 
  `gzochid_data_reserve_oids_response_free'.
*/

gzochid_data_reserve_oids_response *gzochid_data_reserve_oids_response_new
(char *, gzochid_data_oids_block *);

/* Free the specified oid reservation response. */

void gzochid_data_reserve_oids_response_free
(gzochid_data_reserve_oids_response *);

/* 
  Serialize the specified oid reservation response to the specified byte array.
  See `GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE' for format details.
*/

void gzochid_data_protocol_reserve_oids_response_write
(gzochid_data_reserve_oids_response *, GByteArray *);

/*
  Deserialize and return an oid reservation response from the specified byte 
  buffer, or return `NULL' if the buffer does not contain a 
  correctly-serialized response. 

  The pointer returned by this function should be freed with 
  `gzochid_data_reserve_oids_response_free'.
*/

gzochid_data_reserve_oids_response *
gzochid_data_protocol_reserve_oids_response_read (GBytes *);

/*
  Construct and return a new response with the specified application and store
  names, success flag, and value bytes; this last argument may be `NULL' to
  indicate the absence of data for the requested key.

  The pointer returned by this function should be freed with 
  `gzochid_data_response_free'.
*/

gzochid_data_response *
gzochid_data_response_new (char *, char *, gboolean, GBytes *);

/* Free the specified response. */

void gzochid_data_response_free (gzochid_data_response *);

/* 
  Serialize the specified object response to the specified byte array. Format:

  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the target store
  1 byte: 0x01 indicating success (and that value data follows), 
    0x00 indicating failure
  2 bytes: The big-endian encoding of the length of the value
    Two zeros indicates the absence of a value; else the value data follows
*/

void gzochid_data_protocol_response_write
(gzochid_data_response *, GByteArray *);

/*
  Deserialize and return an object response from the specified byte buffer,
  or return `NULL' if the buffer does not contain a correctly-serialized 
  response. 

  The pointer returned by this function should be freed with 
  `gzochid_data_response_free'.
*/

gzochid_data_response *gzochid_data_protocol_response_read (GBytes *);

/* Create and return a new changeset with the specified gzochi game application
   name and change array. */

gzochid_data_changeset *gzochid_data_changeset_new (char *, GArray *);

/* Create and return a new changeset with the specified gzochi game application
   name and change array. The `GDestroyNotify' function, if provided, will be
   applied to each element in the change array when 
   `gzochid_data_changeset_free' is called.*/

gzochid_data_changeset *gzochid_data_changeset_new_with_free_func
(char *, GArray *, GDestroyNotify);

/* Free the specified changeset. */

void gzochid_data_changeset_free (gzochid_data_changeset *);

/*
  Serialize the specified changeset object to the specified byte array. Format:

  `NULL'-terminated string: Name of the requesting game application
  2 bytes: The big-endian encoding of the number of changes
  [changes; see below]

  The format of each change is:

  `NULL'-terminated string: Name of the target store
  2 bytes: The big-endian encoding of the length of the key; key bytes follow
  2 bytes: The big-endian encoding of the length of the value
    Two zeros indicates a deletion; else the data follows
 */

void gzochid_data_protocol_changeset_write
(gzochid_data_changeset *, GByteArray *);

/*
  Deserialize and return a changeset from the specified byte buffer, or return
  `NULL' if the buffer does not contain a correctly-serialized changeset. 

  The pointer returned by this function should be freed with 
  `gzochid_data_changeset_free'.
*/

gzochid_data_changeset *gzochid_data_protocol_changeset_read (GBytes *);

#endif /* GZOCHID_DATA_PROTOCOL_H */
