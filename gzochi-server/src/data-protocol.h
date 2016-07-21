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
#include <gmp.h>
#include <sys/time.h>

#include "oids.h"

#ifndef GZOCHID_DATA_PROTOCOL_H
#define GZOCHID_DATA_PROTOCOL_H

/* The following byte single-constants are "opcodes" for the data protocol, and
   correspond to the different messages that can be exchanged between the client
   (gzochi application server) and the server (gzochi meta server) participating
   in the protocol. The format of the message payload that follows each opcode 
   is opcode-specific and is described below. */

/* The following opcodes are for messages sent from the client to the server. */

/*
  Login to the data server and pass some configuration data from the client.
  Format:
  
  1 byte: Data protocol version. (0x01)
  `NULL'-terminated string: gzochid admin server base URL
    or 1 `NULL' byte if the client is not running an admin web console
*/

#define GZOCHID_DATA_PROTOCOL_LOGIN 0x10

/*
  Reserve a block of object ids from the data server. Format:
  
  `NULL'-terminated string: Name of the requesting game application
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_OIDS 0x20

/*
  Request from the data server the bytes associated with a specified key, 
  establishing a point lock on that key. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the target store
  1 byte (0x00 or 0x01) indicating whether the object should be locked for write
  2 bytes: The big-endian encoding of the length of the key; the key bytes 
    follow
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_VALUE 0x21

/*
  Request from the data server the key that falls directly after the specified 
  key establishing a range lock on the interval between the two keys. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the target store
  2 bytes: The big-endian encoding of the length of the key
    Two zeros indicates a request for the first key, else the key bytes follow
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_KEY 0x22

/* Transmit a series of object and binding modifications to the data server for
   persistence as a single, transactional unit. See 
   `gzochid_data_protocol_changeset_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET 0x30

/*
  Release the point lock on the specified object key. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the target store
  2 bytes: The big-endian encoding of the length of the key; the key bytes 
    follow
*/  

#define GZOCHID_DATA_PROTOCOL_RELEASE_KEY 0x40

/*
  Release the range lock on the key interval between between the specified start
  and end keys. specified binding name. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the target store
  2 bytes: The big-endian encoding of the length of the lower key
    Two zeros indicates the beginning of the key space, else the key bytes 
    follow
  2 bytes: The big-endian encoding of the length of the upper key
    Two zeros indicates the end of the key space, else the key bytes follow
*/  

#define GZOCHID_DATA_PROTOCOL_RELEASE_KEY_RANGE 0x42

/* The following opcodes are for messages sent from the server to the client. */

/*
  Describes a block of oids reserved for the client. Format:

  `NULL'-terminated string: Name of the requesting game application
  8 bytes: The big-endian encoding of the first oid
  2 bytes: The big-endian encoding of the length of the block in oids
*/

#define GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE 0x50

/* Contains the serialized object data stored at a particular oid. See 
   `gzochid_data_protocol_value_response_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_VALUE_RESPONSE 0x51

/* Contains the object id bound to a particular name. See 
   `gzochid_data_protocol_key_response_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_NEXT_KEY_RESPONSE 0x52

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
