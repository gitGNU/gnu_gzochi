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
  Request from the data server the serialized form of the object with a 
  specified id, establishing a point lock on that id. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  1 byte (0x00 or 0x01) indicating whether the object should be locked for write
  `NULL'-terminated string: Hexadecimal string representation of the target oid
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_OBJECT 0x21

/*
  Request from the data server the oid to which the specified name is bound,
  establishing a point lock on that name. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  1 byte (0x00 or 0x01) indicating whether the binding should be locked for
    write
  `NULL'-terminated string: The target binding name
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_BINDING 0x22

/*
  Request from the data server the name of the binding that falls directly
  after the specified binding name, establishing a range lock on the key 
  interval between the two keys. Format:
   
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: The target binding name
    or 1 `NULL' byte to request the first binding
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_NEXT_BINDING 0x23

/* Transmit a series of object and binding modifications to the data server for
   persistence as a single, transactional unit. See 
   `gzochid_data_protocol_changeset_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_SUBMIT_CHANGESET 0x30

/*
  Release the point lock on the specified object id. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Hexadecimal string representation of the target oid
*/  

#define GZOCHID_DATA_PROTOCOL_RELEASE_OBJECT 0x40

/*
  Release the point lock on the specified binding name. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: The target binding name
*/  

#define GZOCHID_DATA_PROTOCOL_RELEASE_BINDING 0x41

/*
  Release the range lock on the key interval between between the specified start
  and end keys. specified binding name. Format:
  
  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: The first binding in the key range
    or 1 `NULL' byte to indicate the beginning of the key space
  `NULL'-terminated string: The last binding in the key range
    or 1 `NULL' byte to indicate the end of the key space
*/  

#define GZOCHID_DATA_PROTOCOL_RELEASE_BINDING_RANGE 0x42

/* The following opcodes are for messages sent from the server to the client. */

/*
  Describes a block of oids reserved for the client. Format:

  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Hexadecimal string representation of the first oid
    in the reserved block
  2 bytes: The big-endian encoding of the length of the block in oids
*/

#define GZOCHID_DATA_PROTOCOL_OIDS_RESPONSE 0x50

/* Contains the serialized object data stored at a particular oid. See 
   `gzochid_data_protocol_object_response_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_OBJECT_RESPONSE 0x51

/* Contains the object id bound to a particular name. See 
   `gzochid_data_protocol_binding_response_write' below for format details. */

#define GZOCHID_DATA_PROTOCOL_BINDING_RESPONSE 0x52

/* Contains the object id bound to a particular name. See 
   `gzochid_data_protocol_binding_key_response_write' below for format 
   details. */

#define GZOCHID_DATA_PROTOCOL_NEXT_BINDING_RESPONSE 0x53

/*
  Requests the client release, by sending an object release request, all point
  locks (read and write) on a particular object id. Format:

  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Hexadecimal string representation of the target oid
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_OBJECT_RELEASE 0x60

/*
  Requests the client release, by sending a binding release request, all point
  locks (read and write) on a particular binding. Format:

  `NULL'-terminated string: Name of the requesting game application
  `NULL'-terminated string: Name of the requested binding  
*/

#define GZOCHID_DATA_PROTOCOL_REQUEST_BINDING_RELEASE 0x61

/* A response to a request to reserve a block of oids. */

struct _gzochid_data_reserve_oids_response
{
  char *app; /* The requesting application name. */
  gzochid_data_oids_block block; /* The reserved block of oids. */
};

typedef struct _gzochid_data_reserve_oids_response
gzochid_data_reserve_oids_response;

/* A response to a request for the object data at a specified oid. */

struct _gzochid_data_object_response
{
  char *app; /* The requesting application name. */
  gboolean success; /* Whether the request was successful. */

  union
  {
    /* If the request was successful, the target object data, or `NULL' if no 
       such object exists. */

    GBytes *data; 

    /* If the request was unsuccessful, the amount of time to wait before 
       retrying. */

    struct timeval timeout; 
  };
};

typedef struct _gzochid_data_object_response gzochid_data_object_response;

/* A response to a request for the oid and a specified binding. */

struct _gzochid_data_binding_response
{
  char *app; /* The requesting application name. */
  gboolean success; /* Whether the request was successful. */
  gboolean present; /* Whether the binding exists. */

  union
  {
    /* If the request was successful and the binding exists, the target oid. */

    mpz_t oid; 

    /* If the request was unsuccessful, the amount of time to wait before 
       retrying. */

    struct timeval timeout;
  };
};

typedef struct _gzochid_data_binding_response gzochid_data_binding_response;

/* A response to a request for the name of the next sequential binding. */

struct _gzochid_data_binding_key_response
{
  char *app; /* The requesting application name. */
  gboolean success; /* Whether the request was successful. */

  union
  {
    /* If the request was successful, the next sequential binding name, or 
       `NULL' if no such binding exists. */

    char *name;
    
    /* If the request was unsuccessful, the amount of time to wait before 
       retrying. */
    
    struct timeval timeout;
  };
};

typedef struct _gzochid_data_binding_key_response
gzochid_data_binding_key_response;

/* A change to an object to be made as part of a changeset. */

struct _gzochid_data_object_change
{
  gboolean delete; /* Whether the object should be deleted. */
  mpz_t oid; /* The targer oid. */
  GBytes *data; /* The new data for the object, if not deleting, else `NULL'. */
};

typedef struct _gzochid_data_object_change gzochid_data_object_change;

/* A change to a binding to be made as part ofa changeset. */

struct _gzochid_data_binding_change
{
  gboolean delete; /* Whether the binding should be deleted. */
  char *name; /* The target binding name. */
  mpz_t oid; /* The new value for the binding, if not deleting. */
};

typedef struct _gzochid_data_binding_change gzochid_data_binding_change;

/* A set of changes to objects and bindings to be persisted. */

struct _gzochid_data_changeset
{
  char *app; /* The requesting application name. */

  /* The array of `gzochid_data_object_change' structs. */
  
  GArray *object_changes; 

  /* The array of `gzochid_data_binding_change' structs. */

  GArray *binding_changes;
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
  Construct and return a new object response with the specified application 
  name, success flag, and object bytes; this last argument may be `NULL' to
  indicate the absence of data for the requested oid.

  The pointer returned by this function should be freed with 
  `gzochid_data_object_response_free'.
*/

gzochid_data_object_response *
gzochid_data_object_response_new (char *, gboolean, GBytes *);

/* Free the specified object response. */

void gzochid_data_object_response_free (gzochid_data_object_response *);

/* 
  Serialize the specified object response to the specified byte array. Format:

  `NULL'-terminated string: Name of the requesting game application
  1 byte: 0x01 indicating success (and that object data follows), 
    0x00 indicating failure
  2 bytes: The big-endian encoding of the length of the serialized object
    Two zeros indicates the absence of an object; else the object data follows
*/

void gzochid_data_protocol_object_response_write
(gzochid_data_object_response *, GByteArray *);

/*
  Deserialize and return an object response from the specified byte buffer,
  or return `NULL' if the buffer does not contain a correctly-serialized 
  response. 

  The pointer returned by this function should be freed with 
  `gzochid_data_object_response_free'.
*/

gzochid_data_object_response *gzochid_data_protocol_object_response_read
(GBytes *);

/*
  Construct and return a new successful binding response with the specified 
  application name and oid.

  The pointer returned by this function should be freed with 
  `gzochid_data_binding_response_free'.
*/

gzochid_data_binding_response *gzochid_data_binding_response_oid_new
(char *, mpz_t);

/*
  Construct and return a new binding response with the specified application 
  name and success flag; if the flag value is `TRUE' the response indicates the
  absence of an oid for the requested binding.

  The pointer returned by this function should be freed with 
  `gzochid_data_binding_key_response_free'.
*/

gzochid_data_binding_response *gzochid_data_binding_response_new
(char *, gboolean);

/* Free the specified binding response. */

void gzochid_data_binding_response_free (gzochid_data_binding_response *);

/* 
  Serialize the specified binding response to the specified byte array. Format:

  `NULL'-terminated string: Name of the requesting game application
  1 byte: 0x01 indicating success (and that binding data follows), 
    0x00 indicating failure
  `NULL'-terminated string: Hexadecimal string representation of the target oid
    or 1 `NULL' byte to indicate an absent binding
*/

void gzochid_data_protocol_binding_response_write
(gzochid_data_binding_response *, GByteArray *);

/*
  Deserialize and return a binding response from the specified byte buffer, or 
  return `NULL' if the buffer does not contain a correctly-serialized response. 

  The pointer returned by this function should be freed with 
  `gzochid_data_binding_response_free'.
*/

gzochid_data_binding_response *
gzochid_data_protocol_binding_response_read (GBytes *);

/*
  Construct and return a new binding key response with the specified application
  name, success flag, and next binding key; this last argument may be `NULL' to
  indicate the absence of a next binding.

  The pointer returned by this function should be freed with 
  `gzochid_data_binding_key_resonse_free'.
*/

gzochid_data_binding_key_response *gzochid_data_binding_key_response_new
(char *, gboolean, char *);

/* Free the specified binding key response. */

void gzochid_data_binding_key_response_free
(gzochid_data_binding_key_response *);

/* 
  Serialize the specified binding key response to the specified byte array. 
  Format:

  `NULL'-terminated string: Name of the requesting game application
  1 byte: 0x01 indicating success (and that binding data follows), 
    0x00 indicating failure
  `NULL'-terminated string: Name of the next binding
    or 1 `NULL' byte to indicate the absence of a next binding
*/

void gzochid_data_protocol_binding_key_response_write
(gzochid_data_binding_key_response *, GByteArray *);

/*
  Deserialize and return a binding key response from the specified byte buffer,
  or return `NULL' if the buffer does not contain a correctly-serialized 
  response. 

  The pointer returned by this function should be freed with 
  `gzochid_data_binding_key_response_free'.
*/

gzochid_data_binding_key_response *
gzochid_data_protocol_binding_key_response_read (GBytes *);

/*
  Construct and return a new changeset with the specified application name, and
  object and binding change arrays, which should be arrays with elements of type
  `gzochid_data_object_change' and `gzochid_data_binding_change', 
  respectively.

  The pointer returned by this function should be freed with 
  `gzochid_data_changeset_free'.
*/

gzochid_data_changeset *gzochid_data_changeset_new (char *, GArray *, GArray *);

/* Free the specified changeset. */

void gzochid_data_changeset_free (gzochid_data_changeset *);

/*
  Serialize the specified changeset object to the specified byte array. Format:

  `NULL'-terminated string: Name of the requesting game application
  2 bytes: The big-endian encoding of the number of object changes
  [object changes; see below]
  2 bytes: The big-endian encoding of the number of binding changes
  [binding changes; see below]

  The format of each object change is:

  `NULL'-terminated string: Hexadecimal string representation of the target oid
  2 bytes: The big-endian encoding of the length of the serialized object
    Two zeros indicates a deletion; else the object data follows

  The format of each binding change is:

  `NULL'-terminated string: The target binding name
  `NULL'-terminated string: Hexadecimal string representation of the target oid
    or 1 `NULL' byte to indicate a deletion
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
