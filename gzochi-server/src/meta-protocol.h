/* meta-protocol.h: Opcode definitions for the meta protocol.
 * Copyright (C) 2017 Julian Graham
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

#ifndef GZOCHID_META_PROTOCOL_H
#define GZOCHID_META_PROTOCOL_H

/* The following byte single-constants are "opcodes" for the meta protocol, and
   correspond to the different messages that can be exchanged between the client
   (gzochi application server) and the server (gzochi meta server) participating
   in the protocol. The format of the message payload that follows each opcode 
   is opcode-specific and is described below. */

/* The following opcodes are for messages sent from the client to the server. */

/*
  Login to the meta server and pass some configuration data from the client.
  Format:
  
  1 byte: Meta protocol version. (0x02)
  `NULL'-terminated string: gzochid admin server base URL
    or 1 `NULL' byte if the client is not running an admin web console
*/

#define GZOCHID_META_PROTOCOL_LOGIN 0x10

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

/*
  Notify the meta server that a session has connected to an application server
  node.

  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
*/

#define GZOCHID_SESSION_PROTOCOL_SESSION_CONNECTED 0x60

/*
  Notify the meta server that a session has disconnected from an application 
  server node.

  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
*/

#define GZOCHID_SESSION_PROTOCOL_SESSION_DISCONNECTED 0x61

/*
  Relay a disconnect signal via the meta server for the target session.

  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
*/

#define GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_FROM 0x62

/*
  Relay a message via the meta server for the target session.

  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
  2 bytes: The big-endian encoding of the length of the key; the key bytes 
    follow
*/

#define GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_FROM 0x64

/* The following opcodes are for messages sent from the server to the client. */

/*
  1 byte: Meta protocol version. (0x02)
  `NULL'-terminated string: gzochi-metad admin server base URL
    or 1 `NULL' byte if the meta server is not running an admin web console
 */

#define GZOCHID_META_PROTOCOL_LOGIN_RESPONSE 0x11

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

/* 
  Directs the target server to disconnect the specified client session.
   
  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
*/

#define GZOCHID_SESSION_PROTOCOL_RELAY_DISCONNECT_TO 0x63

/*
  Relays a message to the session connected to the target server.
  
  `NULL'-terminated string: Name of the game application that owns the session
  8 bytes: The big-endian encoding of the target session oid
  2 bytes: The big-endian encoding of the length of the key; the key bytes 
    follow
*/

#define GZOCHID_SESSION_PROTOCOL_RELAY_MESSAGE_TO 0x65

#endif /* GZOCHID_META_PROTOCOL_H */
