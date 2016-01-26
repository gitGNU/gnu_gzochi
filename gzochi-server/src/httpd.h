/* httpd.h: Prototypes and declarations for httpd.c
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

#ifndef GZOCHID_HTTPD_H
#define GZOCHID_HTTPD_H

#include <glib.h>
#include <sys/socket.h>

#include "context.h"

/* The httpd service context. */

typedef struct _gzochid_httpd_context gzochid_httpd_context;

/* Construct and return a new httpd service context. This pointer should be 
   freed via `gzochid_httpd_context_free' when no longer needed. */

gzochid_httpd_context *gzochid_httpd_context_new (void);

/* Releases the resources associated with the specified httpd service 
   context. */

void gzochid_httpd_context_free (gzochid_httpd_context *);

/* Initialize the specified httpd service context, binding it to the specified
   parent content and starting it listening on the specified port. */

void gzochid_httpd_context_init 
(gzochid_httpd_context *, gzochid_context *, int);

/* The following functions and typedefs form a minimal web framework that allows
   intermediate ("continuation") and terminal handlers to be mapped to regular
   expressions mapping fragments of the path component in an URL. */

/* The HTTP response sink represents the output channel for an HTTP request
   handling control flow. It can be used (via `gzochid_httpd_write_response' 
   below) to send data to a connected client. */

typedef struct _gzochid_httpd_response_sink gzochid_httpd_response_sink;

/* Returned by the `continuation' functions below; represents an intermediate 
   state of request path matching, to which multiple patterns can be attached to
   describe a match of the entire request path. */ 

typedef struct _gzochid_httpd_partial gzochid_httpd_partial;

/* The function pointer typedef for a terminal handler, invoked as the final
   step in a chain of expression matching and continuation handlers. This 
   function will be called with the match structure for the matched path 
   fragment, as well as a response sink that can be used to write a response, 
   and opaque pointers to a request context (produced by an earlier 
   continuation handler) and any user data bound to the handler during
   registration. */

typedef void (*gzochid_httpd_terminal)
(const GMatchInfo *, gzochid_httpd_response_sink *, gpointer, gpointer);

/* The function pointer typedef for a continuation handler, invoked to bind
   path parameters common to multiple downstream request patterns. This function
   will be called with the match structure for the matched path fragment, as
   well as a response sink that can be used to write a response, and opaque
   pointers to a request context and any user data bound to the handler during
   registration. 

   The handler should return a pointer to contextual data intended for use by
   downstream continuation handlers or terminal handlers; this may be a pointer
   to a newly-allocated structure, the passed-in request context pointer, or
   `NULL'. */

typedef gpointer (*gzochid_httpd_continuation)
(const GMatchInfo *, gzochid_httpd_response_sink *, gpointer, gpointer);

/* Adds a new terminal handler that will be called for paths matching the 
   specified pattern, which will be rooted at the beginning of the request 
   path. */

void gzochid_httpd_add_terminal
(gzochid_httpd_context *, const char *, gzochid_httpd_terminal, gpointer);

/* Adds a new terminal handler that will be called for paths matching the
   specified pattern, which will be rooted at the part of the path following the
   specified partial. */

void gzochid_httpd_append_terminal
(gzochid_httpd_partial *, const char *, gzochid_httpd_terminal, gpointer);

/* Adds a new continuation handler that will be called for paths matching the
   specified pattern, which will be rooted at the beginning of the request
   path. Returns a partial that can be used to attach matchers for downstream
   terminals or continuations. */

gzochid_httpd_partial *gzochid_httpd_add_continuation
(gzochid_httpd_context *, const char *, gzochid_httpd_continuation, gpointer);

/* Adds a new continuation handler that will be called for paths matching the
   specified pattern, which will be rooted at the part of the path following the
   specified partial. Returns a partial that can be used to attach matchers for
   downstream terminals or continuations. */

gzochid_httpd_partial *gzochid_httpd_append_continuation
(gzochid_httpd_partial *, const char *, gzochid_httpd_continuation, gpointer);

/* Writes the specified bytes to the sink, setting the specified HTTP response 
   code. Note that the first write in a request-handling flow will terminate the
   flow, even if the write occurs within a continuation handler. */

void gzochid_httpd_write_response
(gzochid_httpd_response_sink *, int, char *, size_t);

/* Private httpd service context API, visible for testing only. */

/* Copies the actual socket address of the specified httpd service context to 
   the specified `sockaddr' struct. Semantics following the `getsockname' 
   function in `<sys/socket.h>' - the length pointer indicates the available 
   buffer in the address struct. It is updated to reflect the actual size of the
   address, which may be truncated as necessary. */

void _gzochid_httpd_context_getsockname
(gzochid_httpd_context *, struct sockaddr *, socklen_t *);

#endif /* GZOCHID_HTTPD_H */
