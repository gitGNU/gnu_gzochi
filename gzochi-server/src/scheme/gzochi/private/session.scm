;; gzochi/private/session.scm: Private infrastructure for gzochi client API 
;; Copyright (C) 2016 Julian Graham
;;
;; gzochi is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library (gzochi private session)
  (export gzochi:make-client-session
	  gzochi:client-session?
	  gzochi:client-session-name
	  gzochi:client-session-oid

	  gzochi:client-session-listener
	  gzochi:make-client-session-listener
	  gzochi:client-session-listener?
	  gzochi:client-session-listener-received-message
	  gzochi:client-session-listener-disconnected

	  gzochi:send-message
	  gzochi:disconnect)

  (import (gzochi io)
	  (gzochi private data)
	  (rnrs base)
	  (rnrs bytevectors)
	  (rnrs conditions)
	  (rnrs exceptions))

  (define primitive-send-message #f)
  (define primitive-disconnect #f)

  (gzochi:define-managed-record-type 
   (gzochi:client-session gzochi:make-client-session gzochi:client-session?)

   (fields (immutable name (serialization gzochi:string-serialization))
	   (immutable oid (serialization gzochi:integer-serialization))))

  (gzochi:define-managed-record-type 
   (gzochi:client-session-listener
    gzochi:make-client-session-listener
    gzochi:client-session-listener?)
    
   (fields received-message disconnected)
   (sealed #t))

  (define (gzochi:send-message session msg)
    (or (gzochi:client-session? session)
	(assertion-violation
	 'gzochi:send-message "Expected gzochi:client-session." session))

    (or (bytevector? msg)
	(assertion-violation 'gzochi:send-message "Expected bytevector." msg))
    
    (primitive-send-message session msg))

  (define (gzochi:disconnect session)
    (or (gzochi:client-session? session)
	(assertion-violation
	 'gzochi:disconnect "Expected gzochi:client-session." session))

    (primitive-disconnect session))
)
