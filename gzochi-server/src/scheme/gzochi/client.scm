;; gzochi/client.scm: Public exports for gzochi client session API
;; Copyright (C) 2011 Julian Graham
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

(library (gzochi client)
  (export gzochi:client-session?
	  gzochi:client-session-name

	  gzochi:client-session-listener
	  gzochi:make-client-session-listener
	  gzochi:client-session-listener?
	  gzochi:client-session-listener-received-message
	  gzochi:client-session-listener-disconnected

	  gzochi:send-message
	  gzochi:disconnect)

  (import (rnrs)
	  (gzochi app)
	  (gzochi data)
	  (gzochi private client)
	  (gzochi private data))
  
  (gzochi:define-managed-record-type 
   (gzochi:client-session-listener
    gzochi:make-client-session-listener
    gzochi:client-session-listener?)
    
   (fields received-message disconnected)
   (nongenerative gzochi:client-session-listener)
   (sealed #t))
)
