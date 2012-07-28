;; gzochi.scm: The gzochi composite library
;; Copyright (C) 2012 Julian Graham
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

(library (gzochi)
  (export ;; (gzochi app)

          gzochi:make-callback gzochi:callback? gzochi:callback-module
	  gzochi:callback-procedure gzochi:callback-data 
	  %gzochi:application-root
   
          ;; (gzochi channel)

	  gzochi:create-channel gzochi:get-channel gzochi:channel?
	  gzochi:channel-name gzochi:channel-delivery gzochi:channel-sessions
	  gzochi:join-channel gzochi:leave-channel gzochi:send-channel-message
	  gzochi:close-channel
	  
          ;; (gzochi client)

	  gzochi:client-session? gzochi:client-session-name 
	  gzochi:client-session-listener gzochi:make-client-session-listener
	  gzochi:client-session-listener-received-message
	  gzochi:client-session-listener-disconnected gzochi:send-message
	  gzochi:disconnect

          ;; (gzochi conditions)

	  &gzochi:name-exists gzochi:make-name-exists-condition
	  gzochi:name-exists-condition? &gzochi:name-not-bound
	  gzochi:make-name-not-bound-condition gzochi:name-not-bound-condition?

          ;; (gzochi data)

	  gzochi:make-managed-record-type-descriptor
	  gzochi:managed-record-accessor gzochi:managed-record-mutator
	  gzochi:managed-record-constructor gzochi:managed-record-predicate
	  gzochi:managed-record? gzochi:define-managed-record-type
	  gzochi:managed-record-type-descriptor 
	  gzochi:managed-record-constructor-descriptor gzochi:get-binding 
	  gzochi:set-binding! gzochi:remove-binding!

          ;; (gzochi io)

	  gzochi:read-integer gzochi:read-boolean gzochi:read-string
	  gzochi:read-symbol gzochi:read-bytevector gzochi:write-integer 
	  gzochi:write-boolean gzochi:write-string gzochi:write-symbol 
	  gzochi:write-bytevector gzochi:integer-serialization
	  gzochi:boolean-serialization gzochi:string-serialization
	  gzochi:symbol-serialization gzochi:bytevector-serialization 
	  gzochi:make-uniform-list-serialization gzochi:serialization 
	  gzochi:serialization? gzochi:make-serialization
	  gzochi:serialization-deserializer gzochi:serialization-serializer

          ;; (gzochi log)

	  gzochi:log gzochi:log-err gzochi:log-warning gzochi:log-notice
	  gzochi:log-info gzochi:log-debug
	  
          ;; (gzochi task)

	  gzochi:make-task gzochi:task? gzochi:task-module 
	  gzochi:task-procedure gzochi:task-data gzochi:schedule-task)

  (import (gzochi app)
	  (gzochi channel)
	  (gzochi client)
	  (gzochi conditions)
	  (gzochi data)
	  (gzochi io)
	  (gzochi log)
	  (gzochi task))
)
