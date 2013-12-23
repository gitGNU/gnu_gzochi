;; gzochi.scm: The gzochi composite library
;; Copyright (C) 2013 Julian Graham
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
	  &gzochi:transaction-aborted gzochi:make-transaction-aborted-condition
	  gzochi:transaction-aborted-condition? &gzochi:transaction-retry
	  gzochi:make-transaction-retry-condition 
	  gzochi:transaction-retry-condition? &gzochi:transaction-timeout 
	  gzochi:make-transaction-timeout-condition
	  gzochi:transaction-timeout-condition? &gzochi:object-removed
	  gzochi:make-object-removed-condition gzochi:object-removed-condition?

          ;; (gzochi data)

	  gzochi:make-managed-record-type-descriptor
	  gzochi:managed-record-accessor gzochi:managed-record-mutator
	  gzochi:managed-record-constructor gzochi:managed-record-predicate
	  gzochi:managed-record? gzochi:managed-record-rtd 
	  gzochi:managed-record-type-name gzochi:managed-record-type-parent 
	  gzochi:managed-record-type-uid gzochi:define-managed-record-type 
	  gzochi:managed-record-type-descriptor 
	  gzochi:managed-record-constructor-descriptor gzochi:get-binding 
	  gzochi:set-binding! gzochi:remove-binding! gzochi:remove-object!

	  gzochi:make-managed-serializable gzochi:managed-serializable?
	  gzochi:managed-serializable-value
	  gzochi:managed-serializable-value-set!

	  gzochi:make-managed-vector gzochi:managed-vector?
	  gzochi:managed-vector-ref gzochi:managed-vector-set!
          gzochi:managed-vector-length gzochi:managed-vector->list

	  gzochi:make-managed-sequence gzochi:managed-sequence?
          gzochi:managed-sequence->list gzochi:managed-sequence-add!
          gzochi:managed-sequence-delete! gzochi:managed-sequence-delete-at! 
	  gzochi:managed-sequence-fold-left gzochi:managed-sequence-fold-right 
	  gzochi:managed-sequence-insert! gzochi:managed-sequence-ref 
	  gzochi:managed-sequence-size

          gzochi:make-managed-hashtable gzochi:managed-hashtable?
          gzochi:managed-hashtable-size gzochi:managed-hashtable-ref
          gzochi:managed-hashtable-set! gzochi:managed-hashtable-delete!
          gzochi:managed-hashtable-contains? gzochi:managed-hashtable-update!
          gzochi:managed-hashtable-clear! gzochi:managed-hashtable-keys
          gzochi:managed-hashtable-entries
	  gzochi:managed-hashtable-hash-function
          gzochi:managed-hashtable-equivalence-function

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
