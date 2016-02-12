;; gzochi/tx.scm: Public exports for gzochi external transaction API
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

;; This library forms the Scheme side of the external transaction API. It
;; provides the `gzochi:transaction-participant' record type, which implementors
;; must use to register the prepare / commit / rollback hooks for their external
;; transaction.

(library (gzochi tx)
  (export gzochi:abort-transaction
	  gzochi:join-transaction

	  gzochi:make-transaction-participant
	  gzochi:transaction-participant?
	  gzochi:transaction-participant-prepare
	  gzochi:transaction-participant-commit
	  gzochi:transaction-participant-rollback)

  (import (only (guile) define* thunk?)
	  (rnrs base)
	  (rnrs conditions)
	  (rnrs exceptions)
	  (rnrs records syntactic))

  ;; The rnrs record type definition for external transaction participants.
  ;; A `gzochi:transaction-participant' is constructed with three callback
  ;; procedures for the prepare, commit, and rollback events. Each procedure
  ;; must be a thunk.

  (define-record-type (gzochi:transaction-participant
		       gzochi:make-transaction-participant
		       gzochi:transaction-participant?)
    (fields prepare commit rollback)
    (protocol (lambda (p)
		(lambda (prepare commit rollback)
		  (or (thunk? prepare)
		      (assertion-violation 'gzochi:make-transaction-participant
					   "Prepare must be a thunk." prepare))
		  (or (thunk? commit)
		      (assertion-violation 'gzochi:make-transaction-participant
					   "Commit must be a thunk." commit))
		  (or (thunk? rollback)
		      (assertion-violation
		       'gzochi:make-transaction-participant
		       "Rollback must be a thunk." rollback))

		  (p prepare commit rollback)))))
  
  ;; `primitive-join-transaction' and `primitive-abort-transaction' are replaced
  ;; at boot time with concrete implementations in api/tx.c.
	      
  (define primitive-join-transaction #f)
  (define primitive-abort-transaction #f)

  ;; Wrapper around `primitive-join-transaction' that asserts the participant 
  ;; is valid as per `gzochi:transaction-participant?'.

  (define (gzochi:join-transaction participant)
    (or (gzochi:transaction-participant? participant)
	(assertion-violation
	 'gzochi:join-transaction "Expected transaction participant."
	 participant))
    
    (primitive-join-transaction participant))

  ;; Wrapper around `primitive-abort-transaction' that asserts the participant 
  ;; is valid as per `gzochi:transaction-participant?'.

  (define* (gzochi:abort-transaction participant #:optional (retryable? #t))
    (or (gzochi:transaction-participant? participant)
	(assertion-violation
	 'gzochi:join-transaction "Expected transaction participant."
	 participant))

    (primitive-abort-transaction participant retryable?))
)
