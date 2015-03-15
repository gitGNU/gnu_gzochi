;; gzochi/test-tx.scm: Scheme unit tests for gzochi external transaction API
;; Copyright (C) 2015 Julian Graham
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

(import (gzochi tx))
(import (rnrs))
(import (srfi :64))

(test-begin "gzochi:transaction-participant")

(test-group "constructor"
  (test-assert (gzochi:transaction-participant? 
		(gzochi:make-transaction-participant 
		 (lambda () *unspecified*)
		 (lambda () *unspecified*)
		 (lambda () *unspecified*))))
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		  (gzochi:make-transaction-participant 
		  'not 'procedure 'expressions)
		  #f)))
		  
(test-end "gzochi:transaction-participant")

(define (make-test-transaction-participant)
  (gzochi:make-transaction-participant 
   (lambda () *unspecified*)
   (lambda () *unspecified*)
   (lambda () *unspecified*)))

(test-begin "gzochi:join-transaction")

(define join-participant #f)
(module-set! (resolve-module '(gzochi tx)) 
	     'primitive-join-transaction 
	     (lambda (participant) (set! join-participant participant)))

(test-group "join"
  (let ((participant (make-test-transaction-participant)))
    (gzochi:join-transaction participant)
    (test-eq participant join-participant))
	      
  (test-assert (guard (ex ((assertion-violation? ex) #t))
	         (gzochi:join-transaction 'not-a-transaction-participant)
		 #f)))

(test-end "gzochi:join-transaction")

(test-begin "gzochi:abort-transaction")

(define abort-participant #f)
(define abort-retryable? *unspecified*)

(module-set! (resolve-module '(gzochi tx)) 
	     'primitive-abort-transaction 
	     (lambda* (participant #:optional retryable?) 
	       (set! abort-participant participant)
	       (set! abort-retryable? retryable?)))

(test-group "abort"
  (let ((participant (make-test-transaction-participant)))
    (gzochi:abort-transaction participant)
    (test-eq participant abort-participant)
    (test-eqv #t abort-retryable?))

  (let ((participant (make-test-transaction-participant)))
    (gzochi:abort-transaction participant #f)
    (test-eq participant abort-participant)
    (test-eqv #f abort-retryable?))

  (test-assert (guard (ex ((assertion-violation? ex) #t))
	         (gzochi:abort-transaction 'not-a-transaction-participant)
		 #f)))

(test-end "gzochi:abort-transaction")
