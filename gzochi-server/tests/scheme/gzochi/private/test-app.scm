;; gzochi/private/test-app.scm: Scheme unit tests for private app support module
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

(library (gzochi private test-app)
  (export)
  (import (only (guile) *unspecified*)
	  (gzochi app) 
	  (gzochi private app)
	  (gzochi srfi-64-support)
	  (rnrs base) 
	  (rnrs conditions)
	  (rnrs exceptions)
	  (rnrs hashtables) 
	  (srfi :64))

(test-runner-current (gzochi:test-runner))

(define (false-handler) #f)
(define (unspecified-handler) *unspecified*)

(define ready-counter 0)
(define (clear-ready-counter!) (set! ready-counter 0))
(define (ready-handler properties) (set! ready-counter (+ ready-counter 1)))

(test-begin "gzochi:execute-logged-in")

(test-group "#f"
  (test-eqv #f (gzochi:execute-logged-in (g:@ false-handler) 'session)))
(test-group "unspecified"
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:execute-logged-in (g:@ unspecified-handler) 'session)
		 #f)))

(test-end "gzochi:execute-logged-in")

(test-begin "gzochi:execute-ready")

(test-group "execute"
  (clear-ready-counter!)
  (gzochi:execute-ready (g:@ ready-handler) (make-eq-hashtable))
  (test-eqv 1 ready-counter))

(test-end "gzochi:execute-ready")

)
