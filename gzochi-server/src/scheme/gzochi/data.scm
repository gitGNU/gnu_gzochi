;; gzochi/data.scm: Public exports for gzochi data API
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

(library (gzochi data)

  (export gzochi:make-managed-record-type-descriptor
	  gzochi:managed-record-accessor
	  gzochi:managed-record-mutator
	  gzochi:managed-record-constructor
	  gzochi:managed-record-predicate

	  gzochi:managed-record?

          gzochi:define-managed-record-type
	  gzochi:managed-record-type-descriptor
	  gzochi:managed-record-constructor-descriptor

	  gzochi:car
	  gzochi:cdr
	  gzochi:make-managed-pair
	  gzochi:managed-pair?

	  gzochi:make-managed-vector
	  gzochi:managed-vector?
	  gzochi:managed-vector-ref
	  gzochi:managed-vector-set!
	  gzochi:managed-vector->list

	  gzochi:make-managed-hashtable
	  gzochi:managed-hashtable?
	  gzochi:managed-hashtable-set!
	  gzochi:managed-hashtable-ref

	  gzochi:serialize
	  gzochi:deserialize

	  gzochi:create-reference
	  gzochi:dereference
	  gzochi:get-binding
	  gzochi:set-binding!
	  gzochi:remove-binding!)

  (import (gzochi io)
	  (gzochi private data)
	  (rnrs)
	  (only (srfi :1) split-at)
	  (srfi :8))

  (define (gzochi:car pair) (gzochi:managed-pair-car pair))
  (define (gzochi:cdr pair) (or (gzochi:managed-pair-cdr pair) '()))
  (define (gzochi:cons x y)
    (or (and (gzochi:managed-record? x)
	     (or (gzochi:managed-record? y) (null? y)))
	(gzochi:make-managed-pair x (if (null? y) #f y))))

  (define (gzochi:managed-list head . rest)
    (gzochi:cons 
     head (if (null? rest) rest (gzochi:managed-list (car rest) (cdr rest)))))

  (gzochi:define-managed-record-type 
    (gzochi:managed-pair gzochi:make-managed-pair gzochi:managed-pair?)

    (fields (immutable ca gzochi:managed-pair-car)
	    (immutable cd gzochi:managed-pair-cdr))
    (nongenerative gzochi:managed-pair)
    (sealed #t))

  (define (serialize-managed-vector port vec)
    (gzochi:write-integer port (vector-length vec))
    (vector-for-each
     (lambda (ref) (gzochi:serialize-managed-reference port ref)) vec))
  
  (define (deserialize-managed-vector port)
    (let* ((n (gzochi:read-integer port)))
      (let loop ((i n) (refs '()))
	(if (zero? i)
	    (list->vector (reverse refs))
	    (loop (- i 1) 
		  (cons (gzochi:deserialize-managed-reference port) refs))))))
  
  (define (gzochi:managed-vector-ref vec i)
    (gzochi:dereference (vector-ref (gzochi:managed-vector-vector vec) i)))

  (define (gzochi:managed-vector->list vec)
    (map gzochi:dereference (vector->list (gzochi:managed-vector-vector vec))))

  (define (gzochi:managed-vector-set! vec i obj)
    (vector-set! (gzochi:managed-vector-vector vec) i 
		 (gzochi:create-reference obj)))

  (gzochi:define-managed-record-type
   (gzochi:managed-vector gzochi:make-managed-vector gzochi:managed-vector?)

    (fields (immutable vector 
		       (serialization (gzochi:make-serialization 
				       serialize-managed-vector
				       deserialize-managed-vector))))

    (nongenerative gzochi:managed-vector)
    (protocol (lambda (n)
		(lambda args
		  (let ((p (n)))
		    (p (apply vector (map gzochi:create-reference args)))))))
    (sealed #t))

  (define (gzochi:managed-hashtable-set! ht key value) (if #f #f))

  (define (gzochi:managed-hashtable-ref ht key) (if #f #f))

  (define (gzochi:write-noop port obj) (if #f #f)) 
  (define (gzochi:read-noop port) #f)

  (gzochi:define-managed-record-type 
    (gzochi:managed-hashtable 
     gzochi:make-managed-hashtable 
     gzochi:managed-hashtable?)

    (fields (immutable hashtable (serialization 
				  (gzochi:make-serialization 
				   gzochi:write-noop gzochi:read-noop))))
    (nongenerative gzochi:managed-hashtable)
    (protocol (lambda (n)
		(lambda ()
		  (let ((p (n)))
		    (p (make-eq-hashtable))))))		  
    (sealed #t))

  (define (gzochi:serialize port obj)
    (or (gzochi:managed-record? obj)
	(raise (condition 
		(make-assertion-violation)
		(make-message-condition 
		 "Only managed records may be auto-serialized."))))

    (gzochi:serialize-managed-reference port (gzochi:create-reference obj)))

  (define (gzochi:deserialize port)
    (gzochi:dereference (gzochi:deserialize-managed-reference port)))
)
