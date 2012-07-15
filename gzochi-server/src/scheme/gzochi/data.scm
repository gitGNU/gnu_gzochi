;; gzochi/data.scm: Public exports for gzochi data API
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

(library (gzochi data)

  (export gzochi:make-managed-record-type-descriptor
	  gzochi:make-managed-record-constructor-descriptor
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
	  gzochi:cons

	  gzochi:managed-list
	  gzochi:managed-list->list
	  gzochi:list->managed-list

	  gzochi:make-managed-serializable
	  gzochi:managed-serializable?
	  gzochi:managed-serializable-value
	  gzochi:managed-serializable-value-set!
	  
	  gzochi:make-managed-vector
	  gzochi:managed-vector
	  gzochi:managed-vector?
	  gzochi:managed-vector-ref
	  gzochi:managed-vector-set!
	  gzochi:managed-vector-length
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
	  gzochi:remove-binding!
	  gzochi:remove-object!)

  (import (gzochi io)
	  (gzochi private data)
	  (rnrs)
	  (only (srfi :1) split-at)
	  (srfi :8))

  (gzochi:define-managed-record-type 
    (gzochi:managed-pair gzochi:make-managed-pair gzochi:managed-pair?)

    (fields (immutable ca gzochi:managed-pair-car)
	    (immutable cd gzochi:managed-pair-cdr))
    (nongenerative gzochi:managed-pair)
    (sealed #t))

  (define gzochi:car gzochi:managed-pair-car)
  (define gzochi:cdr gzochi:managed-pair-cdr)
  (define (gzochi:cons x y)
    (or (and (or (gzochi:managed-record? x) (not x))
	     (or (gzochi:managed-record? y) (not y) (null? y)))
	(raise (make-assertion-violation)))
    (gzochi:make-managed-pair x (if (null? y) #f y)))

  (define (gzochi:managed-list h . t)
    (gzochi:cons h (if (null? t) t (gzochi:managed-list (car t) (cdr t)))))

  (define (gzochi:managed-list->list l)
    (if l (cons (gzochi:car l) (gzochi:managed-list->list (gzochi:cdr l))) '()))
  
  (define (gzochi:list->managed-list l)
    (and (not (null? l))
	 (gzochi:cons (car l) (gzochi:list->managed-list (cdr l)))))
  
  (define (serialize-serialization-with-value port pair)
    (let* ((serializer-callback (car pair))
	   (deserializer-callback (cadr pair))
	   (serializer-callback-procedure
	    (gzochi:resolve-procedure 
	     (gzochi:callback-procedure serializer-callback) 
	     (gzochi:callback-module serializer-callback))))
      (gzochi:serialize-managed-record port serializer-callback)
      (gzochi:serialize-managed-record port deserializer-callback)
      (serializer-callback-procedure port (caddr pair))))

  (define (deserialize-serialization-with-value port)
    (let* ((serializer-callback (gzochi:deserialize-managed-record port))
	   (deserializer-callback (gzochi:deserialize-managed-record port))
	   (deserializer-callback-procedure
	    (gzochi:resolve-procedure
	     (gzochi:callback-procedure deserializer-callback) 
	     (gzochi:callback-module deserializer-callback))))
      (list serializer-callback 
	    deserializer-callback 
	    (deserializer-callback-procedure port))))

  (define serialization-with-value-serialization
    (gzochi:make-serialization 
     serialize-serialization-with-value deserialize-serialization-with-value))

  (gzochi:define-managed-record-type 
   (gzochi:managed-serializable 
    gzochi:make-managed-serializable 
    gzochi:managed-serializable?)
   (fields (mutable callback-with-value 
		    (serialization serialization-with-value-serialization)))
   (nongenerative gzochi:managed-serializable)
   (protocol (lambda (n)
	       (lambda (value serializer-callback deserializer-callback)
		 (or (gzochi:callback? serializer-callback)
		     (raise (condition 
			     (make-assertion-violation)
			     (make-irritants-condition serializer-callback))))
		 (or (gzochi:callback? deserializer-callback)
		     (raise (condition 
			     (make-assertion-violation)
			     (make-irritants-condition deserializer-callback))))

		 (let ((p (n)))
		   (p (list serializer-callback 
			    deserializer-callback 
			    value)))))))
 
  (define (gzochi:managed-serializable-value obj)
    (caddr (gzochi:managed-serializable-callback-with-value obj)))
  
  (define (gzochi:managed-serializable-value-set! obj val)
    (let ((l (gzochi:managed-serializable-callback-with-value obj)))
      (gzochi:managed-serializable-callback-with-value-set!
       obj (list (car l) (cadr l) val))
      (if #f #f)))

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
  
  (gzochi:define-managed-record-type
   (managed-vector gzochi:make-managed-vector gzochi:managed-vector?)

   (fields (immutable vector
		      gzochi:managed-vector-vector
		      (serialization (gzochi:make-serialization 
				      serialize-managed-vector
				      deserialize-managed-vector))))
   
   (nongenerative gzochi:managed-vector)
   (protocol (lambda (n) (lambda (l) (let ((p (n))) (p (make-vector l #f))))))
   (sealed #t))

  (define (gzochi:managed-vector-length vec)
    (vector-length (gzochi:managed-vector-vector vec)))

  (define (gzochi:managed-vector-ref vec i)
    (gzochi:dereference (vector-ref (gzochi:managed-vector-vector vec) i)))

  (define (gzochi:managed-vector->list vec)
    (map gzochi:dereference (vector->list (gzochi:managed-vector-vector vec))))

  (define (gzochi:managed-vector-set! vec i obj)
    (vector-set! (gzochi:managed-vector-vector vec) i 
		 (gzochi:create-reference obj)))

  (define (gzochi:managed-vector . l)
    (define constructor
      (gzochi:managed-record-constructor
       (gzochi:make-managed-record-constructor-descriptor
	managed-vector #f 
	(lambda (n) (lambda (vec) (let ((p (n))) (p vec)))))))

    (constructor (list->vector (map gzochi:create-reference l))))      
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
