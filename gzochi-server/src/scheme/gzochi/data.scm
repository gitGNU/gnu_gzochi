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
	  gzochi:managed-record-rtd
	  gzochi:managed-record-type-name
	  gzochi:managed-record-type-parent
	  gzochi:managed-record-type-uid

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
	  gzochi:managed-hashtable-size
	  gzochi:managed-hashtable-ref
	  gzochi:managed-hashtable-set!
	  gzochi:managed-hashtable-delete!
	  gzochi:managed-hashtable-contains?
	  gzochi:managed-hashtable-update!
	  gzochi:managed-hashtable-clear!
	  gzochi:managed-hashtable-keys
	  gzochi:managed-hashtable-entries
	  gzochi:managed-hashtable-hash-function
	  gzochi:managed-hashtable-equivalence-function

	  gzochi:serialize
	  gzochi:deserialize

	  gzochi:create-reference
	  gzochi:dereference
	  gzochi:get-binding
	  gzochi:set-binding!
	  gzochi:remove-binding!
	  gzochi:remove-object!)

  (import (gzochi io)
	  (gzochi private app)
	  (gzochi private data)
	  (gzochi private reflect)
	  (ice-9 optargs)
	  (rnrs)
	  (only (guile) keyword? modulo)
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
 
  (define (gzochi:managed-serializable-serializer obj)
    (car (gzochi:managed-serializable-callback-with-value obj)))
  
  (define (gzochi:managed-serializable-deserializer obj)
    (cadr (gzochi:managed-serializable-callback-with-value obj)))

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

  (gzochi:define-managed-record-type managed-vector-entry
   (fields (mutable value-internal)
	   (mutable wrapped-value?
		    managed-vector-entry-wrapped-value?
		    managed-vector-entry-wrapped-value-set!
		    (serialization gzochi:boolean-serialization)))

   (nongenerative gzochi:managed-vector-entry)

   (protocol 
    (lambda (n)
      (lambda (value value-serializer value-deserializer)
	(or (gzochi:managed-record? value)
	    (and (gzochi:callback? value-serializer)
		 (gzochi:callback? value-deserializer))
	    (raise (condition
		    (make-assertion-violation)
		    (make-message-condition 
		     "Serialization must be specified for unmanaged values."))))

	(let* ((p (n))
	       (wrapped-value (not (gzochi:managed-record? value)))
	       (value (if wrapped-value
			  (gzochi:make-managed-serializable
			   value value-serializer value-deserializer)
			  value)))

	  (p value wrapped-value)))))

   (sealed #f))

  (define (managed-vector-entry-value entry)
    (let ((value (managed-vector-entry-value-internal entry)))
      (if (managed-vector-entry-wrapped-value? entry)
	  (gzochi:managed-serializable-value value)
	  value)))

  (define (managed-vector-entry-value-set! 
	   entry value value-serializer value-deserializer)
    (if (managed-vector-entry-wrapped-value? entry)
	(gzochi:remove-object! (managed-vector-entry-value-internal entry)))
    (if (gzochi:managed-record? value)
	(begin
	  (managed-vector-entry-value-internal-set! entry value)
	  (managed-vector-entry-wrapped-value-set! entry #f))
	(begin
	  (or (and (gzochi:callback? value-serializer)
		   (gzochi:callback? value-deserializer))
	      (raise 
	       (condition 
		(make-assertion-violation)
		(make-message-condition 
		 "Serialization must be specified for unmanaged values."))))
	  (managed-vector-entry-value-internal-set!
	   entry (gzochi:make-managed-serializable 
		  value value-serializer value-deserializer))
	  (managed-vector-entry-wrapped-value-set! entry #t))))
     
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
    (let ((obj (vector-ref (gzochi:managed-vector-vector vec) i)))
      (and obj (managed-vector-entry-value (gzochi:dereference obj)))))

  (define (gzochi:managed-vector->list vec)
    (map (lambda (obj) 
	   (and obj (managed-vector-entry-value (gzochi:dereference obj))))
	 (vector->list (gzochi:managed-vector-vector vec))))

  (define* (gzochi:managed-vector-set! vec i obj #:key serializer deserializer)
    (let* ((unmanaged-vec (gzochi:managed-vector-vector vec))
	   (ref (vector-ref unmanaged-vec i))
	   (entry (and ref (gzochi:dereference ref))))

      (if entry
	  (begin
	    (managed-vector-entry-value-set! entry obj serializer deserializer)
	    (if (not obj) (gzochi:remove-object! entry)))

	  (if obj
	      (vector-set! 
	       unmanaged-vec i 
	       (gzochi:create-reference 
		(make-managed-vector-entry obj serializer deserializer))))))

    (gzochi:mark-for-write! vec))

  (define* (gzochi:managed-vector #:key serializer deserializer #:rest l)
    (define constructor
      (gzochi:managed-record-constructor
       (gzochi:make-managed-record-constructor-descriptor
	managed-vector #f 
	(lambda (n) (lambda (vec) (let ((p (n))) (p vec)))))))

    (constructor 
     (list->vector 
      (map (lambda (obj) 
	     (gzochi:create-reference 
	      (make-managed-vector-entry obj serializer deserializer)))
	   (let loop 
	       ((l l) (filtered-l '()) (last-was-keyword? #f))
	     (if (null? l)
		 (reverse filtered-l)
		 (let ((cl (car l)))
		   (cond ((keyword? cl) (loop (cdr l) filtered-l #t))
			 (last-was-keyword? (loop (cdr l) filtered-l #f))
			 (else (loop (cdr l) (cons cl filtered-l) #f))))))))))

  (define (gzochi:managed-hashtable-set! ht key value) (if #f #f))

  (gzochi:define-managed-record-type managed-hashtable-entry
   (fields (immutable hash (serialization gzochi:integer-serialization))
	   (immutable key managed-hashtable-entry-key-internal)
	   (immutable 
	    wrapped-key? (serialization gzochi:boolean-serialization))
	   (mutable 
	    value
	    managed-hashtable-entry-value-internal
	    managed-hashtable-entry-value-internal-set!)
	   (mutable 
	    wrapped-value? 
	    managed-hashtable-entry-wrapped-value? 
	    managed-hashtable-entry-wrapped-value-set! 
	    (serialization gzochi:boolean-serialization))
	   (mutable next))

   (nongenerative gzochi:managed-hashtable-entry)
   (protocol 
    (lambda (n)
      (lambda (hash 
	       key key-serializer key-deserializer 
	       value value-serializer value-deserializer)
	(or (gzochi:managed-record? key)
	    (and (gzochi:callback? key-serializer)
		 (gzochi:callback? key-deserializer))
	    (raise (condition 
		    (make-assertion-violation)
		    (make-message-condition 
		     "Serialization must be specified for unmanaged keys."))))
	(or (gzochi:managed-record? value)
	    (and (gzochi:callback? value-serializer)
		 (gzochi:callback? value-deserializer))
	    (raise (condition
		    (make-assertion-violation)
		    (make-message-condition 
		     "Serialization must be specified for unmanaged values."))))

	(let* ((p (n))
	       (wrapped-key (not (gzochi:managed-record? key)))
	       (key (if wrapped-key
			(gzochi:make-managed-serializable 
			 key key-serializer key-deserializer)
			key))
	       (wrapped-value (not (gzochi:managed-record? value)))
	       (value (if wrapped-value
			  (gzochi:make-managed-serializable
			   value value-serializer value-deserializer)
			  value)))

	  (p hash key wrapped-key value wrapped-value #f)))))
   (sealed #f))

  (define (managed-hashtable-entry-key entry)
    (let ((key (managed-hashtable-entry-key-internal entry)))
      (if (managed-hashtable-entry-wrapped-key? entry)
	  (gzochi:managed-serializable-value key)
	  key)))
  
  (define (managed-hashtable-entry-value entry)
    (let ((value (managed-hashtable-entry-value-internal entry)))
      (if (managed-hashtable-entry-wrapped-value? entry)
	  (gzochi:managed-serializable-value value)
	  value)))

  (define (managed-hashtable-entry-value-set! 
	   entry value value-serializer value-deserializer)
    (if (managed-hashtable-entry-wrapped-value? entry)
	(gzochi:remove-object! (managed-hashtable-entry-value-internal entry)))
    (if (gzochi:managed-record? value)
	(begin
	  (managed-hashtable-entry-value-internal-set! entry value)
	  (managed-hashtable-entry-wrapped-value-set! entry #f))
	(begin
	  (or (and (gzochi:callback? value-serializer)
		   (gzochi:callback? value-deserializer))
	      (raise 
	       (condition 
		(make-assertion-violation)
		(make-message-condition 
		 "Serialization must be specified for unmanaged values."))))
	  (managed-hashtable-entry-value-internal-set!
	   entry (gzochi:make-managed-serializable 
		  value value-serializer value-deserializer))
	  (managed-hashtable-entry-wrapped-value-set! entry #t))))

  (gzochi:define-managed-record-type managed-hashtable-node
   (fields (mutable parent)
	   (mutable left-leaf)
	   (mutable right-leaf) 
	   (mutable directory)
	   (mutable entries)

	   (immutable depth (serialization gzochi:integer-serialization))
	   (mutable size (serialization gzochi:integer-serialization)))

   (nongenerative gzochi:managed-hashtable-node)
   (protocol (lambda (n)
	       (lambda (depth)
		 (let ((p (n)))
		   (p #f #f #f #f (gzochi:make-managed-vector 256) depth 0)))))
   (sealed #t))

  (gzochi:define-managed-record-type 
    (gzochi:managed-hashtable 
     gzochi:make-managed-hashtable 
     gzochi:managed-hashtable?)

    (fields hash-function equivalence-function root)

    (nongenerative gzochi:managed-hashtable)
    (protocol (lambda (n)
		(lambda (hash-callback equiv-callback)
		  (let ((p (n)))
		    (let ((root (make-managed-hashtable-node 0)))
		      (managed-hashtable-node-ensure-depth! root 6)
		      (p hash-callback equiv-callback root))))))
    (sealed #t))

  (define (high-bits n num-bits)
    (let ((bit-length (bitwise-length n)))
      (if (< num-bits bit-length)
	  (bitwise-arithmetic-shift-right n (- bit-length num-bits))
	  n)))

  (define (managed-hashtable-node-add-leaves! node prefix left-leaf right-leaf)
    (let* ((prefix (bitwise-arithmetic-shift-left 
		    prefix (managed-hashtable-node-depth node)))
	   (depth (managed-hashtable-node-depth node))
	   (dir-bits (min (- 32 depth) 6))
	   (directory (managed-hashtable-node-directory node))  
	   (index (high-bits prefix dir-bits))
	   (leaf (gzochi:managed-vector-ref directory index)))

      (gzochi:remove-object! leaf)
      (managed-hashtable-node-parent-set! left-leaf node)
      (managed-hashtable-node-parent-set! right-leaf node)
      
      (let* ((sig-bits (- (managed-hashtable-node-depth leaf) depth))
	     (mask (bitwise-arithmetic-shift-left
		    (- (bitwise-arithmetic-shift-left 1 sig-bits) 1)
		    (- dir-bits sig-bits)))

	     (left (bitwise-and index mask))
	     (num-each
	      (bitwise-arithmetic-shift-left 1 (- (- dir-bits sig-bits) 1)))
	     (right (+ left num-each))
	     (left-total (+ left num-each))
	     (right-total (+ right num-each)))

	(let loop ((i left))
	  (or (eqv? i left-total)
	      (begin
		(gzochi:managed-vector-set! directory i left-leaf)
		(loop (+ i 1)))))

	(let loop ((i right))
	  (or (eqv? i right-total)
	      (begin
		(gzochi:managed-vector-set! directory i right-leaf)
		(loop (+ i 1))))))))

  (define (managed-hashtable-node-split! node)
    (or (managed-hashtable-node-entries node)
	(raise (condition 
		(make-assertion-violation)
		(make-message-condition "Can't split a directory node!"))))
    (let* ((depth (managed-hashtable-node-depth node))
	   (entries (managed-hashtable-node-entries node))
	   (left-child (make-managed-hashtable-node (+ depth 1)))
	   (right-child (make-managed-hashtable-node (+ depth 1)))
	   (num-entries (gzochi:managed-vector-length entries))
	   (first-right (/ num-entries 2))
	   (prefix
	    (let loop ((i 0) (prefix 0))
	      (let* ((child (if (< i first-right) left-child right-child))
		     (entry (gzochi:managed-vector-ref entries i))
		     (next-prefix
		      (let loop ((entry entry) (prev #f) (prev-index 0))
			(let* ((hash (managed-hashtable-entry-hash entry))
			       (index (managed-hashtable-leaf-index child hash))
			       (next-entry
				(managed-hashtable-entry-next entry)))
			  (managed-hashtable-node-add-entry!
			   child entry (and (not (eqv? index prev-index)) prev))
			  (if next-entry (loop next-entry entry index) hash)))))
		(loop (+ i 1) next-prefix)))))

      (managed-hashtable-node-entries-set! node #f)
      (managed-hashtable-node-size-set! node 0)

      (let ((left-leaf (managed-hashtable-node-left-leaf node)))
	(if left-leaf
	    (begin
	      (managed-hashtable-node-right-leaf-set! left-leaf left-child)
	      (managed-hashtable-node-left-leaf-set! left-child left-leaf)
	      (managed-hashtable-node-left-leaf-set! node #f))))
      (let ((right-leaf (managed-hashtable-node-right-leaf node)))
	(if right-leaf
	    (begin
	      (managed-hashtable-node-left-leaf-set! right-leaf right-child)
	      (managed-hashtable-node-right-leaf-set! right-child right-leaf)
	      (managed-hashtable-node-right-leaf-set! node #f))))

      (managed-hashtable-node-right-leaf-set! left-child right-child)
      (managed-hashtable-node-left-leaf-set! right-child left-child)

      (if (or (not (managed-hashtable-node-parent node))
	      (eqv? (modulo depth 6) 0)
	      (eqv? depth 6))
	  (begin
	    (managed-hashtable-node-parent-set! right-child node)
	    (managed-hashtable-node-parent-set! left-child node)
	    (let* ((directory-length 
		    (bitwise-arithmetic-shift-left 1 (min (- 32 depth) 6)))
		   (directory (gzochi:make-managed-vector directory-length)))
	      (managed-hashtable-node-directory-set! node directory)
	      (let ((first-right (/ directory-length 2)))
		(let loop ((i 0))
		  (or (eqv? i directory-length)
		      (begin
			(gzochi:managed-vector-set! 
			 directory i (if (< i first-right) 
					 left-child 
					 right-child))
			(loop (+ i 1))))))))
	  
	  (managed-hashtable-node-add-leaves! 
	   (managed-hashtable-node-parent node) prefix left-child 
	   right-child))))

  (define (managed-hashtable-node-ensure-depth! node min-depth)
    (let ((depth (managed-hashtable-node-depth node))) 
      (or (>= depth min-depth)
	  (let* ((directory-length
		  (bitwise-arithmetic-shift-left 1 (min (- 32 depth) 6)))
		 (directory (gzochi:make-managed-vector directory-length)))
	    (managed-hashtable-node-entries-set! node #f)
	    (managed-hashtable-node-directory-set! node directory)
	    (let* ((leaf-bits (min (- min-depth depth) 6))
		   (num-leaves (bitwise-arithmetic-shift-left 1 leaf-bits))
		   (leaves (gzochi:make-managed-vector num-leaves)))
	      (let loop ((i 0))
		(or (eqv? i num-leaves)
		    (let ((leaf (make-managed-hashtable-node 
				 (+ depth leaf-bits))))
		      (gzochi:managed-vector-set! leaves i leaf)
		      (managed-hashtable-node-parent-set! leaf node)
		      (loop (+ i 1)))))
	      (let loop ((i 1))
		(or (eqv? i (- num-leaves 1))
		    (let ((leaf (gzochi:managed-vector-ref leaves i)))
		      (managed-hashtable-node-left-leaf-set! 
		       leaf (gzochi:managed-vector-ref leaves (- i 1)))
		      (managed-hashtable-node-right-leaf-set!
		       leaf (gzochi:managed-vector-ref leaves (+ i 1)))
		      (loop (+ i 1)))))
	      (let ((left-leaf (managed-hashtable-node-left-leaf node))
		    (right-leaf (managed-hashtable-node-right-leaf node))
		    (first-leaf (gzochi:managed-vector-ref leaves 0))
		    (last-leaf (gzochi:managed-vector-ref 
				leaves (- num-leaves 1))))
		(managed-hashtable-node-left-leaf-set! first-leaf left-leaf)
		(if left-leaf 
		    (managed-hashtable-node-right-leaf-set! 
		     left-leaf first-leaf))
		(managed-hashtable-node-right-leaf-set! 
		 first-leaf (gzochi:managed-vector-ref leaves 1))
		(managed-hashtable-node-left-leaf-set! 
		 last-leaf (gzochi:managed-vector-ref leaves (- num-leaves 2)))
		(managed-hashtable-node-right-leaf-set! last-leaf right-leaf)
		(if right-leaf
		    (managed-hashtable-node-left-leaf-set! 
		     right-leaf last-leaf))
		
		(managed-hashtable-node-left-leaf-set! node #f)
		(managed-hashtable-node-right-leaf-set! node #f))
	      
	      ;; Fill the directory.
	      
	      (let* ((entries-per-leaf (/ directory-length num-leaves)))
		(let loop ((i 0) (pos 0))
		  (or (eqv? i num-leaves)
		      (let ((leaf (gzochi:managed-vector-ref leaves i))
			    (next-pos (+ pos entries-per-leaf)))
			(let loop ((j pos))
			  (or (eqv? j next-pos)
			      (begin
				(gzochi:managed-vector-set! directory j leaf)
				(loop (+ j 1)))))
			(loop (+ i 1) next-pos)))))
	      
	      ;; Ensure depth for new leaves.
	      
	      (let loop ((i 0))
		(or (eqv? i num-leaves)
		    (begin
		      (managed-hashtable-node-ensure-depth! 
		       (gzochi:managed-vector-ref leaves i) min-depth)
		      (loop (+ i 1)))))
	      (if #f #f))))))

  (define (managed-hashtable-node-add-entry! node entry prev)
    (managed-hashtable-node-size-set!
     node (+ (managed-hashtable-node-size node) 1))
    (if prev
	(let ((next (managed-hashtable-entry-next prev)))
	  (managed-hashtable-entry-next-set! prev entry)
	  (managed-hashtable-entry-next-set! entry next))
	(let ((index (managed-hashtable-leaf-index
		      node (managed-hashtable-entry-hash entry)))
	      (table (managed-hashtable-node-entries node)))
	  (managed-hashtable-entry-next-set!
	   entry (gzochi:managed-vector-ref table index))
	  (gzochi:managed-vector-set! table index entry))))

  (define (managed-hashtable-node-add-entry-and-split! node entry prev)
    (managed-hashtable-node-add-entry! node entry prev)
    (if (and (> (managed-hashtable-node-size node) 98)
	     (< (managed-hashtable-node-depth node) 31))
	(managed-hashtable-node-split! node)))

  (define (managed-hashtable-node-lookup node prefix)
    (let loop ((node node))
      (if (not (managed-hashtable-node-entries node))
	  (let* ((depth (managed-hashtable-node-depth node))
		 (directory (managed-hashtable-node-directory node))
		 (index (high-bits
			 (bitwise-arithmetic-shift-left prefix depth)
			 (min (- 32 depth) 6))))

	    (loop (gzochi:managed-vector-ref directory index)))
	  node)))

  (define* (gzochi:managed-hashtable-set! ht key value #:key
					  key-serializer key-deserializer
					  value-serializer value-deserializer)
					  
    (let* ((hash-function (gzochi:managed-hashtable-hash-function ht))
	   (hash-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure hash-function)
			   (gzochi:callback-module hash-function)))

	   (equiv-function (gzochi:managed-hashtable-equivalence-function ht))
	   (equiv-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure equiv-function)
			   (gzochi:callback-module equiv-function)))

	   (hash (if key (hash-function key) 0))
	   (leaf (managed-hashtable-node-lookup 
		  (gzochi:managed-hashtable-root ht) hash)))
    
      (let loop ((entry (gzochi:managed-vector-ref 
			 (managed-hashtable-node-entries leaf)
			 (managed-hashtable-leaf-index leaf hash)))
		 (prev #f))
	(if entry
	    (let ((entry-hash (managed-hashtable-entry-hash entry)))
	      (cond ((< entry-hash hash)
		     (loop (managed-hashtable-entry-next entry) entry))
		    ((not (eqv? entry-hash hash)) (loop #f prev))
 		    (else
		     (if (equiv-function 
			  (managed-hashtable-entry-key entry) key)
			 (managed-hashtable-entry-value-set! 
			  entry value value-serializer value-deserializer)
			 (loop (managed-hashtable-entry-next entry) 
			       entry)))))
	    (managed-hashtable-node-add-entry-and-split!
	     leaf (make-managed-hashtable-entry
		   hash
		   key key-serializer key-deserializer 
		   value value-serializer value-deserializer)
	     prev)))))

  (define (managed-hashtable-leaf-index node hash)
    (let* ((leaf-capacity 256)
	   (leaf-bits (bitwise-length leaf-capacity))
	   (left-offset (+ (managed-hashtable-node-depth node) leaf-bits)))
      (bitwise-and (high-bits hash left-offset) (- leaf-capacity 1))))

  (define (managed-hashtable-get-entry ht key)
    (let* ((hash-function (gzochi:managed-hashtable-hash-function ht))
	   (hash-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure hash-function)
			   (gzochi:callback-module hash-function)))

	   (equiv-function (gzochi:managed-hashtable-equivalence-function ht))
	   (equiv-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure equiv-function)
			   (gzochi:callback-module equiv-function)))
	   
	   (hash (if key (hash-function key) 0))
	   (leaf (managed-hashtable-node-lookup 
		  (gzochi:managed-hashtable-root ht) hash)))

      (let loop ((entry (gzochi:managed-vector-ref 
			 (managed-hashtable-node-entries leaf)
			 (managed-hashtable-leaf-index leaf hash))))
	(and entry
	     (let ((entry-hash (managed-hashtable-entry-hash entry)))
	       (cond ((and (eqv? entry-hash hash)
			   (equiv-function
			    (managed-hashtable-entry-key entry) key))
		      entry)
		     ((< entry-hash hash) #f)
		     (else (loop (managed-hashtable-entry-next entry)))))))))

  (define (gzochi:managed-hashtable-contains? ht key)
    (if (managed-hashtable-get-entry ht key) #t #f))

  (define* (gzochi:managed-hashtable-update! 
	    ht key proc default #:key 
	    key-serializer key-deserializer
	    value-serializer value-deserializer)

    (let ((entry (managed-hashtable-get-entry ht key)))
      (if entry
	  (managed-hashtable-entry-value-set! 
	   entry (proc (managed-hashtable-entry-value entry)) value-serializer 
	   value-deserializer)
	  (gzochi:managed-hashtable-set! 
	   ht key (proc default) 
	   #:key-serializer key-serializer #:key-deserializer key-deserializer
	   #:value-serializer value-serializer
	   #:value-deserializer value-deserializer))))

  (define (gzochi:managed-hashtable-clear! ht)
    (define (clear-internal node)
      (let ((entries (managed-hashtable-node-entries node)))
	(if entries
	    (let ((len (gzochi:managed-vector-length entries)))
	      (let loop ((i 0))
		(if (< i len)
		    (begin 
		      (let loop ((entry (gzochi:managed-vector-ref entries i)))
			(if entry
			    (begin
			      (let ((next (managed-hashtable-entry-next entry)))
				(gzochi:remove-object! entry)
				(loop next)))))
		      (loop (+ i 1))))))

	    (let* ((directory (managed-hashtable-node-directory node))
		   (len (gzochi:managed-vector-length directory)))
	      (let loop ((i 0))
		(if (< i len) 
		    (begin
		      (clear-internal 
		       (gzochi:managed-vector-ref directory i))
		      (loop (+ i 1))))))))

      (managed-hashtable-node-size-set! node 0)
      (managed-hashtable-node-left-leaf-set! node #f)
      (managed-hashtable-node-right-leaf-set! node #f)
      (gzochi:remove-object! node))
  
    (let ((root (gzochi:managed-hashtable-root ht)))
      (clear-internal root)
      (managed-hashtable-node-ensure-depth! root 6)))

  (define (gzochi:managed-hashtable-ref ht key default)
    (let ((entry (managed-hashtable-get-entry ht key)))
      (if entry
	  (managed-hashtable-entry-value entry)
	  default)))

  (define (gzochi:managed-hashtable-delete! ht key)
    (let* ((hash-function (gzochi:managed-hashtable-hash-function ht))
	   (hash-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure hash-function)
			   (gzochi:callback-module hash-function)))

	   (equiv-function (gzochi:managed-hashtable-equivalence-function ht))
	   (equiv-function (gzochi:resolve-procedure
			   (gzochi:callback-procedure equiv-function)
			   (gzochi:callback-module equiv-function)))
	   
	   (hash (if key (hash-function key) 0))
	   (leaf (managed-hashtable-node-lookup 
		  (gzochi:managed-hashtable-root ht) hash))
	   (entries (managed-hashtable-node-entries leaf))
	   (index (managed-hashtable-leaf-index leaf hash)))

      (let loop ((entry (gzochi:managed-vector-ref entries index)) (prev #f))
	(and entry
	     (let ((entry-hash (managed-hashtable-entry-hash entry)))
	       (cond ((or (not entry) (> entry-hash hash)) (if #f #f))
		     ((and (eqv? entry-hash hash)
			   (equiv-function 
			    (managed-hashtable-entry-key entry) key))
		      (managed-hashtable-node-size-set! 
		       leaf (- (managed-hashtable-node-size leaf) 1))
		      (let ((next (managed-hashtable-entry-next entry)))
			(if prev
			    (managed-hashtable-entry-next-set! prev next)
			    (gzochi:managed-vector-set! entries index next)))
		      (gzochi:remove-object! entry))
		     (else (loop (managed-hashtable-entry-next entry) 
				 entry))))))))

  (define (gzochi:managed-hashtable-size ht)
    (let ((cur (managed-hashtable-node-lookup 
		(gzochi:managed-hashtable-root ht) 0)))
      (let loop ((cur cur) (total-size (managed-hashtable-node-size cur)))
	(let ((next (managed-hashtable-node-right-leaf cur)))
	  (if next
	      (loop next (+ total-size (managed-hashtable-node-size next)))
	      total-size)))))

  (define (managed-hashtable-entries ht)
    (define (first-entry node)
      (let* ((table (managed-hashtable-node-entries node))
	     (table-length (gzochi:managed-vector-length table)))
	(let loop ((i 0))
	  (and (not (eqv? i table-length))
	       (or (gzochi:managed-vector-ref table i)
		   (loop (+ i 1)))))))
    
    (let ((left-most 
	   (managed-hashtable-node-lookup 
	    (gzochi:managed-hashtable-root ht) 0)))
      
      (let loop ((current-leaf left-most)
		 (current-entry (first-entry left-most))
		 (entries (list)))
	(if current-entry
	    (loop current-leaf 
		  (managed-hashtable-entry-next current-entry) 
		  (cons current-entry entries))
	    (let ((next-leaf (managed-hashtable-node-right-leaf current-leaf)))
	      (if next-leaf
		  (loop next-leaf (first-entry next-leaf) entries)
		  (reverse entries)))))))

  (define (gzochi:managed-hashtable-entries ht)
    (let* ((entries (managed-hashtable-entries ht))
	   (num-entries (length entries))
	   (key-vector (gzochi:make-managed-vector num-entries))
	   (value-vector (gzochi:make-managed-vector num-entries)))

      (let loop ((i 0) (entries entries))
	(or (eqv? i num-entries)
	    (let ((entry (car entries)))
	      (if (managed-hashtable-entry-wrapped-key? entry)
		  (let ((ms (managed-hashtable-entry-key-internal entry)))
		    (gzochi:managed-vector-set!
		     key-vector i (gzochi:managed-serializable-value ms)
		     #:serializer (gzochi:managed-serializable-serializer ms)
		     #:deserializer (gzochi:managed-serializable-deserializer 
				     ms)))
		  (gzochi:managed-vector-set! 
		   key-vector i (managed-hashtable-entry-key entry)))
	      (if (managed-hashtable-entry-wrapped-value? entry)
		  (let ((ms (managed-hashtable-entry-value-internal entry)))
		    (gzochi:managed-vector-set!
		     value-vector i (gzochi:managed-serializable-value ms)
		     #:serializer (gzochi:managed-serializable-serializer ms)
		     #:deserializer (gzochi:managed-serializable-deserializer 
				     ms)))
		  (gzochi:managed-vector-set! 
		   value-vector i (managed-hashtable-entry-value entry)))
	      (loop (+ i 1) (cdr entries)))))

      (values key-vector value-vector)))

  (define (gzochi:managed-hashtable-keys ht)
    (let* ((entries (managed-hashtable-entries ht))
	   (num-entries (length entries))
	   (key-vector (gzochi:make-managed-vector num-entries)))

      (let loop ((i 0) (entries entries))
	(if (< i num-entries)
	    (let ((entry (car entries)))
	      (if (managed-hashtable-entry-wrapped-key? entry)
		  (let ((ms (managed-hashtable-entry-key-internal entry)))
		    (gzochi:managed-vector-set!
		     key-vector i (gzochi:managed-serializable-value ms)
		     #:serializer (gzochi:managed-serializable-serializer ms)
		     #:deserializer (gzochi:managed-serializable-deserializer 
				     ms)))
		     
		  (gzochi:managed-vector-set!
		   key-vector i (managed-hashtable-entry-key entry)))
	      
	      (loop (+ i 1) (cdr entries)))
	    key-vector))))
  
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
