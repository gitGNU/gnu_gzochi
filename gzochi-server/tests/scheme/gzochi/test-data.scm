;; gzochi/test-data.scm: Scheme unit tests for gzochi data API
;; Copyright (C) 2014 Julian Graham
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

(import (gzochi app))
(import (gzochi data))
(import (gzochi io))
(import (gzochi mock-data))
(import (gzochi private data))
(import (gzochi srfi-64-support))
(import (rnrs io ports))
(import (srfi :64))

(initialize-mock-data)

(gzochi:define-managed-record-type test-record
  (fields (mutable foo (serialization gzochi:string-serialization))))

(define write-int (gzochi:make-callback 'gzochi:write-integer '(gzochi io)))
(define read-int (gzochi:make-callback 'gzochi:read-integer '(gzochi io)))

(define write-str (gzochi:make-callback 'gzochi:write-string '(gzochi io)))
(define read-str (gzochi:make-callback 'gzochi:read-string '(gzochi io)))

(define default-max-bucket-size 10)

(test-runner-current (gzochi:test-runner))

(test-begin "gzochi:managed-sequence")

(test-group "->list"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add!
     seq 1 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-add!
     seq 2 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-add!
     seq 3 #:serializer write-int #:deserializer read-int)
    (test-equal '(1 2 3) (gzochi:managed-sequence->list seq))))

(test-group "add!"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add! 
     seq "foo" #:serializer write-str #:deserializer read-str)
    (test-eqv 1 (gzochi:managed-sequence-size seq))
    (test-equal "foo" (gzochi:managed-sequence-ref seq 0))))

(test-group "contains?"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add! 
     seq 123 #:serializer write-int #:deserializer read-int)
    (test-eqv #t (gzochi:managed-sequence-contains? seq 123 eqv?))
    (test-eqv #f (gzochi:managed-sequence-contains? seq 456 eqv?))))

(test-group "insert-at!"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-insert! 
     seq 0 3 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-insert! 
     seq 0 2 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-insert! 
     seq 0 1 #:serializer write-int #:deserializer read-int)
    (test-eqv 3 (gzochi:managed-sequence-size seq))
    (test-eqv 1 (gzochi:managed-sequence-ref seq 0))
    (test-eqv 2 (gzochi:managed-sequence-ref seq 1))
    (test-eqv 3 (gzochi:managed-sequence-ref seq 2))))

(test-group "delete-at!"
  (let ((seq (gzochi:make-managed-sequence))
	(rec (make-test-record "foo")))
    
    (gzochi:managed-sequence-add! seq rec)
    (gzochi:managed-sequence-delete-at! seq 0)
    (test-eqv 0 (gzochi:managed-sequence-size seq))))

(test-group "fold-left"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add!
     seq "a" #:serializer write-str #:deserializer read-str)
    (gzochi:managed-sequence-add!
     seq "b" #:serializer write-str #:deserializer read-str)
    (gzochi:managed-sequence-add!
     seq "c" #:serializer write-str #:deserializer read-str)
    (test-equal 
     "cba" (gzochi:managed-sequence-fold-left seq string-append ""))))

(test-group "fold-right"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add!
     seq "a" #:serializer write-str #:deserializer read-str)
    (gzochi:managed-sequence-add!
     seq "b" #:serializer write-str #:deserializer read-str)
    (gzochi:managed-sequence-add!
     seq "c" #:serializer write-str #:deserializer read-str)
    (test-equal 
     "abc" (gzochi:managed-sequence-fold-right seq string-append ""))))

(test-group "force-split"
  (let ((seq (gzochi:make-managed-sequence)))
    (let loop ((i 0))
      (if (<= i default-max-bucket-size)
	  (begin
	    (gzochi:managed-sequence-add!
	     seq i #:serializer write-int #:deserializer read-int)
	    (loop (+ i 1)))))
    (test-eqv (+ default-max-bucket-size 1) 
	      (gzochi:managed-sequence-size seq))))

(test-group "force-prune"
  (let ((seq (gzochi:make-managed-sequence)))
    (let loop ((i 0))
      (if (<= i default-max-bucket-size)
	  (begin
	    (gzochi:managed-sequence-add!
	     seq i #:serializer write-int #:deserializer read-int)
	    (loop (+ i 1)))))
    (let loop ((i 0))
      (if (<= i (/ default-max-bucket-size 2))
	  (begin (gzochi:managed-sequence-delete-at! seq 0) (loop (+ i 1)))))
      
    (test-eqv (- (+ default-max-bucket-size 1) 
		 (+ (/ default-max-bucket-size 2) 1))
	      (gzochi:managed-sequence-size seq))))

(test-group "insert!"
  (let ((seq (gzochi:make-managed-sequence)))
    (gzochi:managed-sequence-add!
     seq 1 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-add!
     seq 3 #:serializer write-int #:deserializer read-int)
    (gzochi:managed-sequence-insert!
     seq 1 2 #:serializer write-int #:deserializer read-int)
    (test-equal '(1 2 3) (gzochi:managed-sequence->list seq))))

(test-end "gzochi:managed-sequence")
