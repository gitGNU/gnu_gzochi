;; gzochi/mock-data.scm: Ephemeral implementation of gzochi data services
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

(library (gzochi mock-data)
  (export initialize-mock-data)
  (import (guile)
	  (gzochi conditions)
	  (gzochi data)
	  (gzochi private data)
	  (rnrs base)
	  (rnrs hashtables))

  (define records->references (make-eq-hashtable))
  (define oids->records (make-eqv-hashtable))
  (define next-oid 0)

  (define (initialize-mock-data)
    (define gzochi-private-data 
      (resolve-module '(gzochi private data) #:ensure #f))

    (variable-set! 
     (module-variable gzochi-private-data 'primitive-create-reference)
     (lambda (record)
       (or (hashtable-ref records->references record #f)
	   (let ((ref (gzochi:make-managed-reference next-oid)))
	     (hashtable-set! records->references record ref)
	     (hashtable-set! oids->records next-oid record)
	     (set! next-oid (+ next-oid 1))
	     ref))))
    
    (variable-set!
     (module-variable gzochi-private-data 'primitive-dereference)
     (lambda (ref)
       (or (hashtable-ref oids->records (gzochi:managed-reference-oid ref) #f)
	   (raise (gzochi:make-object-removed-condition)))))

    (variable-set!
     (module-variable gzochi-private-data 'primitive-mark-for-write!)
     (lambda (record) (if #f #f)))

    (variable-set!
     (module-variable gzochi-private-data 'primitive-remove-object!)
     (lambda (record) (if #f #f))))
)
