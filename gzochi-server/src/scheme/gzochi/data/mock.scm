;; gzochi/data/mock.scm: Mock infrastructure for gzochi data API
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

(library (gzochi data mock)
  (export)
  (import (only (guile) resolve-module module-set!)
	  (gzochi conditions)
	  (gzochi private data)
	  (rnrs))

  ;; This library has no exports; it exists solely to override some internal
  ;; wiring of the gzochid container to make unit testing gzochi application
  ;; Scheme code easier. Do not import this library into your application
  ;; directly! Doing so will likely break your application. Import this library
  ;; into test code only.
  
  (define names->oids (make-hashtable string-hash equal?))
  (define oids->objs (make-eqv-hashtable))
  (define objs->oids (make-eq-hashtable))

  (define next-oid 0)
  
  (define (mock-mark-for-write! obj) (if #f #f))

  (define (ensure-oid obj)
    (or (hashtable-ref objs->oids obj #f)
	(begin
	  (hashtable-set! objs->oids obj next-oid)
	  (hashtable-set! oids->objs next-oid obj)
	  (let ((oid next-oid)) (set! next-oid (+ next-oid 1)) oid))))
  
  (define (mock-create-reference obj)
    (gzochi:make-managed-reference (ensure-oid obj)))

  (define (mock-dereference ref)
    (or (hashtable-ref oids->objs (gzochi:managed-reference-oid ref) #f)
	(raise (gzochi:make-object-removed-condition))))

  (define (mock-get-binding name)
    (let ((oid (hashtable-ref names->oids name #f)))
      (or oid (raise (gzochi:make-name-not-bound-condition name)))
      (or (hashtable-ref oids->objs oid #f)
	  (raise (gzochi:make-object-removed-condition)))))

  (define (mock-set-binding! name obj)
    (if (hashtable-contains? names->oids name)
	(raise (gzochi:make-name-exists-condition name))
	(hashtable-set! names->oids name (ensure-oid obj))))

  (define (mock-remove-binding! name)
    (if (hashtable-contains? names->oids name)
	(hashtable-delete! names->oids name)
	(raise (gzochi:make-name-not-bound-condition name))))

  (define (mock-remove-object! obj)
    (let ((oid (hashtable-ref objs->oids obj #f)))
      (if oid
	  (begin
	    (hashtable-delete! oids->objs oid)
	    (hashtable-delete! objs->oids obj)))))
      
  (let ((module (resolve-module '(gzochi private data))))
    (module-set! module 'primitive-mark-for-write! mock-mark-for-write!)
    (module-set! module 'primitive-create-reference mock-create-reference)
    (module-set! module 'primitive-dereference mock-dereference)
    (module-set! module 'primitive-get-binding mock-get-binding)
    (module-set! module 'primitive-set-binding! mock-set-binding!)
    (module-set! module 'primitive-remove-binding! mock-remove-binding!)
    (module-set! module 'primitive-remove-object! mock-remove-object!))
)
