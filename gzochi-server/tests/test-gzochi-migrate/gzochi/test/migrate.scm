;; gzochi/test/migrate.scm: Migrator implementation for test-gzochi-migrate.sh
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

(library (gzochi test migrate)
  (export pre-type-registry post-type-registry test-visitor)
  (import (only (guile) simple-format) (gzochi) (rnrs))

  (define pre-type-registry (gzochi:make-managed-record-type-registry))
  (define post-type-registry (gzochi:make-managed-record-type-registry))

  ;; The pre-migration managed record type definition.
  
  (gzochi:define-managed-record-type integer-holder
    (fields (immutable integer (serialization gzochi:integer-serialization)))
    (serial-uid integer-holder)
    (type-registry pre-type-registry) ;; Register in the pre-migration registry.
    (protocol (lambda (p) (lambda (integer) (p integer)))))

  ;; The post-migration managed record type definition.
  
  (gzochi:define-managed-record-type annotated-integer-holder
    (fields (immutable integer (serialization gzochi:integer-serialization))
	    (immutable annotation (serialization gzochi:string-serialization)))
    (serial-uid integer-holder)

    ;; Register in the post-migration registry.
    
    (type-registry post-type-registry) 
    (protocol (lambda (p) (lambda (integer) (p integer "foo")))))

  ;; The migration visitor procedure; transforms the record from an
  ;; `integer-holder' to an `annotated-integer-holder'.
  
  (define (test-visitor obj)
    (if (integer-holder? obj)
	(make-annotated-integer-holder (integer-holder-integer obj))))
)
