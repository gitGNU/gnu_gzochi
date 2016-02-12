;; gzochi/private/data/migration.scm: Private infrastructure for migration tools
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

(library (gzochi private data migration)
  (export gzochi:visit-object)
  (import (gzochi data) (gzochi private data) (rnrs))

  (define (enqueue-oids! migration oids) 
    (assertion-violation 'gzochi:visit-object "enqueue-oids! not initialized."))

  (define (gzochi:visit-object migration obj visitor)
    (define (reference-fields obj)
      (define (reference-fields-inner rtd oids)
	(let ((num-fields (vector-length (record-type-field-names rtd))))
	  (let loop ((i 0) (oids oids))
	    (if (< i num-fields)
		(let* ((accessor (record-accessor rtd i))
		       (val (accessor obj)))
		  (loop (+ i 1)
			(if (gzochi:managed-reference? val)
			    (cons (gzochi:managed-reference-oid val) oids)
			    oids)))
		(let ((parent-rtd (gzochi:managed-record-type-parent rtd)))
		  (if (eq? gzochi:managed-record parent-rtd)
		      oids
		      (reference-fields-inner parent-rtd oids)))))))
      (reference-fields-inner (gzochi:managed-record-rtd obj) '()))
    (enqueue-oids! migration (reference-fields obj))
    (visitor obj))
)
