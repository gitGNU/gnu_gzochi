;; gzochi/private/test-data.scm: Scheme unit tests for private data module
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

(import (gzochi private data))
(import (gzochi srfi-64-support))
(import (rnrs))
(import (srfi :64))

(test-runner-current (gzochi:test-runner))

(test-begin "gzochi:make-managed-record-type-descriptor")
(test-group "serial-uid-nongenerative"
  (gzochi:make-managed-record-type-descriptor 
   'test-record-type-1a #f #f #f #f (vector) #:serial-uid 'test-record-type-1)
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:make-managed-record-type-descriptor 
		  'test-record-type-1b #f #f #f #f (vector) 
		  #:serial-uid 'test-record-type-1)
		 #f))
  (gzochi:make-managed-record-type-descriptor 
   'test-record-type-2a #f 'test-record-type-2 #f #f (vector))
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:make-managed-record-type-descriptor 
		  'test-record-type-2b #f #f #f #f (vector) 
		  #:serial-uid 'test-record-type-2)
		 #f))
  (gzochi:make-managed-record-type-descriptor 
   'test-record-type-3a #f #f #f #f (vector))
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:make-managed-record-type-descriptor 
		  'test-record-type-3b #f #f #f #f (vector) 
		  #:serial-uid 'test-record-type-3a)
		 #f)))

(test-group "type-registry"
  (let ((test-type-registry (gzochi:make-managed-record-type-registry)))
    (gzochi:make-managed-record-type-descriptor
     'test-record-type-4a #f #f #f #f (vector))
    (test-assert (gzochi:make-managed-record-type-descriptor
		  'test-record-type-4b #f #f #f #f (vector)
		  #:serial-uid 'test-record-type-4a
		  #:type-registry test-type-registry)))
  (let ((test-type-registry (gzochi:make-managed-record-type-registry)))
    (gzochi:make-managed-record-type-descriptor
     'test-record-type-5a #f #f #f #f (vector)
     #:type-registry test-type-registry)
    (test-assert (guard (ex ((assertion-violation? ex) #t))
		   (gzochi:make-managed-record-type-descriptor
		    'test-record-type-5b #f #f #f #f (vector)
		    #:serial-uid 'test-record-type-5a
		    #:type-registry test-type-registry)))))
  
(test-end "gzochi:make-managed-record-type-descriptor")

(test-begin "gzochi:define-managed-record-type")
(gzochi:define-managed-record-type syntax-test-record-type-1a
  (serial-uid syntax-test-record-type-1a))

(test-group "serial-uid"
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:make-managed-record-type-descriptor 
		  'syntax-test-record-type-1b #f #f #f #f (vector) 
		  #:serial-uid 'syntax-test-record-type-1a)
		 #f)))

(define syntax-test-type-registry (gzochi:make-managed-record-type-registry))

(gzochi:define-managed-record-type syntax-test-record-type-2a
  (type-registry syntax-test-type-registry))

(test-group "type-registry"
  (test-assert (guard (ex ((assertion-violation? ex) #t))
		 (gzochi:make-managed-record-type-descriptor 
		  'syntax-test-record-type-2b #f #f #f #f (vector) 
		  #:serial-uid 'syntax-test-record-type-2a
		  #:type-registry syntax-test-type-registry)
		 #f)))
(test-end "gzochi:define-managed-record-type")

(test-begin "gzochi:serialize-managed-record")
(test-group "type-registry"
  (let* ((test-type-registry (gzochi:make-managed-record-type-registry))
	 (rtd (gzochi:make-managed-record-type-descriptor 
	       'test-record-type-6a #f #f #f #f (vector) 
	       #:type-registry test-type-registry))
	 (record
	  ((record-constructor 
	    (gzochi:make-managed-record-constructor-descriptor rtd #f #f)))))
    (let-values (((port func) (open-bytevector-output-port)))
      (test-assert (guard (ex ((assertion-violation? ex) #t))
		     (gzochi:serialize-managed-record port record)
		     #f))
      (test-assert
       (with-fluids ((%gzochi:type-registry test-type-registry))
	 (display record) (newline)
	 (gzochi:serialize-managed-record port record)
	 #t)))))
(test-end "gzochi:serialize-managed-record")

(test-begin "gzochi:deserialize-managed-record")
(test-group "type-registry"
  (let* ((test-type-registry (gzochi:make-managed-record-type-registry))
	 (rtd (gzochi:make-managed-record-type-descriptor 
	       'test-record-type-7a #f #f #f #f (vector) 
	       #:type-registry test-type-registry))
	 (record
	  ((record-constructor 
	    (gzochi:make-managed-record-constructor-descriptor rtd #f #f)))))
    (let-values (((port func) (open-bytevector-output-port)))
       (with-fluids ((%gzochi:type-registry test-type-registry))
	 (gzochi:serialize-managed-record port record))
       (let ((bv (func)))
	 (test-assert 
	  (guard (ex ((assertion-violation? ex) #t))
	    (gzochi:deserialize-managed-record (open-bytevector-input-port bv))
	    #f))
	 (test-assert
	  (with-fluids ((%gzochi:type-registry test-type-registry))
	    (gzochi:deserialize-managed-record 
	     (open-bytevector-input-port bv))))))))
(test-end "gzochi:deserialize-managed-record")
