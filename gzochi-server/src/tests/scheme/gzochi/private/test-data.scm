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
(test-end "gzochi:define-managed-record-type")
