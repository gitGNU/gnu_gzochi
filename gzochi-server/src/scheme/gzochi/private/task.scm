;; gzochi/private/task.scm: Private infrastructure for gzochi task API
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

(library (gzochi private task)
  (export gzochi:make-task
	  gzochi:task?
	  gzochi:task-module
	  gzochi:task-procedure
	  gzochi:task-data

          gzochi:run-task
	  gzochi:schedule-task)

  (import (guile)
	  (gzochi data)
	  (gzochi io)
	  (gzochi private data)
	  (gzochi private reflect)
	  (rnrs base)
	  (rnrs conditions)
	  (rnrs exceptions))

  (define primitive-schedule-task #f)
  
  (define (gzochi:run-task task)
    (let ((procedure (gzochi:resolve-procedure 
		      (gzochi:task-procedure task)
		      (gzochi:task-module task))))
      (apply procedure (map gzochi:dereference (gzochi:task-data task)))))

  (define (gzochi:schedule-task task . args)
    (or (gzochi:task? task)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition task))))

    (if (null? args)
	(primitive-schedule-task task 0)
	(or (and (eqv? (length args) 1)
		 (let ((x (car args))) 
		   (and (integer? x) 
			(>= x 0) 
			(primitive-schedule-task task x))))
	    (raise (condition (make-assertion-violation)
			      (make-irritants-condition args))))))

  (gzochi:define-managed-record-type 
    (gzochi:task gzochi:make-task gzochi:task?)

    (fields (immutable procedure (serialization gzochi:symbol-serialization))
	    (immutable module (serialization
			       (gzochi:make-uniform-list-serialization
				gzochi:symbol-serialization)))
	    (immutable data (serialization
			     (gzochi:make-uniform-list-serialization
			      gzochi:managed-reference-serialization))))
    (nongenerative gzochi:task)
    (protocol (lambda (n)
		(lambda (procedure module . args)
		  (let ((p (n))) 
		    (p procedure module 
		       (map gzochi:create-reference args))))))
    (sealed #t))
)