;; gzochi/private/task.scm: Private infrastructure for gzochi task API
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

(library (gzochi private task)
  (export gzochi:cancel-task
	  gzochi:run-task
	  gzochi:schedule-task

	  gzochi:make-task-handle
	  gzochi:task-handle?
	  gzochi:task-handle-oid)

  (import (guile)
	  (gzochi io)
	  (gzochi private app)
	  (gzochi private data)
	  (gzochi private reflect)
	  (ice-9 optargs)
	  (rnrs base)
	  (rnrs conditions)
	  (rnrs exceptions))

  (gzochi:define-managed-record-type
   (gzochi:task-handle gzochi:make-task-handle gzochi:task-handle?)

   (fields (immutable oid (serialization gzochi:integer-serialization))))

  (define primitive-cancel-task #f)
  (define primitive-schedule-task #f)

  (define (gzochi:cancel-task handle)
    (or (gzochi:task-handle? handle)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition handle))))
    
    (primitive-cancel-task handle))

  (define (gzochi:run-task callback)
    (let ((procedure (gzochi:resolve-procedure 
		      (gzochi:callback-procedure callback)
		      (gzochi:callback-module callback))))
      (procedure (gzochi:callback-data callback))))

  (define* (gzochi:schedule-task callback #:optional delay period)
    (or (gzochi:callback? callback)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition callback))))

    (if delay
	(begin
	  (or (and (integer? delay) (>= delay 0))
	      (raise (condition (make-assertion-violation)
				(make-irritants-condition delay))))
	  (if period
	      (begin
		(or (and (integer? period) (>= period 0))
		    (raise (condition (make-assertion-violation)
				      (make-irritants-condition period))))
		(primitive-schedule-task callback delay period))
	      (primitive-schedule-task callback delay)))
	(primitive-schedule-task callback)))
)
