;; gzochi/private/reflect.scm: Exports for common binding reflection procedures
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

(library (gzochi private reflect)
  (export gzochi:resolve-procedure)
  (import (guile)
	  (ice-9 format)
	  (ice-9 threads)
	  (rnrs conditions)
	  (rnrs exceptions))

  (define module-resolution-mutex (make-mutex))

  (define (gzochi:resolve-procedure procedure module-name)
    (with-mutex module-resolution-mutex

      ;; resolve-module is not thread-safe, at least as of Guile 2.0.6.
      ;; If multiple threads attempt to resolve the same module name at once,
      ;; some of them may get false negatives. Protecting this function with a
      ;; a mutex seems to prevent that from happening, but there are surely
      ;; performance implications.

      (let ((module (resolve-module module-name #:ensure #f)))
	(or module
	    (raise 
	     (condition 
	      (make-assertion-violation)
	      (make-message-condition 
	       (format #f "Unable to resolve ~A" module-name)))))

	(let ((variable (module-variable module procedure)))
	  (or variable 
	      (raise 
	       (condition 
		(make-assertion-violation)
		(make-message-condition 
		 (format #f "No binding for ~A in ~A" 
			 procedure module-name)))))

	(variable-ref variable)))))
)
