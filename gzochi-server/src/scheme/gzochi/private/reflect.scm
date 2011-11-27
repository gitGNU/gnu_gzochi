;; gzochi/private/reflect.scm: Exports for common binding reflection procedures
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

(library (gzochi private reflect)
  (export gzochi:resolve-procedure)
  (import (guile)
	  (rnrs conditions)
	  (rnrs exceptions))

  (define (gzochi:resolve-procedure procedure module-name)
    (let* ((module (resolve-module module-name))
	   (variable (module-variable module procedure)))
      (or variable 
	  (raise 
	   (condition 
	    (make-assertion-violation)
	    (make-message-condition 
	     (string-append 
	      "No binding for " (symbol->string procedure) " in "
	      module-name)))))
      (variable-ref variable)))
)
