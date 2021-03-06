;; gzochi/app.scm: Public exports for general gzochi application support 
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

(library (gzochi app)
  (export gzochi:make-callback
	  gzochi:callback?
	  gzochi:callback-module
	  gzochi:callback-procedure
	  gzochi:callback-data

	  %gzochi:application-root
	  g:@)

  (import (only (guile) make-fluid)
	  (gzochi private app)
	  (rnrs base)
	  (rnrs syntax-case)
	  (system syntax))

  (define-syntax g:@
    (lambda (stx)
      (syntax-case stx ()
	((_ (module ...) procedure)
	 #'(gzochi:make-callback (quote procedure) (quote (module ...))))
	((_ (module ...) procedure data)
	 #'(gzochi:make-callback 
	    (quote procedure) (quote (module ...)) data))
	((_ procedure) 
	 #`(gzochi:make-callback 
	    (quote procedure) (syntax-module #'procedure)))
	((_ procedure data)
	 #`(gzochi:make-callback
	    (quote procedure) (syntax-module #'procedure)
	    data)))))

  (define %gzochi:application-root (make-fluid))
)
