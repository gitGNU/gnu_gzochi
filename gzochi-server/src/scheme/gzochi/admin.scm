;; gzochi/admin.scm: Administrative interface to gzochid
;; Copyright (C) 2012 Julian Graham
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

(library (gzochi admin)
  (export gzochi:application-context?
	  gzochi:application-context-name

          gzochi:applications
	  gzochi:current-application
	  gzochi:with-application)
  (import (only (guile) thunk?)
	  (rnrs base)
	  (rnrs conditions)
	  (rnrs exceptions)
	  (rnrs records syntactic))

  (define-record-type (gzochi:application-context 
		       gzochi:make-application-context 
		       gzochi:application-context?)
    (fields name)
    (opaque #t)
    (sealed #t))

  (define primitive-applications #f)
  (define primitive-current-application #f)
  (define primitive-with-application #f)

  (define (gzochi:applications)
    (primitive-applications))

  (define (gzochi:current-application)
    (primitive-current-application))

  (define (gzochi:with-application context thunk)
    (or (gzochi:application-context? context)
	(raise (condition
		(make-assertion-violation)
		(make-irritants-condition context))))
    (or (thunk? thunk)
	(raise (condition
		(make-assertion-violation)
		(make-irritants-condition thunk))))

    (primitive-with-application context thunk))
)
