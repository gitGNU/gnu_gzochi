;; gzochi/private/test-app.scm: Scheme unit tests for private app support module
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

(library (gzochi private test-app)
  (export)
  (import (gzochi app) (rnrs base) (srfi :64))

(define (false-handler) #f)

(test-begin "gzochi:execute-logged-in")

(test-group "#f"
  (test-eqv #f (gzochi:execute-logged-in (g:@ false-handler) 'session)))

(test-end "gzochi:execute-logged-in")

)
