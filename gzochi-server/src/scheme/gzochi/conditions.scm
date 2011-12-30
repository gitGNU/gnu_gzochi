;; gzochi/conditions.scm: Public exports for gzochi condition types 
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

(library (gzochi conditions)
  (export &gzochi:name-exists
	  gzochi:make-name-exists-condition
	  gzochi:name-exists-condition?

          &gzochi:name-not-bound
	  gzochi:make-name-not-bound-condition
	  gzochi:name-not-bound-condition?)
	  
  (import (rnrs base)
	  (rnrs conditions)
	  (rnrs records syntactic))

  (define-record-type (&gzochi:name-exists 
		       gzochi:make-name-exists-condition
		       gzochi:name-exists-condition?)
    (fields name)
    (parent &condition))

  (define-record-type (&gzochi:name-not-bound
		       gzochi:make-name-not-bound-condition
		       gzochi:name-not-bound-condition?)
    (fields name)
    (parent &condition))
)