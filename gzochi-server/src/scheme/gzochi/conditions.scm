;; gzochi/conditions.scm: Public exports for gzochi condition types 
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

(library (gzochi conditions)
  (export &gzochi:name-exists
	  gzochi:make-name-exists-condition
	  gzochi:name-exists-condition-name
	  gzochi:name-exists-condition?

          &gzochi:name-not-bound
	  gzochi:make-name-not-bound-condition
	  gzochi:name-not-bound-condition-name
	  gzochi:name-not-bound-condition?

	  &gzochi:object-removed
	  gzochi:make-object-removed-condition
	  gzochi:object-removed-condition?
	  
	  &gzochi:no-current-application
	  gzochi:make-no-current-application-condition
	  gzochi:no-current-application-condition?

	  &gzochi:transaction-aborted
	  gzochi:make-transaction-aborted-condition
	  gzochi:transaction-aborted-condition?

	  &gzochi:transaction-timeout
	  gzochi:make-transaction-timeout-condition
	  gzochi:transaction-timeout-condition?)
	  
  (import (rnrs base)
	  (rnrs conditions)
	  (rnrs records syntactic))

  (define-condition-type &gzochi:name-exists 
                         &condition
			 gzochi:make-name-exists-condition
			 gzochi:name-exists-condition?
    (name gzochi:name-exists-condition-name))
  
  (define-condition-type &gzochi:name-not-bound
                         &condition
                         gzochi:make-name-not-bound-condition
			 gzochi:name-not-bound-condition?
    (name gzochi:name-not-bound-condition-name))
  
  (define-condition-type &gzochi:object-removed
                         &condition
			 gzochi:make-object-removed-condition
			 gzochi:object-removed-condition?)
  
  (define-condition-type &gzochi:no-current-application
                         &condition
			 gzochi:make-no-current-application-condition
			 gzochi:no-current-application-condition?)
  
  (define-condition-type &gzochi:transaction-aborted
                         &condition
			 gzochi:make-transaction-aborted-condition
			 gzochi:transaction-aborted-condition?)
  
  (define-condition-type &gzochi:transaction-timeout
                         &condition
			 gzochi:make-transaction-timeout-condition
			 gzochi:transaction-timeout-condition?)
)
