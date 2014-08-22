;; gzochi/srfi-64-support.scm: Custom SRFI-64 test runner for `make check'
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

(library (gzochi srfi-64-support)
  (export gzochi:test-runner)
  (import (rnrs) (srfi :64))

  (define (gzochi:test-runner) 
    (let ((runner (test-runner-simple)))
      (test-runner-on-final! runner
        (lambda (runner)
	  (test-on-final-simple runner)
	  (if (> (test-runner-fail-count runner)
		 (test-runner-xfail-count runner))
	      (raise (condition
		      (make-assertion-violation)
		      (make-message-condition "There were test failures."))))))
      runner))
)
