;; gzochi/test-main-loop.scm: Scheme unit tests for gzochi client main loop
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

(import (gzochi main-loop))
(import (gzochi srfi-64-support))
(import (rnrs))
(import (srfi :18))
(import (srfi :64))

(test-begin "gzochi:main-loop")

(define-record-type verifiable-source
  (fields main-loop
	  (mutable prepared)
	  (mutable checked)
	  (mutable dispatched))
  (parent gzochi:source)
  (protocol (lambda (p)
	      (lambda (main-loop) 
		(let ((n (p #f
			    #:prepare prepare-true
			    #:check check-true
			    #:dispatch verifiable-dispatch-stop)))
		  (n main-loop #f #f #f))))))

(define-record-type reading-source
  (fields main-loop
	  (mutable prepared)
	  (mutable checked)
	  (mutable dispatched))
  (parent gzochi:source)
  (protocol (lambda (p)
	      (lambda (selector main-loop) 
		(let ((n (p selector 
			    #:prepare prepare-false
			    #:check check-read
			    #:dispatch reading-dispatch-stop)))
		  (n main-loop #f #f #f))))))

(define (prepare-true source) (verifiable-source-prepared-set! source #t) #t)
(define (prepare-false source) (reading-source-prepared-set! source #t) #f)

(define (check-true source) (verifiable-source-checked-set! source #t) #t)
(define (check-read source)
  (reading-source-checked-set! source #t)
  (enum-set-member? 
   (gzochi:selector-event read) 
   (gzochi:selector-events (gzochi:source-selector source))))

(define (verifiable-dispatch-stop source) 
  (verifiable-source-dispatched-set! source #t)
  (gzochi:main-loop-stop (verifiable-source-main-loop source)))
(define (reading-dispatch-stop source) 
  (reading-source-dispatched-set! source #t)
  (gzochi:main-loop-stop (reading-source-main-loop source)))

(test-group "run"
  (let* ((main-loop (gzochi:make-main-loop))
	 (test-source (make-verifiable-source main-loop)))
    (gzochi:main-loop-add-source! main-loop test-source)

    (let ((t (make-thread (lambda () (gzochi:main-loop-run main-loop) #t))))
      (thread-start! t)
      (let ((now (current-time)))
	(or (thread-join! t (cons (+ (car now) 1) (cdr now)) #f)
	    (thread-terminate! t))

	(test-assert (verifiable-source-prepared test-source))
	(test-assert (verifiable-source-checked test-source))
	(test-assert (verifiable-source-dispatched test-source)))))

  (let* ((main-loop (gzochi:make-main-loop))
	 (socks (socketpair PF_UNIX SOCK_STREAM 0))
	 (test-source (make-reading-source 
		       (gzochi:make-selector 
			(car socks) (gzochi:make-selector-event-set read))
		       main-loop)))
    (gzochi:main-loop-add-source! main-loop test-source)

    (write "ready" (cdr socks))

    (let ((t (make-thread (lambda () (gzochi:main-loop-run main-loop) #t))))
      (thread-start! t)
      (let ((now (current-time)))
	(or (thread-join! t (cons (+ (car now) 1) (cdr now)) #f)
	    (thread-terminate! t))

	(test-assert (reading-source-prepared test-source))
	(test-assert (reading-source-checked test-source))
	(test-assert (reading-source-dispatched test-source))))))

(test-end "gzochi:main-loop")
