;; gzochi/test-client.scm: Scheme unit tests for gzochi client
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

(import (gzochi client))
(import (gzochi client protocol))
(import (gzochi srfi-64-support))
(import (rnrs))
(import (srfi :1))
(import (srfi :18))
(import (srfi :64))

(define login-success-response 
  (u8-list->bytevector (list #x00 #x00 gzochi:protocol/login-success)))
(define login-failure-response 
  (u8-list->bytevector (list #x00 #x00 gzochi:protocol/login-failure)))
(define session-message-1
  (u8-list->bytevector
   (list #x00 #x04 gzochi:protocol/session-message #x4d #x53 #x47 #x31)))
(define session-message-2
  (u8-list->bytevector
   (list #x00 #x04 gzochi:protocol/session-message #x4d #x53 #x47 #x32)))
(define session-message-3
  (u8-list->bytevector
   (list #x00 #x04 gzochi:protocol/session-message #x4d #x53 #x47 #x33)))

(define (bytevector-append . lst) 
  (u8-list->bytevector (apply append (map bytevector->u8-list lst))))

(define (bytevector-hash bv)
  (fold (lambda (e r) (+ (* 31 r) e)) 1 (bytevector->u8-list bv)))

(define-record-type test-server
  (fields socket buffer (mutable buffer-offset) response-table (mutable thread))
  (protocol (lambda (p) 
	      (lambda (response-table)
		(p (socket PF_INET SOCK_STREAM 0) (make-bytevector 100) 0 
		   response-table #f)))))

(define (try-respond server)
  (let ((buffer (test-server-buffer server))
	(offset (test-server-buffer-offset server)))
    (and (>= offset 3)
	 (let ((bv (make-bytevector offset))
	       (responses (test-server-response-table server)))
	   (bytevector-copy! buffer 0 bv 0 offset)
	   (hashtable-ref (test-server-response-table server) bv #f)))))

(define (send-fully sock bv)
  (let ((len (bytevector-length bv)))
    (let loop ((offset 0))
      (let* ((buf-len (- len offset))
	     (buf (make-bytevector buf-len)))
	(bytevector-copy! bv offset buf 0 buf-len)
	(select '() (list sock) '())
	(let ((bytes-sent (send sock buf)))
	  (if (and (>= bytes-sent 0) (< bytes-sent buf-len))
	      (loop (+ offset bytes-sent))))))))

(define (start-server server)
  (define (server-loop client)
    (define buf (make-bytevector 1024))
    (let ((server-buffer (test-server-buffer server)))
      (let loop ()
	(select (list client) '() '())
	(bytevector-fill! buf 0)
	(let ((bytes-read (recv! client buf))
	      (offset (test-server-buffer-offset server)))
	  (if (>= bytes-read 0)
	      (begin
		(bytevector-copy! buf 0 server-buffer offset bytes-read)
		(test-server-buffer-offset-set! server (+ offset bytes-read))
		(let ((response (try-respond server)))
		  (if response
		      (begin
			(send-fully client response)
			(bytevector-fill! server-buffer 0)
			(test-server-buffer-offset-set! server 0))))
		(loop)))))))

  (let ((sock (test-server-socket server)))
    (bind sock AF_INET INADDR_ANY 0)
    (listen sock 1)
    (let ((thread (make-thread (lambda () (server-loop (car (accept sock)))))))
      (test-server-thread-set! server thread)
      (thread-start! thread))))

(define (stop-server server)
  (shutdown (test-server-socket server) 2)
  (thread-terminate! (test-server-thread server)))

(test-begin "gzochi:client")

(define connect/request-1
  (u8-list->bytevector
   (list #x00 #x05 gzochi:protocol/login-request #x74 #x65 #x73 #x74 #x00)))

(test-group "connect"
  (let ((response-table (make-hashtable bytevector-hash bytevector=?)))
    (hashtable-set! response-table connect/request-1 login-success-response)
    (let ((server (make-test-server response-table)))
      (dynamic-wind
	  (lambda () (start-server server))
	  (lambda ()
	    (let* ((server-addr (getsockname (test-server-socket server)))
		   (t (make-thread
		       (lambda () 
			 (gzochi:connect server-addr "test" #vu8())))))
	      (thread-start! t)
	      (let* ((n (current-time))
		     (result (thread-join! t (cons (+ (car n) 1) (cdr n)) #f)))
		(or result (thread-terminate! t))
		(test-assert (gzochi:client? result)))))

	  (lambda () (stop-server server)))))

  (let ((response-table (make-hashtable bytevector-hash bytevector=?)))
    (hashtable-set! response-table connect/request-1 login-failure-response)
    (let ((server (make-test-server response-table)))
      (dynamic-wind
	  (lambda () (start-server server))
	  (lambda ()
	    (let* ((server-addr (getsockname (test-server-socket server)))
		   (t (make-thread
		       (lambda () 
			 (gzochi:connect server-addr "test" #vu8())))))
	      (thread-start! t)
	      (let* ((n (current-time))
		     (result (thread-join! 
			      t (cons (+ (car n) 1) (cdr n)) 'timeout)))
		(if (eq? result 'timeout) (thread-terminate! t))
		(test-assert (not result)))))
	  (lambda () (stop-server server))))))

(define pump-events/request-1 #vu8(#x4d #x53 #x47 #x32))

(test-group "pump-events"
  (let ((response-table (make-hashtable bytevector-hash bytevector=?)))
    (hashtable-set!
     response-table connect/request-1
     (bytevector-append login-success-response session-message-1))
    (hashtable-set! response-table session-message-2 session-message-3)
    (let ((server (make-test-server response-table)) (msg1 #f) (msg2 #f))
      (dynamic-wind
	  (lambda () (start-server server))
	  (lambda ()
	    (let* ((addr (getsockname (test-server-socket server)))
		   (t (make-thread
		       (lambda ()
			 (let ((c (gzochi:connect addr "test" #vu8())))
			   (gzochi:set-received-message-callback! 
			    c (lambda (msg) (set! msg1 (utf8->string msg))))
			   (gzochi:pump-events c 0 10000)
			   (gzochi:set-received-message-callback!
			    c (lambda (msg) (set! msg2 (utf8->string msg))))
			   (gzochi:send c pump-events/request-1)
			   (gzochi:pump-events c 0 10000)
			   
			   #t)))))

	      (thread-start! t)
	      (let* ((n (current-time)))
		(or (thread-join! t (cons (+ (car n) 1) (cdr n)) #f)
		    (thread-terminate! t))

		(test-assert (equal? msg1 "MSG1"))
		(test-assert (equal? msg2 "MSG3")))))
	  (lambda () (stop-server server))))))

(test-end "gzochi:client")

