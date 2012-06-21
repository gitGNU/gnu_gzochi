;; gzochi/private/debug.scm: Remote debugging interface for gzochid
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

(library (gzochi private debug)
  (export gzochi:run-repl-server)
  (import (guile)
	  (ice-9 optargs)
	  (ice-9 threads)
	  (rnrs base)
	  (rnrs eval)
	  (rnrs io simple)
	  (system repl repl))

  ;; The vast majority of the following code is borrowed from the 
  ;; `(system repl server)' module in GNU Guile.

  (define *open-sockets* '())
  (define sockets-lock (make-mutex))
  
  (define (close-socket! s)
    (with-mutex sockets-lock
      (set! *open-sockets* (delq! s *open-sockets*)))
    (close-port s))
  
  (define (add-open-socket! s)
    (with-mutex sockets-lock
      (set! *open-sockets* (cons s *open-sockets*))))
  
  (define (stop-server-and-clients!)
    (cond ((with-mutex sockets-lock 
             (and (pair? *open-sockets*) (car *open-sockets*)))
	   => (lambda (s)
		(close-socket! s)
		(stop-server-and-clients!)))))
  
  (define* (make-tcp-server-socket 
	    #:key 
	    (host #f)
	    (addr (if host (inet-aton host) INADDR_LOOPBACK))
	    (port 37146))
    (let ((sock (socket PF_INET SOCK_STREAM 0)))
      (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
      (bind sock AF_INET addr port)
      sock))

  (define call-with-sigint
    (if (not (provided? 'posix))
	(lambda (thunk) (thunk))
	(lambda (thunk)
	  (let ((handler #f))
	    (dynamic-wind
		(lambda ()
		  (set! handler
			(sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
		thunk
		(lambda ()
		  (if handler
		      (sigaction SIGINT (car handler) (cdr handler))
		      (sigaction SIGINT #f))))))))

  (define* (run-server #:optional (server-socket (make-tcp-server-socket)))
    (define (accept-new-client)
      (catch #t
	(lambda () (call-with-sigint (lambda () (accept server-socket))))
	(lambda (k . args)
	  (cond
	   ((port-closed? server-socket)  #f)
	   ((eq? k 'interrupt) (close-socket! server-socket) #f)
	   (else
	    (warn "Error accepting client" k args)
	    (sleep 1)
	    (accept-new-client))))))
  
    (sigaction SIGPIPE SIG_IGN)
    (add-open-socket! server-socket)
    (listen server-socket 5)
    (let lp ((client (accept-new-client)))
      (if client
	  (let ((client-socket (car client))
		(client-addr (cdr client)))
	    (add-open-socket! client-socket)
	    (make-thread serve-client client-socket client-addr)
	    (lp (accept-new-client))))))
 
  (define* (gzochi:run-repl-server #:key (port 37146))
    (run-server (make-tcp-server-socket #:port port)))
  
  (define (serve-client client addr)

    ;; Create the "sandbox" environment for connecting debugger users. It's not
    ;; really a true sandbox per se -- more of a private module environment 
    ;; with some helpful modules pre-included.

    (define client-environment
      (environment '(guile) '(gzochi) '(gzochi admin) '(rnrs)))

    (with-continuation-barrier
     (lambda ()
       (with-input-from-port client
	 (lambda ()
	   (with-output-to-port client
	     (lambda ()
	       (with-error-to-port client
		  (lambda ()
		    (with-fluids ((*repl-stack* '()))
		      (set-current-module client-environment)
		      (start-repl))))))))))

    (close-socket! client))
)

