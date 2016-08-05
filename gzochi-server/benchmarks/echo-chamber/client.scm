;; echo-chamber/client.scm --- Echo chamber benchmark, client side
;; Copyright (C) 2016 Julian Graham
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

(import (gzochi client))
(import (gzochi main-loop))
(import (ice-9 format))
(import (ice-9 receive))
(import (rnrs))

;; The echo chamber benchmark client application creates a list of client
;; connections, and has each client send PINGs to the benchmark server
;; application as well as respond with a PONG to any forwarded PINGs it receives
;; from the server.
;;
;; The client application is single-threaded; it uses the functionality in
;; `(gzochi main-loop)' to drive the read and write behavior of the clients,
;; polling them for I/O readiness as necessary.

;; The benchmark record type definition. Defines fields for holding the main
;; loop and different client lists, as well as the global configuration for the
;; benchmark process.

(define-record-type benchmark
  (fields total-clients ;; total client count
	  messages-per-client ;; PINGs per client
	  interval ;; the number of milliseconds to wait between PINGs
	  main-loop ;; the main loop
	  
	  (mutable clients) ;; the global client list
	  (mutable finished-clients) ;; clients that have sent all their PINGs
	  (mutable ready-clients)) ;; clients waiting to send their next PING

  (protocol (lambda (p)
	      (lambda (total-clients messages-per-client interval)
		(p total-clients messages-per-client interval
		   (gzochi:make-main-loop) '() '() '())))))

;; The benchmark client record type definition. Defines fields for holding
;; per-client state.

(define-record-type client
  (fields benchmark ;; a reference to the benchmark
	  session ;; the gzochi client session
	  id ;; the client id
	  
	  (mutable messages-sent) ;; number of PINGs sent
	  (mutable messages-received) ;; number of PINGs and PONGs received
	  (mutable last-sent-time) ;; the timestamp of the last PING
	  (mutable min-response-time) ;; the min PONG rtt
	  (mutable max-response-time) ;; the max PONG rtt
	  (mutable avg-response-time) ;; the average PONG rtt

	  ;; the number of PONGs outstanding for the current PING

	  (mutable outstanding-pongs)) 

  (protocol (lambda (p)
	      (lambda (benchmark session id)
		(p benchmark session id 0 0 (cons 0 0) #f #f (cons 0 0) 0)))))

;; Some Scheme implementations of the C timer* family of functions defined in
;; `<sys/time.h>', using Guile's version of a timeval, a pair of seconds and
;; microseconds.

;; Returns a new timeval pair in which the `cdr' falls within the range 0 and
;; 100000 (exclusive) and the `car' is updated accordingly.

(define (normalize-timeval t)
  (let ((s (car t)) (us (cdr t)))
    (if (> us 99999) (cons (+ s 1) (- us 1000000)) (cons s us))))

;; Returns -1 if the time represented by the first timeval pair argument is
;; before the second; 1 if the first is after the second; 0 if the arguments
;; represent the same instant.

(define (timercmp t1 t2)
  (cond ((< (car t1) (car t2)) -1)
	((> (car t1) (car t2)) 1)
	((< (cdr t1) (cdr t2)) -1)
	((> (cdr t1) (cdr t2)) 1)
	(else 0)))

;; Returns the normalized result of subtracting the second timeval pair argument
;; from the first. Raises a condition if the second timeval is greater than the
;; first.

(define (timersub t1 t2)
  (or (> (timercmp t1 t2) 0)
      (raise (make-assertion-violation)))
  
  (let ((t3s (- (car t1) (car t2)))
	(t3us (- (cdr t1) (cdr t2))))
    (normalize-timeval (cons t3s t3us))))

;; Returns the normalized result of adding the two timeval pair arguments.

(define (timeradd t1 t2)
  (let ((t3s (+ (car t1) (car t2)))
	(t3us (+ (cdr t1) (cdr t2))))
    (normalize-timeval (cons t3s t3us))))

;; Converts the specified timeval to the number of microseconds since the epoch.

(define (timeval->us t) (+ (* (car t) 1000000) (cdr t)))

;; Converts the specified timeval to the number of milliseconds since the epoch.

(define (timeval->ms t) (exact->inexact (/ (timeval->us t) 1000)))

;; Returns a new timeval representing the same time as its argument, a number
;; giving some number of microseconds since the epoch.

(define (us->timeval us)
  (call-with-values (lambda () (truncate/ us 1000000)) cons))

;; Construct a "timeout" source for the main loop. This source performs its
;; prepare/check operation at least every [send interval] milliseconds, and
;; identifies clients on the "ready list" whose send interval has expired and
;; are now ready to send a new PING message.

(define (make-send-timeout-source benchmark)
  (define (can-send? client)
    (positive?
     (timercmp
      (timersub (gettimeofday) (client-last-sent-time client))
      (normalize-timeval (cons 0 (* (benchmark-interval benchmark) 1000))))))

  ;; Send a PING, and put the client into "read" mode, such that it will not
  ;; send further PINGs until the most recent one has been acknowledged with a
  ;; PONG from all clients.
  
  (define (send-ping client)
    (gzochi:send (client-session client)
		 (string->utf8 (simple-format #f "PING ~A" (client-id client))))

    (client-messages-sent-set! client (+ (client-messages-sent client) 1))
    (client-outstanding-pongs-set! client (benchmark-total-clients benchmark))
    (client-last-sent-time-set! client (gettimeofday)))

  (define (send-timeout-prepare/check source)
    (let ((ready-clients (benchmark-ready-clients benchmark))) 
      (and (not (null? ready-clients)) (can-send? (car ready-clients)))))

  (define (send-timeout-dispatch source)
    (let ((now (gettimeofday)))
      (receive (cool hot)

	;; Remove from the ready list all clients that have "cooled off" enough
	;; to send their next PING.
	       
        (partition can-send? (benchmark-ready-clients benchmark))
	(benchmark-ready-clients-set! benchmark hot)
	(for-each send-ping cool))))

  (gzochi:make-source
   #f (normalize-timeval (cons 0 (benchmark-interval benchmark)))

   #:prepare send-timeout-prepare/check
   #:check send-timeout-prepare/check
   #:dispatch send-timeout-dispatch))

;; Create a new client connection to the gzochid server listening at the
;; specified server address.

(define (connect hostname port num)
  (define addr-info (car (getaddrinfo hostname)))
  (define serv-addr
    (make-socket-address
     AF_INET (sockaddr:addr (addrinfo:addr addr-info)) port))
  
  (gzochi:connect serv-addr "echo-chamber" (string->utf8 (number->string num))))

(define (received-message msg client)

  ;; Handle a PING message by sending a PONG with the same client id.
  
  (define (handle-ping str)
    (gzochi:send (client-session client)
		 (string->utf8 (string-append "PONG " (substring str 5)))))

  ;; Handle a PONG by decrementing the number of outstanding PONGs and adjusting
  ;; the message response stats.
  ;;
  ;; If the number of outstanding PONGs reaches zero, then move the client to
  ;; either the ready-to-send client list (if there are more PINGs to send) or
  ;; the finished client list (if the client has already sent of all of its
  ;; PINGs).
  
  (define (handle-pong)
    (let* ((now (gettimeofday))
	   (rtt (timersub now (client-last-sent-time client))))
      
      (client-outstanding-pongs-set!
       client (- (client-outstanding-pongs client) 1))

      ;; Update delivery stats.
      
      (if (or (not (client-min-response-time client))
	      (< (timercmp rtt (client-min-response-time client)) 0))
	  (client-min-response-time-set! client rtt))
      (if (or (not (client-max-response-time client))
	      (> (timercmp rtt (client-max-response-time client)) 0))
	  (client-max-response-time-set! client rtt))
            
      (client-avg-response-time-set!
       client (us->timeval
	       (/ (timeval->us (timeradd (client-avg-response-time client) rtt))
		  (- (benchmark-total-clients (client-benchmark client))
		     (client-outstanding-pongs client))))))

    (if (eqv? (client-outstanding-pongs client) 0)
	(let ((benchmark (client-benchmark client)))
	  (if (>= (client-messages-sent client)
		  (benchmark-messages-per-client benchmark))

	      ;; If the client has already sent all of its messages, move it to
	      ;; the "finished" list.
	      
	      (begin
		(benchmark-finished-clients-set! benchmark
		 (cons client (benchmark-finished-clients benchmark)))

		;; Are all the clients finished? If so, stop the main loop.

		(if (eqv? (length (benchmark-finished-clients benchmark))
			  (benchmark-total-clients benchmark))
		    (gzochi:main-loop-stop (benchmark-main-loop benchmark))))

	      ;; Otherwise, add it to the "ready" list for polling by the
	      ;; timeout source.
	      
	      (benchmark-ready-clients-set!
	       benchmark (merge (benchmark-ready-clients benchmark)
				(list client)
				(lambda (a b)
				  (< (timercmp (client-last-sent-time a)
					       (client-last-sent-time b))
				     0))))))))

  (let ((str (utf8->string msg)))
    (client-messages-received-set!
     client (+ (client-messages-received client) 1))
    (cond ((string-prefix? "PING" str) (handle-ping str))
	  ((string-prefix? "PONG" str) (handle-pong))
	  (else (raise (make-assertion-violation))))))

;; Print to standard out a report of message delivery performance.

(define (report benchmark)
  (let ((clients (benchmark-clients benchmark)))
    (format #t "Messages sent: ~d\n"
      (apply + (map client-messages-sent clients)))
    (format #t "Messages received: ~d\n"
      (apply + (map client-messages-received clients)))
    
    (format #t "Min response ms: ~1,3f\n"
      (apply min (map (lambda (client)
			(timeval->ms (client-min-response-time client)))
		      clients)))
    (format #t "Max response ms: ~1,3f\n"
      (apply max (map (lambda (client)
			(timeval->ms (client-max-response-time client)))
		      clients)))

    ;; The average response time across all per-client average response times.
    
    (format #t "Average response ms: ~1,3f\n"
      (/ (timeval->ms
	  (fold-left timeradd '(0 . 0) (map client-avg-response-time clients)))
	 (benchmark-total-clients benchmark)))

    ;; The min response time across all per-client average respons times.
    
    (format #t "Min average response ms: ~1,3f\n"
      (apply min (map (lambda (client)
			(timeval->ms (client-avg-response-time client)))
		      clients)))

    ;; The min response time across all per-client average respons times.
    
    (format #t "Max average response ms: ~1,3f\n"
      (apply max (map (lambda (client)
			(timeval->ms (client-avg-response-time client)))
		      clients)))))

;; The entry point to the benchmark client. Intended to be invoked via the
;; `echo-chamber.sh' script.
;;
;; guile -e main -s client.scm [hostname] [port] [num-clients] \
;;    [messages-per-client] [send-interval-ms]

(define (main args)
  (let* ((hostname (cadr args))
	 (port (string->number (caddr args)))
	 (num-clients (string->number (cadddr args)))
	 (messages-per-client (string->number (list-ref args 4)))
	 (interval (string->number (list-ref args 5)))		

	 (benchmark (make-benchmark num-clients messages-per-client interval))
	 (main-loop (benchmark-main-loop benchmark)))

    (let loop ((num 0))
      (and (< num num-clients)
	   (let* ((session (connect hostname port num))
		  (client (make-client benchmark session num)))
	     
	     (gzochi:set-received-message-callback!
	      session (lambda (msg) (received-message msg client)))
	     (benchmark-clients-set!
	      benchmark (cons client (benchmark-clients benchmark)))

	     ;; Mark the new client immediately ready to send a PING.
	     
	     (benchmark-ready-clients-set!
	      benchmark (cons client (benchmark-ready-clients benchmark)))

	     ;; Add the client session source.
	     
	     (gzochi:main-loop-add-source! main-loop session)
	     (loop (+ num 1)))))

    ;; Add the send timeout source.
    
    (gzochi:main-loop-add-source!
     main-loop (make-send-timeout-source benchmark))

    ;; Run the loop until `gzochi:main-loop-stop' is called (see `handle-pong'
    ;; above).
    
    (gzochi:main-loop-run main-loop)

    ;; Report on the benchmark results.
    
    (report benchmark)))
