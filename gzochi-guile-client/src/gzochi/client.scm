;; gzochi/client.scm: The Scheme gzochi reference client library
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

(library (gzochi client)
  (export gzochi:client?
	  gzochi:connect
	  gzochi:disconnect
	  gzochi:pump-events
	  gzochi:send

	  gzochi:set-received-message-callback!
	  gzochi:set-disconnected-callback!)

  (import (guile)
	  (gzochi client protocol)
	  (gzochi main-loop)
	  (ice-9 threads)
	  (rnrs)
	  (srfi :18))

;; This is a reference implementation of a client for the gzochi game
;; development frameework. It handles the connection lifecycle and network
;; communication for the client side of your game application. Use the client's
;; API to send messages to the server and register callbacks for messages sent
;; from the server to the client. This implementation is designed to take
;; advantage of the asynchronous I/O capabilities provided by most modern
;; operating systems; add a connected client to a main loop created with the
;; routines in (gzochi main-loop) or use this library's gzochi:pump-events
;; procedure to be notified of new messages as soon as they arrive.
;;
;; The following examples demonstrate both models of operation. 
;;
;; The gzochi:client record type is a subtype of gzochi:source in 
;; (gzochi main-loop) so it can participate in the iteration of the main loop
;; alongside other input sources:
;;
;; (let* ((client (gzochi:connect server-addr "mygame" (string->utf8 "passwd")))
;;        (console 
;;	   (gzochi:make-source #f
;;	    #:prepare (lambda (source) (char-ready?))
;;	    #:check (lambda (source) (char-ready?))
;;	    #:dispatch (lambda (source) 
;;			 (gzochi:client-send 
;;			  client (string->utf8 
;;				  (drain-input (current-input-port))))))))
;;
;;   (gzochi:set-client-received-message!
;;    client (lambda (msg) (display (utf8->string msg)) (newline)))
;;
;;   (gzochi:main-loop-add-source! client)
;;   (gzochi:main-loop-add-source! console)
;;     (gzochi:main-loop-run))
;;
;;
;; If your client application is naturally multi-threaded or if the client is
;; the only source that should be polled for events, you can use the
;; gzochi:pump-events procedure to simulate the main loop's iteration over the
;; client in isolation.
;;
;; (let ((client (gzochi:connect server-addr "mygame" (string->utf8 "passwd"))))
;;   (gzochi:set-client-received-message!
;;    client (lambda (msg) 
;;	       (if (equal? (utf->string msg) "PING")
;;		   (gzochi:send client (string->utf8 "PONG")))))
;;   (gzochi:pump-events client))

  (define chunk-size 512)
  (define max-buffer-size 65538)

;; Record type definition for gzochi:client.
;;
;; To create a new connected client, see gzochi:connect below.

  (define-record-type (gzochi:client gzochi:make-client gzochi:client?)
    (parent gzochi:source)
    (fields (mutable received-message) 
	    (mutable disconnected)
	    (mutable connected)
	    (mutable disconnect-acknowledged)
	    buffer
	    (mutable buffer-offset)
	    send-mutex)

    (protocol (lambda (n)
		(lambda (socket)
		  (let ((p (n (gzochi:make-selector 
			       socket (gzochi:make-selector-event-set 
				       read error))

			      #:prepare prepare
			      #:check check
			      #:dispatch dispatch-all)))
		    (p #f #f #t #f (make-bytevector max-buffer-size) 0
		       (make-mutex)))))))
  
  (define (read-short bv off) (bytevector-u16-ref bv off (endianness big)))

  (define (protocol-read client)
    (let* ((buffer (gzochi:client-buffer client))
	   (offset (gzochi:client-buffer-offset client))
	   (chunk (make-bytevector chunk-size))
	   (available-buffer (- max-buffer-size offset))
	   (bytes-requested (min chunk-size available-buffer))
	   (bytes-read (recv! (gzochi:selector-port/fd 
			       (gzochi:source-selector client))
			      chunk
			      0)))

      (if (> bytes-read 0)
	  (begin
	    (bytevector-copy! chunk 0 buffer offset bytes-read)
	    (gzochi:client-buffer-offset-set! client (+ offset bytes-read))))
      bytes-read))

  (define (bytevector-shift! bv from to len fill)
    (bytevector-copy! bv from bv to len)
    (if (> len 1)
	(let* ((diff (abs (- from to))) 
	       (flen (min diff len)))
	  (bytevector-copy! 
	   (make-bytevector flen) 0 bv 
	   (if (< from to) from (- (+ from len) flen)) flen))
	(if (not (eqv? from to))
	    (begin
	      (bytevector-u8-set! bv to (bytevector-u8-ref bv from))
	      (bytevector-u8-set! bv from fill)))))

  (define (dispatch-session-message client message)
    (let ((received-message (gzochi:client-received-message client)))
      (and received-message (received-message message))))

  (define (dispatch-disconnect client)
    (gzochi:client-connected-set! client #f)
    (if (not (gzochi:client-disconnect-acknowledged client))
	(begin 
	  (gzochi:client-disconnect-acknowledged-set! client #t)
	  (let ((disconnected (gzochi:client-disconnected client)))
	    (and disconnected (disconnected))))))

  (define* (attempt-dispatch client #:optional all?)
    (define (dispatch-inner opcode message)
      (cond ((eqv? opcode gzochi:protocol/login-success))
	    ((eqv? opcode gzochi:protocol/session-message)
	     (dispatch-session-message client message))
	    ((or (eqv? opcode gzochi:protocol/login-failure)
		 (eqv? opcode gzochi:protocol/session-disconnected))
	     (dispatch-disconnect client))))

    (let ((buffer (gzochi:client-buffer client)))
      (let loop ((continue? #t) (offset 0) (dispatched 0))
	(let ((buffer-offset (gzochi:client-buffer-offset client)))
	  (cond ((not continue?)
		 (if (> offset 0)
		     (if (< offset buffer-offset)
			 (let ((len (- buffer-offset offset)))
			   (bytevector-shift! buffer offset 0 len 0)
			   (gzochi:client-buffer-offset-set! client len))
			 (begin
			   (bytevector-copy! 
			    (make-bytevector offset 0) 0 buffer 0 offset)
			   (gzochi:client-buffer-offset-set! client 0))))
		 (> dispatched 0))
		((not (gzochi:client-connected client))
		 (dispatch-disconnect client) (loop #f offset dispatched))
		((< (- buffer-offset offset) 3) (loop #f offset dispatched))
		(else (let ((len (read-short buffer offset)))
			(if (>= (- buffer-offset offset) (+ len 3))
			    (let ((op (bytevector-u8-ref buffer (+ offset 2)))
				  (message (make-bytevector len)))
			      (bytevector-copy! 
			       buffer (+ offset 3) message 0 len)
			      (dispatch-inner op message)
			      (loop all? (+ offset 3 len) (+ dispatched 1)))
			    (loop #f offset dispatched)))))))))

  (define (dispatch client) (attempt-dispatch client))
  
  (define (send-protocol-message client opcode message)
    (let* ((message-len (bytevector-length message))
	   (bv-len (+ message-len 3))
	   (bv (make-bytevector bv-len)))
      (bytevector-u8-set!
       bv 0 (bitwise-arithmetic-shift-right (bitwise-and message-len #xff00) 8))
      (bytevector-u8-set! bv 1 (bitwise-and message-len #xff))
      (bytevector-u8-set! bv 2 opcode)
      (bytevector-copy! message 0 bv 3 message-len)

      (let ((sock (gzochi:selector-port/fd (gzochi:source-selector client))))
	(let send-fully ((bv bv) (len bv-len))
	  (select '() (list sock) (list sock))
	  (let ((bytes-sent (send sock bv)))
	    (cond ((< bytes-sent 0))
		  ((not (eqv? bytes-sent len))
		   (let* ((new-bv-len (- len bytes-sent))
			  (new-bv (make-bytevector new-bv-len)))
		     (bytevector-copy! bv bytes-sent new-bv 0 new-bv-len)
		     (send-fully new-bv new-bv-len)))))))))

  (define (send-login-request client endpoint credentials)
    (let* ((endpoint-len (string-length endpoint))
	   (credentials-len (bytevector-length credentials))
	   (buffer (make-bytevector (+ endpoint-len credentials-len 1))))
      (bytevector-copy! (string->utf8 endpoint) 0 buffer 0 endpoint-len)
      (bytevector-copy! credentials 0 buffer (+ endpoint-len 1) credentials-len)
      (send-protocol-message client gzochi:protocol/login-request buffer)))

  (define (send-disconnect client)
    (send-protocol-message 
     client gzochi:protocol/logout-request (make-bytevector 0)))

  (define (send-session-message client message)
    (send-protocol-message client gzochi:protocol/session-message message))

  (define (check-argument predicate argument optional?)
    (or (and optional? (not argument))
	(predicate argument)
	(raise (condition (make-assertion-violation) 
			  (make-irritants-condition argument)))))

;; Creates a new client connection to the specified game application endpoint at
;; the specified address.
;;
;; The addr argument must be a socket address as created by make-socket-address.
;; The endpoint argument is a string that names a game application endpoint on 
;; the target gzochi server. The credentials argument is a bytevector that is
;; passed uninterpreted to the authentication plugin configured for the target
;; game.
;; 
;; This procedure returns a new gzochi:client record if connection and
;; authentication are successful, #f otherwise.

  (define (gzochi:connect addr endpoint credentials)
    (define TCP_NODELAY #x01)

    (check-argument vector? addr #f)
    (check-argument string? endpoint #f)
    (check-argument bytevector? credentials #f)

    (let ((sock (socket PF_INET SOCK_STREAM 0)))
      (setsockopt sock IPPROTO_TCP TCP_NODELAY 1)
      (connect sock addr)

      (let ((client (gzochi:make-client sock)))
	(send-login-request client endpoint credentials)	

	(let loop ()
	  (cond ((dispatch client) #t)
		((< (protocol-read client) 0)
		 (gzochi:client-connected-set! client #f)
		 (let ((disconnected (gzochi:client-disconnected client)))
		   (and disconnected (disconnected client))))
		(else (loop))))

	(and (gzochi:client-connected client) 
	     (begin
	       (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK))
	       client)))))

;; Sets the "received message" callback on the specified gzochi:client instance.
;; 
;; The callback should be a procedure taking a single bytevetor argument, and 
;; will be called when the client has received a complete message from the
;; server.

  (define (gzochi:set-received-message-callback! client callback)
    (check-argument gzochi:client? client #f)
    (check-argument procedure? callback #f)

    (gzochi:client-received-message-set! client callback))
  
;; Sets the "disconnected" callback on the specified gzochi:client instance.
;;
;; The callback should be a procedure taking zero arguments, and will be called
;; when the client has been disconnected from the server.
  
  (define (gzochi:set-disconnected-callback! client callback)
    (check-argument gzochi:client? client #f)
    (check-argument procedure? callback #f)

    (gzochi:client-disconnected-set! client callback))

;; Disconnects the specified client. The disconnected callback procedure will be
;; called if it is set.

  (define (gzochi:disconnect client)
    (check-argument gzochi:client? client #f)

    (shutdown (gzochi:selector-port/fd (gzochi:source-selector client)) 2)
    (dispatch-disconnect client))

;; Sends the specified message to the server to which the specified client is
;; connected.
;;
;; The message argument must be a bytevector of at most 65535 bytes.

  (define (gzochi:send client msg)
    (check-argument gzochi:client? client #f)
    (check-argument bytevector? msg #f)

    (with-mutex (gzochi:client-send-mutex client)
      (send-session-message client msg) #t))

  (define (dispatchable? client)
    (let ((offset (gzochi:client-buffer-offset client)))
      (cond ((and (not (gzochi:client-connected client))
		  (not (gzochi:client-disconnect-acknowledged client)))
	     #t)
	    ((< offset 3) #f)
	    (else (>= offset (+ (read-short (gzochi:client-buffer client) 0) 
				3))))))

  (define (prepare client) (dispatchable? client))

  (define (check client)
    (or (dispatchable? client)
	(let* ((selector (gzochi:source-selector client))
	       (events (gzochi:selector-revents selector)))
	  (cond ((enum-set-member? 'error events) #t)
		((enum-set-member? 'read events) 
		 (protocol-read client) (dispatchable? client))	 
		(else #f)))))

  (define (dispatch-all client) (attempt-dispatch client #t))

  (define (nonnegative-integer? x) (and (integer? x) (>= x 0)))
  (define (normalize-timeout sec usec)
    (and sec (if usec
		 (let-values (((d m) (div-and-mod usec 1000000)))
		   (cons (+ sec d) m))
		 (cons sec 0))))

  (define (timeoutsub t1 t2)
    (normalize-timeout 0 (- (+ (* (car t1) 1000000) (cdr t1))
			    (+ (* (car t2) 1000000) (cdr t2)))))

;; Waits for messages to arrive from the server to which the specified client is
;; attached, or for the client to become disconnected from the server. The 
;; received message and disconnected callback procedures will be called as
;; appropriate until this procedure returns. An optional timeout may be 
;; specified as [secs [usecs]] and gives the maximum time this procedure should
;; wait for and dispatch events.

  (define (gzochi:pump-events client . timeout)
    (define (time-left? t)
      (or (not t) (> (car t) 0) (and (zero? (car t)) (> (cdr t) 0))))

    (let* ((sec (if (null? timeout) #f (car timeout)))
	   (usec (and sec (if (null? (cdr timeout)) #f (cadr timeout)))))

      (check-argument gzochi:client? client #f)
      (check-argument nonnegative-integer? sec #t)
      (check-argument nonnegative-integer? usec #t)
      
      (let* ((selector (list (gzochi:source-selector client))))
	(let loop ((remaining (normalize-timeout sec usec)))
	  (let ((now (gettimeofday)))
	    (and (time-left? remaining)
		 (begin
		   (gzochi:selector-clear-events! (car selector))
		   (if (not (prepare client))
		       (if remaining
			   (gzochi:poll 
			    selector (car remaining) (cdr remaining))
			   (gzochi:poll selector)))
		   (if (check client)
		       (dispatch-all client))
		   (loop (let ((elapsed (timeoutsub (gettimeofday) now)))
			   (timeoutsub remaining elapsed))))))))))
)
