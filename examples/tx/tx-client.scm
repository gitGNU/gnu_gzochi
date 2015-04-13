;; tx-client.scm --- Console client for gzochi External Transaction example

;; Copyright (C) 2015 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

(import (gzochi client))
(import (gzochi main-loop))
(import (ice-9 popen))
(import (ice-9 rdelim))
(import (ice-9 receive))
(import (rnrs bytevectors))
(import (rnrs conditions))
(import (rnrs exceptions))

;; The following is a simple client application to exercise the external
;; transaction example code within the gzochid container (see the `server' 
;; subdirectory in the directory containing this file). The client reads input
;; from the console, sends it to the server, and prints any messages from the
;; server back to the console.



;; Connect to the gzochid container at the specified socket address and 
;; establish a connection to the "tx-example" application using the specified
;; identity string.
;;
;; Returns a connected `gzochi:client' structure on success. On failure, causes
;; the process to exit.

(define (connect-client addr identity)
  (display "Connecting... ")
  (let ((client (gzochi:connect addr "tx-example" (string->utf8 identity))))
    (if client
	(begin
	  (display "OK") (newline)
	  client)
	(begin 
	  (display "failed.") (newline)
	  (exit 1)))))

;; Construct a new `gzochi:source' suitable for use with an instance of
;; `gzochi:main-loop' using the specified `gzochi:client' structure. The source
;; accepts line-oriented input from stdin and dispatches it to the server by
;; calling `gzochi:send'.

(define (create-console client)
  (or (gzochi:client? client) (raise (make-assertion-violation)))

  (let ((stdin (fdes->inport 0)))

    (gzochi:make-source 

     ;; Creating the source with a selector allows the main loop to use the
     ;; operating system to poll for input efficiently.

     (gzochi:make-selector 0 (gzochi:make-selector-event-set read))

     ;; stdin is usually line-buffered, so the `prepare' and `check' callbacks 
     ;; will only return `#t' once the user presses `enter'.

      #:prepare (lambda (source) (char-ready? stdin))
      #:check (lambda (source) (char-ready? stdin))
      #:dispatch (lambda (source)
		   (gzochi:send client (string->utf8 (read-line stdin)))))))

(define (main args)

  ;; If incorrect arguments passed, display usage info and exit.

  (or (eqv? (length args) 3)
      (begin
	(display args)
	(simple-format #t "Usage: ~A [hostname]:[port] [username]" (car args))
	(newline)
	(exit 1)))

  (let* ((hostname-port-str (list-ref args 1))
	 (username (list-ref args 2)))
    (receive (hostname port-str)

      ;; Split the [hostname]:[port] argument and bind it to the variables 
      ;; above.

      (apply values (string-split hostname-port-str #\:))

      ;; Parse the arguments and use them to bootstrap the client connection.
      
      (let* ((addrinfo (car (getaddrinfo hostname)))
	     (port (string->number port-str))
	     (addr (addrinfo:addr addrinfo))
	     (sockaddr (make-socket-address AF_INET (sockaddr:addr addr) port))
	     (client (connect-client sockaddr username))
	     (console (create-console client))
	     (main-loop (gzochi:make-main-loop)))

	;; Install `received-message' and `disconnected' callbacks.

	(gzochi:set-received-message-callback!
	 client (lambda (msg)

		  ;; Display the message from the server.

		  (simple-format #t "Received: ~A" (utf8->string msg)) 
		  (newline)))

	(gzochi:set-disconnected-callback!
	 client (lambda () 
		  (display "Disconnected.") (newline)

		  ;; Halt the main loop if disconnected from the server. (This
		  ;; will have the effect of exiting the client application.)

		  (gzochi:main-loop-stop main-loop)))

	;; Register the client and console as sources of events with the main
	;; loop...
	
	(gzochi:main-loop-add-source! main-loop client)
	(gzochi:main-loop-add-source! main-loop console)

	;; ...and start the application.

	(gzochi:main-loop-run main-loop)))))
