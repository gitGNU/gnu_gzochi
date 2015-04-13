;; tx.scm --- Server-side implementation of gzochi External Transaction Example

;; Copyright (C) 2015 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

#!r6rs

(library (gzochi example tx)
  (export disconnected initialized logged-in ready received-message)
  (import (dbi dbi)
	  (guile)
	  (gzochi app)
	  (gzochi session)
	  (gzochi tx)
	  (rnrs))

  ;; The values of the "database-backend" and "connection-string" properties
  ;; passed to the `ready' callback.

  (define database-backend #f)
  (define connection-string #f)

  ;; Use Guile's fluid functionality to create a simple connection pool, such
  ;; that each dynamic state is allocated its own database handle. The 
  ;; implementation makes no attempts to reconnect on error.

  (define database-handle-fluid (make-unbound-fluid))

  (define (database-handle)
    (if (not (fluid-bound? database-handle-fluid))
	(let* ((handle (dbi-open database-backend connection-string))
	       (status (and handle (dbi-get_status handle))))
	  (cond ((not status) 
		 (raise (condition (make-assertion-violation)
				   (make-message-condition
				    "Failed to connect to database."))))
		((not (eqv? (car status) 0))
		 (raise (condition (make-assertion-violation)
				   (make-message-condition (cdr status)))))
		(else (fluid-set! database-handle-fluid handle)))))
    (fluid-ref database-handle-fluid))

  ;; The following functions implement the transaction lifecycle functions for 
  ;; the supported database backends, all of which support them slightly 
  ;; differently.

  (define (begin-transaction db-handle)
    (cond ((equal? database-backend "mysql") 
	   (dbi-query db-handle "START TRANSACTION"))
	  ((equal? database-backend "postgresql")
	   (dbi-query db-handle "BEGIN TRANSACTION")

	   ;; The PostgresQL driver requires that all rows from the result be 
	   ;; consumed before processing additional commands.

	   (dbi-get_row db-handle))
	  ((equal? database-backend "sqlite3")
	   (dbi-query db-handle "BEGIN TRANSACTION"))
	  (else (raise (condition
			(make-assertion-violation)
			(make-message-condition 
			 "Unsupported database backend."))))))

  (define (commit-transaction db-handle)
    (if (or (equal? database-backend "mysql") 
	    (equal? database-backend "postgresql") 
	    (equal? database-backend "sqlite3"))
	(dbi-query db-handle "COMMIT")
	(raise (condition
		(make-assertion-violation)
		(make-message-condition "Unsupported database backend.")))))

  (define (rollback-transaction db-handle)
    (cond ((or (equal? database-backend "mysql") 
	       (equal? database-backend "sqlite3"))
	   (dbi-query db-handle "ROLLBACK"))
	  ((equal? database-backend "postgresql")
	   (dbi-query db-handle "ROLLBACK")
	   
	   ;; The PostgresQL driver requires that all rows from the result be 
	   ;; consumed before processing additional commands.

	   (dbi-get_row db-handle))
	  (else (raise (condition
			(make-assertion-violation)
			(make-message-condition 
			 "Unsupported database backend."))))))

  ;; Create the transaction participant structure, which registers the lifecycle
  ;; functions with the gzochid container's transaction management system. The
  ;; `prepare' callback is stubbed to `#t' for this implementation because the
  ;; there isn't no consistent support for it across Guile DBI's database 
  ;; backends.

  (define (make-transaction-participant db-handle)
    (gzochi:make-transaction-participant
     (lambda () #t) ;; Prepare
     (lambda () (commit-transaction db-handle)) ;; Commit 
     (lambda () (rollback-transaction db-handle)))) ;; Rollback

  (define (join-transaction db-handle)
    ;; Join the current gzochid container transaction, providing a transaction
    ;; participant to handle transacftion lifecycle events.

    (gzochi:join-transaction (make-transaction-participant db-handle))
    (begin-transaction db-handle))

  ;; The `ready' lifecycle callback handler. Called by the gzochid container
  ;; whenever it launches this application.

  (define (ready properties)
    (set! database-backend (hashtable-ref properties "database-backend" #f))
    (set! connection-string (hashtable-ref properties "connection-string" #f)))

  ;; The `initialized' lifecycle callback handler. This is a no-op for this
  ;; application.
  
  (define (initialized properties) *unspecified*)

  ;; Transactionally update the message counter by SELECTing the current value
  ;; and increment it via an UPDATE.
  ;;
  ;; Note that a transaction is initiated but not committed by this function;
  ;; the changes to the external database won't be permanent until the entire
  ;; application transaction is complete.

  (define (update-message-count! name)
    (define select "SELECT message_count FROM user_stats WHERE name = '~A'")
    (define update "UPDATE user_stats SET message_count = ~A WHERE name = '~A'")

    (let ((db-handle (database-handle)))
      (join-transaction db-handle)

      (dbi-query db-handle (simple-format #f select name))
      (let* ((message-count-row (dbi-get_row db-handle))
	     (message-count (+ (cdar message-count-row) 1)))
	
	;; Consume the remainder of the row set.
	
	(dbi-get_row db-handle)
	(dbi-query db-handle (simple-format #f update message-count name))
	(dbi-get_row db-handle))))

  ;; The `received-message' callback handler. Called by the gzochid container
  ;; when a complete message has been received by a connected session.

  (define (received-message message session)
    (update-message-count! (gzochi:client-session-name session))
    (gzochi:send-message session message))

  ;; The `disconnected' callback handler. This is a no-op for this application.

  (define (disconnected session) *unspecified*)

  ;; Transactionally update the login counter by SELECTing the current value
  ;; and increment it via an UPDATE to the current row or an INSERT of a new row
  ;; if this user hasn't logged in before.
  ;;
  ;; Note that a transaction is initiated but not committed by this function;
  ;; the changes to the external database won't be permanent until the entire
  ;; application transaction is complete.

  (define (update-login-count! name)
    (define insert (string-append "INSERT INTO user_stats "
				  "(name, login_count, message_count) "
				  "VALUES ('~A', 1, 0)"))
    (define select "SELECT login_count FROM user_stats WHERE name = '~A'")
    (define update "UPDATE user_stats SET login_count = ~A WHERE name = '~A'")

    (let ((db-handle (database-handle)))
      (join-transaction db-handle)
      
      (dbi-query db-handle (simple-format #f select name))
      (let ((login-count-row (dbi-get_row db-handle)))

	;; Consume the remainder of the row set.

	(dbi-get_row db-handle)
	(if login-count-row
	    (let ((login-count (+ (cdar login-count-row) 1)))
	      (dbi-query db-handle (simple-format #f update login-count name)))
	    (dbi-query db-handle (simple-format #f insert name)))
	
	(dbi-get_row db-handle))))

  ;; The `logged-in' lifecycle callback handler. Called when a user
  ;; successfully authenticates with the application. This implementation 
  ;; updates the user statistics before returning a new session listener for the
  ;; connected client.

  (define (logged-in session)
    (update-login-count! (gzochi:client-session-name session))

    (gzochi:make-client-session-listener
     (g:@ received-message session) (g:@ disconnected session)))
)
