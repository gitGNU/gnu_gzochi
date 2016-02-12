;; gzochi/private/app.scm: Private infrastructure for application support
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

(library (gzochi private app)
  (export gzochi:execute-disconnected
	  gzochi:execute-initialized
	  gzochi:execute-logged-in
	  gzochi:execute-ready
	  gzochi:execute-received-message
	  
	  gzochi:make-callback
	  gzochi:callback?
	  gzochi:callback-module
	  gzochi:callback-procedure
	  gzochi:callback-data)

  (import (rnrs)
	  (gzochi io)
	  (gzochi private data)
	  (gzochi private reflect)
	  (gzochi private session))

  (gzochi:define-managed-record-type 
    (gzochi:callback gzochi:make-callback gzochi:callback?)
    
    (fields (immutable procedure (serialization gzochi:symbol-serialization))
	    (immutable module (serialization
			       (gzochi:make-uniform-list-serialization 
				gzochi:symbol-serialization)))
	    (immutable data))
    (protocol 
     (lambda (p)
       (lambda (procedure module . args)
	 (or (list? module)
	     (assertion-violation
	      'gzochi:make-callback "Invalid module name." module))
	 (if (null? args)
	     (p procedure module #f)
	     (let ((arg (car args)))
	       (or (and (or (gzochi:managed-record? arg) (not arg))
			(null? (cdr args)))
		   (assertion-violation
		    'gzochi:make-callback
		    "Callback data must be a single managed record or #f." arg))
	       (p procedure module arg))))))
    (sealed #t))

  (define (gzochi:execute-initialized callback properties)
    (or (gzochi:callback? callback)
	(assertion-violation
	 'gzochi:execute-initialized "Expected gzochi:callback." callback))

    (let ((procedure (gzochi:resolve-procedure
		      (gzochi:callback-procedure callback)
		      (gzochi:callback-module callback))))
      (procedure properties)))

  (define (gzochi:execute-logged-in callback client-session)
    (or (gzochi:callback? callback)
	(assertion-violation
	 'gzochi:execute-logged-in "Expected gzochi:callback." callback))
      
    (let* ((procedure (gzochi:resolve-procedure 
		       (gzochi:callback-procedure callback)
		       (gzochi:callback-module callback)))
	   (handler (procedure client-session)))
      (cond ((not handler) #f)
	    ((gzochi:client-session-listener? handler) handler)
	    (else (assertion-violation
		   'gzochi:execute-logged-in
		   "Invalid type returned by logged-in callback." handler)))))

  (define (gzochi:execute-disconnected callback)
    (or (gzochi:callback? callback)
	(assertion-violation
	 'gzochi:execute-disconnected "Expected gzochi:callback." callback))

    (let ((procedure (gzochi:resolve-procedure
		      (gzochi:callback-procedure callback)
		      (gzochi:callback-module callback)))
	  (data (gzochi:callback-data callback)))
      (if data (procedure data) (procedure))))

  (define (gzochi:execute-ready callback properties)
    (or (gzochi:callback? callback)
	(assertion-violation
	 'gzochi:execute-ready "Expected gzochi:callback." callback))

    (let ((procedure (gzochi:resolve-procedure
		      (gzochi:callback-procedure callback)
		      (gzochi:callback-module callback))))
      (procedure properties)))

  (define (gzochi:execute-received-message callback msg)
    (or (gzochi:callback? callback)
	(assertion-violation
	 'gzochi:execute-received-message "Expected gzochi:callback." callback))
 
    (let* ((procedure (gzochi:resolve-procedure 
		       (gzochi:callback-procedure callback)
		       (gzochi:callback-module callback)))
	   (data (gzochi:callback-data callback)))
      (if data (procedure msg data) (procedure msg))))
)
