;; gzochi/main-loop.scm: A main loop implementation
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

(library (gzochi main-loop)
  (export gzochi:selector
	  gzochi:selector?
	  gzochi:make-selector

	  gzochi:selector-event
	  gzochi:make-selector-event-set

	  gzochi:selector-events
	  gzochi:selector-revents
	  gzochi:selector-port/fd
	  gzochi:selector-clear-events!
	  gzochi:selector-add-event!

	  gzochi:poll

	  gzochi:source
	  gzochi:source?
	  gzochi:make-source
	  gzochi:source-selector

	  gzochi:main-loop?
	  gzochi:make-main-loop
	  gzochi:main-loop-add-source!
	  gzochi:main-loop-run
	  gzochi:main-loop-stop)

  (import (only (guile) hashv select sort) 
	  (ice-9 optargs) 
	  (ice-9 threads) 
	  (rnrs)
	  (srfi :1) 
	  (srfi :18))

;; This is a simple "main loop" implementation that gzochi client instances
;; created with (gzochi client) may be plugged into, along with other sources of
;; input to your game application; to provide fast, asynchronous responses to
;; I/O and "soft" events. This implementation is inspired by the design of the
;; GLib main loop API.
;;
;; Like GLib's main loop, this one queries a set of a registered event sources
;; to discover events that are ready to be handled. Each source attached to a
;; gzochi:main-loop must be a subtype of the gzochi:source record type and must
;; provide three lifecycle procedures that support the loop's interrogation of
;; the source's state. Those procedures are:
;;
;; prepare -- Return #t if the source has an event ready to be dispatched;
;;   #f if it is unclear whether the source has an event ready to be dispatched;
;;   or a "timeout" value of the form (secs . usecs) to indicate that the source
;;   should be checked for an event after that amount of time has elapsed.
;;
;; check -- Return #t if the source has an event ready to be dispatched;
;;   #f if the source does not have an event ready to be dispatched.
;;
;; dispatch -- Do the work of handling the ready event: Update internal state,
;;   trigger side effects, etc.
;; 
;; In addition to these procedures, a source may optionally include:
;;
;;   A default timeout value of the form (secs . usecs), to indicate how long
;;   the source should be polled for events if the prepare procedure does not
;;   return #t.
;;
;;   A "selector" that wraps a port or file descriptor to be passed to a call to
;;   select. This allows the main loop's polling mechanism to support the most
;;   common form of asynchronous event detection. The selector structure 
;;   includes the types of events (read, write, error) the source is interested
;;   in and a mutable set manipulated by the main loop to indicate which event 
;;   types were detected by the poll.
;;
;; The methods and properties above are used by the main loop to implement its
;; asynchronous event detection as follows: First, the prepare procedure of all
;; sources is called. Each source that returns #f or a timeout value is then
;; polled; polled sources that have a selector will be used to invoke select,
;; and the results of select are used to update the ready events state of each
;; participating source. Next, the check procedure of all sources is called,
;; followed by the dispatch procedure for any sources whose check invocation
;; returns #t.
;;
;; Here is a complete example of using the main loop to process events from two
;; hypothetical input ports:
;;
;; (define (keyboard-prepare/check source) 
;;   (char-ready? (gzochi:selector-port/fd (gzochi:source-selector source))))
;; (define (keyboard-dispatch source)
;;   (display (read-char (gzochi:selector-port/fd 
;;                        (gzochi:source-selector source)))))
;;
;; (define (mouse-prepare source) (or (mouse-check source) (cons 0 10000)))
;; 
;; (define (mouse-check source)
;;   (or (not (eqv? current-x (mouse-x mouse))
;;            (eqv? current-y (mouse-y mouse)))))
;;
;; (define (mouse-dispatch source) (draw-cursor mouse))
;;
;; (let ((keyboard-source (gzochi:make-source #f
;;                         #:prepare keyboard-prepare/check 
;;                         #:check keyboard-prepare/check 
;;                         #:dispatch keyboard-dispatch))
;;       (mouse-source (gzochi:make-source #f
;;                      #:prepare mouse-prepare
;;                      #:check mouse-check
;;                      #:dispatch mouse-dispatch))
;;       (main-loop (gzochi:make-main-loop)))
;;    (gzochi:main-loop-add-source! main-loop keyboard-source)
;;    (gzochi:main-loop-add-source! main-loop mouse-source)
;;    (gzochi:main-loop-run main-loop))

  (define (timeout? obj)
    (and (list? obj)
	 (let ((len (length obj)))
	   (or (= len 1)
	       (= len 2)))
	 (every integer? obj)))	 

;; The selector event enumeration. Possible events are: read, write, error.
;;
;; To create a new enum-set in this universe, call 
;; (gzochi:make-selector-event-set . events) where events are zero or more of 
;; the literals read, write, error.

  (define-enumeration gzochi:selector-event 
    (read write error) 
    gzochi:make-selector-event-set)
  
  (define (port/fd? obj) (or (port? obj) (integer? obj)))

  (define (check-argument predicate argument optional?)
    (or (and optional? (not argument))
	(predicate argument)
	(raise (condition (make-assertion-violation) 
			  (make-irritants-condition argument)))))

;; Record type definition for gzochi:selector.
;;
;; To create a new selector, call (gzochi:make-selector port/fd events), where
;; port/fd is a port or file descriptor, and events is an enum-set created by a
;; call to gzochi:make-selector-event-set specifying the events for which this 
;; port or file descriptor should be polled.
 
  (define-record-type (gzochi:selector gzochi:make-selector gzochi:selector?)
    (fields port/fd events (mutable revents))
    (protocol (lambda (p)
		(lambda (port/fd events)
		  (check-argument port/fd? port/fd #f)

		  (p port/fd events (gzochi:make-selector-event-set))))))

;; Empty the set of "ready" events on the specified selector. This procedure is
;; called on every iteration of the main loop, so it is typically not necessary
;; to call yourself unless your code explicitly polls selectors.

  (define (gzochi:selector-clear-events! selector)
    (check-argument gzochi:selector? selector #f)
    (gzochi:selector-revents-set! selector (gzochi:make-selector-event-set)))
  
;; Adds the specified event (which must be one of the symbols 'read 'write or
;; 'error) to the set of "ready" events on the specified selector. This
;; procedure is called when the main loop polls selectors, so it is typically
;; not necessary to call yourself unless your code explicitly manipulates
;; selector state.

  (define (gzochi:selector-add-event! selector event)
    (check-argument gzochi:selector? selector #f)
    (check-argument symbol? event #f)

    (let ((events (gzochi:selector-revents selector)))
      (gzochi:selector-revents-set!
       selector (enum-set-union 
		 events (enum-set-projection (make-enumeration (list event)) 
					     events)))))

;; Record type definition for gzochi:source.
;;
;; To create a new source, call 
;; (gzochi:make-source selector #:optional timeout #:key prepare check dispatch)
;; where selector is a gzochi:selector or #f, timeout is an optional timeout
;; value of the form (secs . usecs) specifying a maximum poll time, and the
;; keyword arguments #:prepare, #:check, and #:dispatch specify the prepare,
;; check, and dispatch procedures, respectively.

  (define-record-type (gzochi:source gzochi:make-source gzochi:source?)
    (fields selector timeout prepare check dispatch)
    (protocol (lambda (p)
		(lambda* (selector 
			  #:optional timeout
			  #:key prepare check dispatch)

		    (check-argument gzochi:selector? selector #t)
		    (check-argument timeout? timeout #t)

		    (check-argument procedure? prepare #f)
		    (check-argument procedure? check #f)
		    (check-argument procedure? dispatch #f)

		    (p selector timeout prepare check dispatch)))))

;; Record type definition for the main loop.
;;
;; To create a new main loop, call (gzochi:make-main-loop).
 
  (define-record-type (gzochi:main-loop gzochi:make-main-loop gzochi:main-loop?)
    (fields (mutable sources)
	    (mutable running)
	    port/fd->source
	    sources-mutex)
    (protocol (lambda (p) 
		(lambda () (p (list) #f (make-eqv-hashtable) (make-mutex))))))

;; Add the specified source to the specified main loop. This procedure may be
;; called while the main loop is running; the new source will be included in the
;; next iteration of the loop.

  (define (gzochi:main-loop-add-source! main-loop source)
    (check-argument gzochi:main-loop? main-loop #f)
    (check-argument gzochi:source? source #f)

    (with-mutex (gzochi:main-loop-sources-mutex main-loop)
      (let ((sources (gzochi:main-loop-sources main-loop)))
	(if (not (memq source sources))
	    (begin
	      (gzochi:main-loop-sources-set! main-loop (cons source sources))
	      (if (gzochi:source-selector source)
		  (hashtable-set! (gzochi:main-loop-port/fd->source main-loop) 
				  (gzochi:selector-port/fd
				   (gzochi:source-selector source))
				  source)))))))

  (define-record-type (gzochi:prepared-source 
		       gzochi:make-prepared-source
		       gzochi:prepared-source?)
    (fields dispatchable? prepare-timeout default-timeout source))

;; Poll the specified selectors for events. The selectors to be polled must be
;; passed as a list of gzochi:selector objects. An optional timeout may be
;; specified as [secs [usecs]] and gives the maximum time this procedure may
;; remain in the underlying call to select if no events are detected.

  (define (gzochi:poll selectors . timeout)
    (define (normalize-timeout sec usec)
      (and sec (if usec
		   (let-values (((d m) (div-and-mod usec 1000000)))
		     (cons (+ sec d) m))
		   (cons sec 0))))
    (define (nonnegative-integer? x) (and (integer? x) (>= x 0)))
    
    (check-argument (lambda (x) (every gzochi:selector? x)) selectors #f)
    
    (let* ((len (length selectors))
	   (port/fd-less (lambda (a b) (< (hashv a len) (hashv b len))))
	   (selector-less (lambda (a b) 
			    (< (hashv (gzochi:selector-port/fd a) len)
			       (hashv (gzochi:selector-port/fd b) len))))
	   (selectors (sort selectors selector-less))
	   (port/fds (map gzochi:selector-port/fd selectors))
	   (sec (if (null? timeout) #f (car timeout)))
	   (usec (and sec (if (null? (cdr timeout)) #f (cadr timeout))))

	   (read (filter-map (lambda (selector) 
			       (and (enum-set-member? 
				     (gzochi:selector-event read)
				     (gzochi:selector-events selector))
				    (gzochi:selector-port/fd selector)))
			     selectors))

	   (write (filter-map (lambda (selector) 
				(and (enum-set-member? 
				      (gzochi:selector-event write)
				      (gzochi:selector-events selector))
				     (gzochi:selector-port/fd selector)))
			      selectors))

	   (error (filter-map (lambda (selector) 
				(and (enum-set-member? 
				      (gzochi:selector-event error)
				      (gzochi:selector-events selector))
				     (gzochi:selector-port/fd selector)))
			      selectors))
	   
	   (timeout (begin
		      (check-argument nonnegative-integer? sec #t)
		      (check-argument nonnegative-integer? usec #t)
		      (normalize-timeout sec usec)))

	   (ready (if timeout 
		      (select read write error (car timeout) (cdr timeout))
		      (select read write error))))

      (let loop ((read (sort (car ready) port/fd-less))
		 (write (sort (cadr ready) port/fd-less))
		 (error (sort (caddr ready) port/fd-less))
		 (selectors selectors))

	(if (not (null? selectors))
	    (let* ((selector (car selectors))
		   (pfd (gzochi:selector-port/fd selector))
		   (read (drop-while (lambda (x) (port/fd-less x pfd)) read))
		   (write (drop-while (lambda (x) (port/fd-less x pfd)) write))
		   (error (drop-while (lambda (x) (port/fd-less x pfd)) error)))

	      (if (not (and (null? read) (null? write) (null? error)))
		  (begin
		    (or (null? read)
			(if (eqv? (car read) pfd)
			    (gzochi:selector-add-event! 
			     selector (gzochi:selector-event read))))
		    (or (null? write)
			(if (eqv? (car write) pfd)
			    (gzochi:selector-add-event! 
			     selector (gzochi:selector-event write))))
		    (or (null? error)
			(if (eqv? (car error) pfd)
			    (gzochi:selector-add-event! 
			     selector (gzochi:selector-event error))))
		    (loop read write error (cdr selectors)))))))))

;; Stops the specified main loop from running; no further iterations will occur.

  (define (gzochi:main-loop-stop main-loop)
    (with-mutex (gzochi:main-loop-sources-mutex main-loop)
      (gzochi:main-loop-running-set! main-loop #f)))

;; Runs the specified main loop. The loop will run until gzochi:main-loop-stop
;; is called (from within one of the loops callback procedures or from a
;; separate thread) or one of the prepare, check, or dispatch callbacks exits
;; non-locally (i.e., via an error).

  (define (gzochi:main-loop-run main-loop)
    (define (prepared-source-less a b)
      (define (timeout-less a b)
	(define (timeout->usecs t)
	  (+ (* (car t) 1000000) (if (pair? (cdr t)) (cadr t) 0)))
	(< (timeout->usecs a) (timeout->usecs b)))
      
      (let* ((prepare-timeout-a (gzochi:prepared-source-prepare-timeout a))
	     (prepare-timeout-b (gzochi:prepared-source-prepare-timeout b))
	     (default-timeout-a (gzochi:prepared-source-default-timeout a))
	     (default-timeout-b (gzochi:prepared-source-default-timeout b))
	     (timeout-a (or prepare-timeout-a default-timeout-a))
	     (timeout-b (or prepare-timeout-b default-timeout-b)))
	
	(if timeout-a 
	    (if timeout-b 
		(timeout-less timeout-a timeout-b)
		#t)
	    #f)))

    (define (prepare source) 
      (let ((result ((gzochi:source-prepare source) source)))
	(gzochi:make-prepared-source 
	 (eqv? result #t) 
	 (and (timeout? result) result)
	 (gzochi:source-timeout source)
	 source)))

    (define (check prepared-source)
      (let ((source (gzochi:prepared-source-source prepared-source)))
	((gzochi:source-check source) source)))

    (define (dispatch prepared-source)
      (let ((source (gzochi:prepared-source-source prepared-source)))
	((gzochi:source-dispatch source) source)))

    (gzochi:main-loop-running-set! main-loop #t)

    (let loop ()
      (with-mutex (gzochi:main-loop-sources-mutex main-loop)
        (let* ((sources (gzochi:main-loop-sources main-loop))
	       (selectors (filter-map gzochi:source-selector sources)))
	  (for-each gzochi:selector-clear-events! selectors)
	  (let ((prepared-sources (map prepare sources)))
	    (let-values
		(((dispatchable-sources non-dispatchable-sources)
		  (partition gzochi:prepared-source-dispatchable?
			   prepared-sources)))
	      (let ((pollable (map (lambda (prepared-source)
				     (gzochi:source-selector
				      (gzochi:prepared-source-source 
				       prepared-source)))
				   non-dispatchable-sources)))

		(if (not (null? pollable))
		    (let* ((sorted-sources 
			    (sort prepared-sources prepared-source-less))
			   (earliest (car sorted-sources))
			   (timeout
			    (or (gzochi:prepared-source-prepare-timeout 
				 earliest)
				(gzochi:prepared-source-default-timeout 
				 earliest))))

		      (if timeout
			  (gzochi:poll pollable (car timeout) (cdr timeout))
			  (gzochi:poll pollable))))
	      
		(for-each dispatch (filter check prepared-sources))
		(if (gzochi:main-loop-running main-loop)
		  (loop)))))))))
)
