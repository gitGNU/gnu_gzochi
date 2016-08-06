;; echo-chamber/server.scm --- Echo chamber benchmark, server side
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

(library (server)
  (export disconnected initialized logged-in received-message)
  (import (only (guile) string-prefix?) (gzochi) (rnrs))

  ;; Collocates the client session with the "echo" channel so that the channel
  ;; binding doesn't have to be looked up on every messages.
  
  (gzochi:define-managed-record-type echo-chamber-client
    (fields session channel))
  
  ;; Create the echo channel.
  
  (define (initialized properties) (gzochi:create-channel "echo"))

  ;; The bulk of the logic on the server side is bound up in the "received
  ;; message" callback. It does two things: It broadcasts "PING" messages on the
  ;; echo channel; and it forwards "PONG" messages to the session that sent the
  ;; "PING" to which the sender is responding.
  
  (define (received-message msg client)
    (let ((str (utf8->string msg)))
      (cond ((string-prefix? "PING" str)
             (gzochi:send-channel-message
	      (echo-chamber-client-channel client) msg))
            ((string-prefix? "PONG" str)
             (let ((ping-session (gzochi:get-binding (substring str 5)))
                   (pong-message
		    (string-append
		     "PONG " (gzochi:client-session-name
			      (echo-chamber-client-session client)))))
               (gzochi:send-message ping-session (string->utf8 pong-message))))
            (else (raise (make-assertion-violation))))))

  ;; Remove the session binding and remove the session from the channel.
  
  (define (disconnected client)
    (let ((session (echo-chamber-client-session client)))
      (gzochi:remove-binding! (gzochi:client-session-name session))
      (gzochi:leave-channel (echo-chamber-client-channel client) session)))

  ;; Bind the session by name and add it to the channel.
  
  (define (logged-in client-session)
    (let ((echo-channel (gzochi:get-channel "echo")))
      (gzochi:set-binding!
       (gzochi:client-session-name client-session) client-session)
      (gzochi:join-channel echo-channel client-session)
      
      (let ((client (make-echo-chamber-client client-session echo-channel)))
	(gzochi:make-client-session-listener
	 (g:@ received-message client) (g:@ disconnected client)))))
)
