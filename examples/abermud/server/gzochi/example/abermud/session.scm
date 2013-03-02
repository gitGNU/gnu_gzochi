;; session.scm --- Session-related structures and procedures

;; Copyright (C) 2013 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

#!r6rs

(library (gzochi example abermud session)
  (export abermud:session-state 
	  abermud:make-session-state-set

	  abermud:send-output
	  abermud:set-session-echo!
	  abermud:set-session-prompt!
	  abermud:set-session-title!
	  abermud:allow-input

	  abermud:broadcast-output
	  abermud:broadcast-packet

	  abermud:make-session
	  abermud:session?
	  abermud:session-client
	  abermud:session-player
	  abermud:session-session-state
	  abermud:set-session-player!
	  abermud:set-session-session-state!)

  (import (gzochi)
	  (gzochi example abermud data)
	  (gzochi example abermud util)
	  (rnrs))

  ;; Opcodes for the various packet types that can be sent from the server to a
  ;; client session.

  (define abermud:packet-type-echo-on #x05)
  (define abermud:packet-type-echo-off #x06)
  (define abermud:packet-type-allow-input #x07)
  (define abermud:packet-type-set-prompt #x83)
  (define abermud:packet-type-output #x81)
  (define abermud:packet-type-set-title #xf0)

  ;; The enumeration of possible session states, from initial player 
  ;; registration to fully logged in.
  
  (define-enumeration abermud:session-state 
    (awaiting-name awaiting-password awaiting-password-set ready)
    abermud:make-session-state-set)

  ;; Returns a bytevector in the gzochi abermud "packet" format: One byte
  ;; giving the packet opcode; zero or more additional bytes encoding an 
  ;; optional text payload.
  
  (define (make-packet type data)
     (if (null? data)
	 (make-bytevector 1 type)
	 (bytevector-concat (make-bytevector 1 type) 
			    (string->utf8 (car data)))))

  ;; Creates a gzochi abermud packet out of the specified opcode and data, and
  ;; sends it to the specified session.

  (define (abermud:send-packet session type . data)
    (gzochi:send-message
     (abermud:session-client session) (make-packet type data)))

  ;; Sends the specified text to the specified session, prefixing it with the
  ;; `output' packet opcode.

  (define (abermud:send-output session text)
    (abermud:send-packet session abermud:packet-type-output text))

  ;; Creates a gzochi abermud packet out of the specified opcode and data, and
  ;; broadcasts it to the specified channel.

  (define (abermud:broadcast-packet channel type . data)
    (gzochi:send-channel-message channel (make-packet type data)))

  ;; Broadcasts the specified text to the specified channel, prefixing it with 
  ;; the `output' packet opcode.

  (define (abermud:broadcast-output channel text)
    (abermud:broadcast-packet channel abermud:packet-type-output text))

  ;; Notifies the client associated with the specified session to allow input 
  ;; from the keyboard.

  (define (abermud:allow-input session)
    (abermud:send-packet session abermud:packet-type-allow-input))

  ;; Set the "echo" state of the specified session according to the `echo?'
  ;; parameter (a boolean value). When echo is `#f', players will not be able
  ;; to see the result of their keystrokes; this is useful for visually 
  ;; sensitive contexts, such as entering passwords.

  (define (abermud:set-session-echo! session echo?)
    (abermud:send-packet session 
			 (if echo? 
			     abermud:packet-type-echo-on 
			     abermud:packet-type-echo-off)))
  
  ;; Set the title to be displayed for the specified session's client.
  
  (define (abermud:set-session-title! session title)
    (abermud:send-packet session abermud:packet-type-set-title title))

  ;; Set the input prompt to be displayed for the specified session's client.

  (define (abermud:set-session-prompt! session prompt)
    (abermud:send-packet session abermud:packet-type-set-prompt prompt))

  ;; The gzochi abermud session type. This managed record type contains fields
  ;; for a gzochi client session, the session state, and the player item (which
  ;; is set following login or registration).

  (gzochi:define-managed-record-type
   (abermud:session abermud:make-session abermud:session?)

   (fields (immutable client)
           (mutable session-state 
		    abermud:session-session-state 
		    abermud:set-session-session-state!
		    (serialization gzochi:symbol-serialization))
	   (mutable player 
		    abermud:session-player 
		    abermud:set-session-player!)))
)
