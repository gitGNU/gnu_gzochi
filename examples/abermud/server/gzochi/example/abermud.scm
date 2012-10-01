;; abermud.scm --- Server-side implementation of AberMUD for gzochi

;; Copyright (C) 2012 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

#!r6rs

(library (gzochi example abermud)
  (export disconnected initialized logged-in received-message)
  (import (only (guile) fluid-ref)
	  (gzochi)
	  (gzochi example abermud command)
	  (gzochi example abermud data)
	  (gzochi example abermud load)
	  (gzochi example abermud session)
	  (gzochi example abermud util)
	  (only (ice-9 regex) string-match)
	  (rnrs)
	  (srfi :2))

  ;; The starting universe distribution doesn't include exits to connect its
  ;; set of rooms. This `fixup' function creates connections between all of
  ;; the existing rooms so that players can explore them fully.
  
  (define (fixup-room-connections!)
    (define start-room 
      (abermud:find-item 
       (abermud:find-adjective "start") (abermud:find-noun "room")))
    (define creators-grove 
      (abermud:find-item 
       (abermud:find-adjective "Creators") (abermud:find-noun "Grove")))
    (define silent-grove 
      (abermud:find-item 
       (abermud:find-adjective "Silent") (abermud:find-noun "Grove")))
    (define hobbits-hole
      (abermud:find-item 
       (abermud:find-adjective "Hobbit's") (abermud:find-noun "Hole")))
    (define wizards-garden
      (abermud:find-item
       (abermud:find-adjective "Wizards") (abermud:find-noun "Garden")))

    ;; The `Storage Grove' room instructs players to `GOTO HOME' and has no
    ;; other exits. Here we associate the `start room' with the `home' noun
    ;; so that that directive has the desired effect.

    (abermud:add-item! 
     start-room (abermud:find-word "home" (abermud:word-type noun)))

    ;; Link the `Creators Grove' to the `start room' via the `out' and `in'
    ;; directions.

    (abermud:add-room-exit!
     (abermud:as-room start-room) (abermud:direction out) creators-grove)      
    (abermud:add-room-exit! 
     (abermud:as-room creators-grove) (abermud:direction in) start-room)

    ;; Link the `Hobbit's Hole' and `Silent Grove' via the `up' and `down'
    ;; directions.

    (abermud:add-room-exit! 
     (abermud:as-room hobbits-hole) (abermud:direction up) silent-grove)
    (abermud:add-room-exit! 
     (abermud:as-room silent-grove) (abermud:direction down) hobbits-hole)

    ;; Link the `Creators Grove' and `Wizards Garden' via the `south' and
    ;; `north' directions.
    
    (abermud:add-room-exit!
     (abermud:as-room creators-grove) (abermud:direction south) wizards-garden)
    (abermud:add-room-exit!
     (abermud:as-room wizards-garden) 
     (abermud:direction north) 
     creators-grove))
  
  ;; The `initialized' callback. Called once over the lifetime of a gzochi
  ;; AberMUD game.

  (define (initialized properties)
    (define universe-path (hashtable-ref properties "universe_path" #f))
    (define motd-path (hashtable-ref properties "motd_path" #f))
    (define help-path (hashtable-ref properties "help_path" #f))

    ;; Load the MOTD and "help" screens from the paths specified in `game.xml'
    ;; and passed in the `properties' table.

    (define motd 
      (get-string-all 
       (open-file-input-port 
	(string-append (fluid-ref %gzochi:application-root) "/" motd-path))))
    (define help
      (get-string-all
       (open-file-input-port
	(string-append (fluid-ref %gzochi:application-root) "/" help-path))))

    ;; Bootstrap the initial object graph from the universe file given in
    ;; `game.xml'.

    (abermud:bootstrap
     (open-file-input-port 
      (string-append
       (fluid-ref %gzochi:application-root) "/" universe-path)))

    ;; Adjust some of the connections between rooms.

    (fixup-room-connections!)

    ;; Give named bindings to the MOTD and "help" contents so that they may be
    ;; retrieved easily later, providing a serialization wrapper to allow them
    ;; to be persisted.

    (gzochi:set-binding! 
     "motd" (gzochi:make-managed-serializable 
	     motd 
	     (gzochi:make-callback 'gzochi:write-string '(gzochi io)) 
	     (gzochi:make-callback 'gzochi:read-string '(gzochi io))))
    (gzochi:set-binding!
     "help" (gzochi:make-managed-serializable
	     help 
	     (gzochi:make-callback 'gzochi:write-string '(gzochi io))
	     (gzochi:make-callback 'gzochi:read-string '(gzochi io))))

    (gzochi:log-info "Creator Of Legends: Initialised"))

  ;; The `disconnected' callback. Called when a player disconnects explicitly
  ;; or because of a network error condition. Players who have not completed
  ;; the registration process are removed from the game world entirely;
  ;; otherwise they are simply detached from their sessions.

  (define (disconnected abermud-session)
    (let ((player (abermud:session-player abermud-session)))
      (if player 
	  (if (eq? (abermud:session-session-state abermud-session) 
		   (abermud:session-state awaiting-password-set))
	      (begin
		(gzochi:remove-binding! 
		 (string-append 
		  "player." (abermud:player-name (abermud:as-player player))))
		(gzochi:remove-object! player))))))

  ;; Update the prompt displayed to the specified session and notify the client
  ;; that input is allowed. If `echo?' is `#f', characters typed at the prompt
  ;; will not be displayed on the screen.

  (define (update-prompt session prompt echo?)
    (abermud:set-session-echo! session echo?)
    (abermud:set-session-prompt! session prompt)
    (abermud:allow-input session))

  ;; The `logged-in' callback. Called when a client connects to the game. In
  ;; gzochi abermud, this starts the login/registration control flow.

  (define (logged-in client-session)
    (let* ((abermud-session 
	    (abermud:make-session 
	     client-session (abermud:session-state awaiting-name) #f)))

      (abermud:send-output 
       abermud-session
       (gzochi:managed-serializable-value (gzochi:get-binding "motd")))
      (update-prompt abermud-session "What be thy name ? " #t)

      (gzochi:make-client-session-listener
       (gzochi:make-callback
	'received-message '(gzochi example abermud) abermud-session)
       (gzochi:make-callback
	'disconnected '(gzochi example abermud) abermud-session))))

  ;; A convenience function for creating a player: Creates a new game item and 
  ;; adds a "player" facet to it with the specified player name.

  (define (create-player name)
    (let ((item (abermud:make-item 
		 #f (abermud:make-word name (abermud:word-type noun)))))
      (abermud:add-facet! item (abermud:make-player name))
      item))

  ;; The `received-message' callback. Called when the game receives a message
  ;; from a connected player. Depending on the player's session state, advances
  ;; them in the login/registration flow or dispatches to the main command
  ;; parser.

  (define (received-message message abermud-session)
    (case (abermud:session-session-state abermud-session)

      ;; Waiting for the player to type their name.

      ((awaiting-name)
       (let ((name (utf8->string message)))
	 (guard (ex ((gzochi:name-not-bound-condition? ex)
		     (let ((player (create-player name)))
		       (gzochi:set-binding! 
			(string-append "player." name) player)
		       (abermud:set-session-player! abermud-session player)
		       (abermud:set-session-session-state!
			abermud-session 
			(abermud:session-state awaiting-password-set))
		       (update-prompt abermud-session
			"Give me a password for this character : " #f))))

	   (let ((player (gzochi:get-binding (string-append "player." name))))
	     (abermud:set-session-player! abermud-session player)
	     (update-prompt abermud-session "Password: " #f)
	     (abermud:set-session-session-state!
	      abermud-session (abermud:session-state awaiting-password))))))
      
      ;; Waiting for an existing user to type their password.

      ((awaiting-password)
       (let ((password (utf8->string message))
	     (player (abermud:as-player 
		      (abermud:session-player abermud-session))))

	 ;; Do the passwords match?

	 (if (string=? password (abermud:player-password player))
	     (let* ((parent (abermud:item-parent 
			     (abermud:session-player abermud-session)))
		    (room (and parent (abermud:as-room parent))))

	       (abermud:set-session-session-state!
		abermud-session (abermud:session-state ready))

	       ;; Re-connect the client to the broadcast channel of the room
	       ;; the player is currently in.

	       (if room (gzochi:join-channel 
			 (abermud:room-channel room) 
			 (abermud:session-client abermud-session)))

	       (abermud:look 
		abermud-session (abermud:session-player abermud-session))
	       (update-prompt abermud-session "-}--- " #t))

	     (gzochi:disconnect (abermud:session-client abermud-session)))))

      ;; Waiting for a new player to set a password.

      ((awaiting-password-set)
       (let* ((password (utf8->string message))
	      (player (abermud:session-player abermud-session))
	      (player-facet (abermud:as-player player))
	      (start-room (abermud:find-item 
			   (abermud:find-adjective "start")
			   (abermud:find-noun "room"))))

	 (abermud:set-player-password! player-facet password)
	 (abermud:set-session-session-state!
	  abermud-session (abermud:session-state ready))

	 ;; Send the player to the "start room" once they've completed the
	 ;; registration process.

	 (abermud:enter abermud-session start-room player)
	 (update-prompt abermud-session "-}--- " #t)))

      ;; A fully logged-in player; hand off control to the command dispatcher.

      ((ready)
       (abermud:dispatch-command abermud-session (utf8->string message)))
      (else (raise (make-assertion-violation)))))
)
