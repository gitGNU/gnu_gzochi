;; mazewar.scm --- Server-side implementation of gzochi mazewar example game

;; Copyright (C) 2014 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

#!r6rs

(library (gzochi example mazewar)
  (export disconnected initialized logged-in received-message move-missile)
  (import (ice-9 receive)
	  (only (guile) string-join random)
	  (srfi :1)

	  (gzochi app)
	  (gzochi channel)
	  (gzochi client)
	  (gzochi data)
	  (gzochi io)
	  (gzochi log)
	  (gzochi task)

	  (gzochi example mazewar data)
	  (gzochi example mazewar maze)
	  (gzochi example mazewar util)

	  (rnrs))

  ;; Return a random orientation from the enum set of orientations.

  (define (random-orientation) 
    (list-ref (enum-set->list 
	       (mazewar:make-orientation-set north south east west))
	      (random 4)))

  ;; Return a random, unoccupied space from the maze. An assertion violation
  ;; is raise in the unlikely event that no unoccupied spaces can be found.
  
  (define (random-space)

    ;; Retrieve the maze binding.
    
    (define maze (gzochi:get-binding "maze"))
    (define space-vector (mazewar:maze-spaces maze))

    ;; Copy the indexes of potentially free spaces in the maze to a list and 
    ;; loop over it in random order, removing each index as it is considered.

    (let* ((len (gzochi:managed-vector-length space-vector))
	   (free-spaces (let loop ((i 0) (lst (list)))
			  (if (eqv? i len) lst (loop (+ i 1) (cons i lst))))))

      (let loop ((free-spaces free-spaces) (num-spaces len))
	(if (null? free-spaces)
	    (raise (condition
		    (make-message-condition "No unoccupied spaces available.")
		    (make-assertion-violation)))
	    (let* ((index (list-ref free-spaces (random num-spaces)))
		   (space (gzochi:managed-vector-ref space-vector index)))
	      (if (mazewar:space-player space)
		  (loop (remq space free-spaces) (- num-spaces 1))
		  space))))))

  ;; Sends the specified message to each player in the list of observers.

  (define (notify msg observers)
    (for-each (lambda (o) (gzochi:send-message (mazewar:player-session o) msg))
	      observers))

  ;; Notify each player in the list of observers `obs' of the current position
  ;; of the player `p'.

  (define (notify-position p obs) (notify (player-position-message p) obs))

  ;; Notify each player in the list of observers `obs' that player `p' is no
  ;; longer visible.

  (define (notify-hidden p obs) (notify (player-hidden-message p) obs))

  ;; Opcodes for the various message types that can be sent from the server to
  ;; a client session.

  (define mazewar:message-type-player-joined #x01)
  (define mazewar:message-type-player-left #x02)
  (define mazewar:message-type-player-score #x03)
  (define mazewar:message-type-player-position #x04)
  (define mazewar:message-type-player-hidden #x05)
  (define mazewar:message-type-player-shot #x06)

  ;; Opcodes for the various message types that can be sent from a client
  ;; session to the server.

  (define mazewar:message-type-client-turn-left #x01)
  (define mazewar:message-type-client-turn-right #x02)
  (define mazewar:message-type-client-step #x03)
  (define mazewar:message-type-client-shoot #x04)

  ;; Create a message (represented as a bytevector) fragment containing an 
  ;; update to the specified player's score. The first byte of the message 
  ;; contains the number of bytes (n) in the player's name; the following n
  ;; bytes encode the player's name; the last four bytes are the little-endian
  ;; encoding of the player's score.

  (define (player-score player)
    (let ((player-name (mazewar:player-name player))
	  (score-vector (make-bytevector 4)))
      (bytevector-u32-set!
       score-vector 0 (mazewar:player-score player) (endianness little))
      (bytevector-concat (make-bytevector 1 (string-length player-name))
			 (string->utf8 player-name)
			 score-vector)))

  ;; Create a message containing an update to the specified player's score.
  ;; The first byte is the message type opcode; the following bytes are those
  ;; returned by `player-score'.

  (define (player-score-message player)
    (bytevector-concat (make-bytevector 1 mazewar:message-type-player-score)
		       (player-score player)))

  ;; Create a message containing an update to the specified player's position.
  ;; The resulting bytevector contains the message type opcode; followed by a
  ;; single byte giving the length (n) of the player's name; followed by n
  ;; bytes encoding the player's name; followed by one byte giving the x
  ;; position of the player in the maze; followed by one byte giving the y
  ;; position of the player in the maze; followed by one byte representing the
  ;; the orientation of the player.

  (define (player-position-message player)
    (define space (mazewar:player-space player))
    
    (bytevector-concat
     (make-bytevector 1 mazewar:message-type-player-position)
     (make-bytevector 1 (string-length (mazewar:player-name player)))
     (string->utf8 (mazewar:player-name player))
     (u8-list->bytevector
      (list (mazewar:tile-x space)
	    (mazewar:tile-y space)
	    (mazewar:orientation-index (mazewar:player-orientation player))))))

  ;; Create a message indicating that the specified player is no longer 
  ;; visible. The resulting bytevector contains the message type opcode 
  ;; followed by the bytes encoding the player's name.

  (define (player-hidden-message player)
    (bytevector-concat (make-bytevector 1 mazewar:message-type-player-hidden)
		       (string->utf8 (mazewar:player-name player))))

  ;; Returns a list of players visible from the specified space and orientation
  ;; in the maze.

  (define (players-visible-from space orientation)
    (define (players-visible-from-inner space orientation)
      (if (mazewar:space? space)
	  (let ((player (mazewar:space-player space)))
	    (if player
		(cons player (players-visible-from-inner 
			      (mazewar:next space orientation) orientation))
		(players-visible-from-inner
		 (mazewar:next space orientation) orientation)))
 	  '()))
	    
      (players-visible-from-inner (mazewar:next space orientation) orientation))

  ;; Returns a list of the players who can "see" the specified space in the
  ;; maze.

  (define (players-observing space)
    (define (facing orientation)
      (lambda (player) (eq? (mazewar:player-orientation player) orientation)))
    
    (let ((north (filter (facing 'south) (players-visible-from space 'north)))
	  (south (filter (facing 'north) (players-visible-from space 'south)))
	  (east (filter (facing 'west) (players-visible-from space 'east)))
	  (west (filter (facing 'east) (players-visible-from space 'west))))
      (append north south east west)))

  ;; Removes a missile object from the game. Called when the missile hits a
  ;; wall or a player.

  (define (destroy-missile! missile)
    (mazewar:set-player-missile! (mazewar:missile-player missile) #f)
    (gzochi:remove-object! missile))

  ;; Awards the specified number of points to the specified player and notifies
  ;; the other players of that player's new score.

  (define (add-points player points)

    ;; Retrieve the main channel.

    (define main-channel (gzochi:get-channel "main"))

    ;; Set the player's new score.
    
    (mazewar:set-player-score! player (+ (mazewar:player-score player) points))

    ;; Broadcast the player's new score to the main channel.

    (gzochi:send-channel-message main-channel (player-score-message player)))

  ;; Called when a player is hit by a missile.

  (define (player-shot player missile)

    ;; Award a point to the player who "owns" (i.e. fired) the missile 
    ;; originally.
    
    (add-points (mazewar:missile-player missile) 1)

    ;; Destroy the missile.

    (destroy-missile! missile)

    ;; Move the player who got shot to a new space and give them a new
    ;; orientation. 

    (hide-visible-players player)
    (move-player player (random-space))
    (mazewar:set-player-orientation! player (random-orientation))
    (show-visible-players player))
  
  ;; Moves the specified player to the specified space and notify any players
  ;; observing the old or new locations.
  
  (define (move-player p target-space)
    (define space (mazewar:player-space p))
    (define observers (players-observing space))

    (let ((target-observers (cons p (players-observing target-space))))
      (mazewar:set-player-space! p target-space)
      (mazewar:set-space-player! target-space p)
      (mazewar:set-space-player! space #f)
      (notify-position p target-observers)
      (notify-hidden p (lset-difference eq? observers target-observers))))

  ;; The callback for the missile movement task. Advance the missile one space
  ;; and check to see whether it hit anything.

  (define (move-missile missile)
    (define player (mazewar:missile-player missile))
    (define space (mazewar:missile-space missile))
    (define orientation (mazewar:missile-orientation missile))

    (let ((next-tile (mazewar:next space orientation)))
      (cond 
       
       ;; If the missile hit a wall, just destroy it.
       
       ((not (mazewar:space? next-tile)) (destroy-missile! missile))

       ;; If the missile hit a player, handle that case.

       ((mazewar:space-player next-tile) =>
	(lambda (p1) (player-shot p1 missile)))

       ;; Otherwise, reschedule the missile movement task for this missile for
       ;; 200 milliseconds in the future.

       (else (mazewar:set-missile-space! missile next-tile)
	     (gzochi:schedule-task 
	      (g:@ (gzochi example mazewar) move-missile missile)
	      200)))))

  ;; Send `player-hidden' messages to the specified player for each of the
  ;; players visible from the player's current location. To be called right 
  ;; before the player leaves the current space.

  (define (hide-visible-players p)
    (for-each (lambda (p1)
		(gzochi:send-message
		 (mazewar:player-session p) (player-hidden-message p1)))
	      (players-visible-from (mazewar:player-space p)
				    (mazewar:player-orientation p))))

  ;; Send `player-position' messages to the specified player for each of the
  ;; players visible from the player's current location. To be called right
  ;; after the player arrives at a new space.
  
  (define (show-visible-players p)
    (for-each (lambda (p1) 
		(gzochi:send-message 
		 (mazewar:player-session p) (player-position-message p1)))
	      (players-visible-from (mazewar:player-space p)
				    (mazewar:player-orientation p))))

  ;; The `initialized' callback. Called at the beginning of a mazewar game
  ;; before any players are allowed to connect.

  (define (initialized properties)

    ;; Creates a new maze record, which is the primary state object for a game
    ;; of mazewar. The structure of the maze is generated here, as well as a
    ;; serialized form that can be sent to new players when they login.

    (define (generate-maze height width)

      ;; Serializes the maze into a bytevector whose contents are a bitmap such
      ;; that every "one" bit is a space and every "zero" bit is a wall.
      
      (define (maze->bytes tiles)
	(define (tile->byte-fold tile prev)
	  (bitwise-ior (bitwise-arithmetic-shift prev 1)
		       (if (mazewar:space? tile) 1 0)))
	(let ((tile-bytes (make-bytevector (/ (* width height) 8))))
	  (let loop ((i 0) (tiles tiles) (num-tiles (length tiles)))
	    (if (null? tiles)
		tile-bytes
		(receive (next8 tiles)
			 (if (> num-tiles 8) 
			     (split-at tiles 8) 
			     (values tiles '()))
		  (let* ((byte (fold tile->byte-fold 0 next8))
			 (byte (if (< num-tiles 8)
				   (bitwise-arithmetic-shift 
				    byte (- 8 num-tiles))
				   byte)))
		    (bytevector-u8-set! tile-bytes i byte)
		    (loop (+ i 1) tiles (- num-tiles 8))))))))

      ;; Order two maze tiles by their x- and y-positions.

      (define (positional-less t1 t2)
	(cond ((< (mazewar:tile-y t1) (mazewar:tile-y t2)) #t)
	      ((eqv? (mazewar:tile-y t1) (mazewar:tile-y t2))
	       (< (mazewar:tile-x t1) (mazewar:tile-x t2)))
	      (else #f)))
      
      ;; Creaate a new set of maze tiles using the Prim algorithm and use it
      ;; to create a new maze record with an empty list of players.
      
      (let* ((maze (mazewar:prim width height))
	     (tiles 
	      (apply append (vector->list (vector-map vector->list maze))))
	     (tiles (list-sort positional-less tiles)))

	(mazewar:make-maze (apply gzochi:managed-vector 
				  (filter mazewar:space? tiles))
			   (maze->bytes tiles)
			   #f)))

    ;; Create a named binding for the maze.
    
    (gzochi:set-binding! "maze" (generate-maze 16 32))

    ;; Create the "main" channel, which is used for broadcasting global events
    ;; like players joining and leaving the game.

    (gzochi:create-channel "main"))

  ;; The `disconnected' callback. Called when a player disconnects explicitly
  ;; or because of a network error condition.

  (define (disconnected player)
    (let ((space (mazewar:player-space player))
	  (main-channel (gzochi:get-channel "main")))

      ;; Detach the player from the maze.

      (mazewar:set-space-player! space #f)
      (mazewar:set-player-space! player #f)
      
      (let* ((maze (gzochi:get-binding "maze"))
	     (players (gzochi:managed-list->list (mazewar:maze-players maze))))
	
	;; Remove the disconnected player from the global list of players.
	
	(mazewar:set-maze-players! 
	 maze (gzochi:list->managed-list (remq player players))))
      
      ;; Send a message to the main channel notifying the other players of the
      ;; player's departure.

      (gzochi:send-channel-message 
       main-channel (bytevector-concat
		     (make-bytevector 1 mazewar:message-type-player-left)
		     (string->utf8 (mazewar:player-name player))))))

  ;; The `received-message' callback. Called when the game receives a message
  ;; from a connected player.

  (define (received-message msg p)
    (define cmd (car (bytevector->u8-list msg)))
    (define orientation (mazewar:player-orientation p))
    (define space (mazewar:player-space p))

    ;; Set the player's orientation to the specified value and update player
    ;; visibility appropriately: Hide the players that were visible, and reveal
    ;; the players visible from the new orientation. Additionally, notify
    ;; observers of the player's current space that the player's orientation
    ;; has changed.

    (define (turn orientation)
      (hide-visible-players p)
      (mazewar:set-player-orientation! p orientation)
      (notify-position p (cons p (players-observing space)))
      (show-visible-players p))

    (cond

     ;; The player turned left.

     ((eqv? cmd mazewar:message-type-client-turn-left) 
      (turn (mazewar:left orientation)))

     ;; The player turned right.

     ((eqv? cmd mazewar:message-type-client-turn-right) 
      (turn (mazewar:right orientation)))

     ;; The player stepped forward. Verify that the next tile forward, relative
     ;; to their current orientation, is an unoccupied space. If so, move the
     ;; player there.
     
     ((eqv? cmd mazewar:message-type-client-step) 
      (let ((next-space (mazewar:next space orientation)))
	(if (and next-space
		 (mazewar:space? next-space) 
		 (not (mazewar:space-player next-space)))
	    (move-player p next-space))))

     ;; The player shot a missile. Create a new missile record and start the
     ;; missile movement task.

     ((eqv? cmd mazewar:message-type-client-shoot)
      (let ((missile (mazewar:make-missile orientation p space)))
	(mazewar:set-player-missile! p missile)
	(gzochi:schedule-task 
	 (g:@ (gzochi example mazewar) move-missile missile) 200)))))

  ;; The `logged-in' callback. Called when a new player attempts to join the
  ;; game.

  (define (logged-in session)
    (let* ((maze (gzochi:get-binding "maze"))
	   (starting-space (random-space))

	   ;; Create the new player and assign them a random starting space and
	   ;; orientation. Use the name associated with the session's identity
	   ;; as the new player's name.

	   (player (mazewar:make-player
		    starting-space
		    (random-orientation)

		    #f ;; missile
		    0 ;; score

		    (gzochi:client-session-name session)
		    session)))

      ;; Attach the player to the space.

      (mazewar:set-space-player! starting-space player)

      (let* ((player-list (mazewar:maze-players maze))
	     (players (gzochi:managed-list->list player-list)))

	;; Send a "bootstrapping" message to the new player. This message
	;; consists of: One byte giving the width in tiles of the maze; one 
	;; one byte giving the height in tiles of the maze; one byte giving the
	;; current number of other players; followed by a list of the current 
	;; players' names and scores (as generated by `player-score'). 

	(gzochi:send-message
	 session (bytevector-concat 
		  #u8(32 16)
		  (mazewar:maze-bytes maze)
		  (make-bytevector 1 (length players))
		  (apply bytevector-concat (map player-score players))))

	;; Add the new player to the global list of players. 

	(mazewar:set-maze-players! maze (gzochi:cons player player-list)))

      ;; Tell the new player their own location.

      (notify-position player (list player))

      ;; Retrieve the main channel and use it to broadcast a message notifying
      ;; all the existing players of the presence of the new player, then add 
      ;; the new player to the main channel.
      
      (let ((main-channel (gzochi:get-channel "main")))
	(gzochi:send-channel-message 
	 main-channel (bytevector-concat 
		       (make-bytevector 1 mazewar:message-type-player-joined)
		       (string->utf8 (mazewar:player-name player))))
	(gzochi:join-channel main-channel session))

      ;; Return a new client session listener with the appropriate callbacks,
      ;; indicating a successful login.
      
      (gzochi:make-client-session-listener
       (g:@ (gzochi example mazewar) received-message player)
       (g:@ (gzochi example mazewar) disconnected player))))
)
