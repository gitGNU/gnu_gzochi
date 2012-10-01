;; command.scm --- Command-dispatching structures and procedures

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

(library (gzochi example abermud command)
  (export abermud:dispatch-command

	  abermud:look
	  abermud:enter)

  (import (only (guile) string-join)
	  (gzochi)
	  (gzochi data)
	  (gzochi example abermud data)
	  (gzochi example abermud session)
	  (ice-9 format)
	  (ice-9 optargs)
	  (rnrs))

  ;; Derives the text of the message to send indicating that a player or object 
  ;; has arrived at a location, defaulting to the string "arrives".
   
  (define (in-msg item)
    (let ((ioh (abermud:as-inouthere item)))
      (if ioh (abermud:inouthere-in ioh) "arrives")))

  ;; Derives the text of the message to send indicating that a player or object 
  ;; has departed from a location, defaulting to the string "goes".

  (define (out-msg item)
    (let ((ioh (abermud:as-inouthere item)))
      (if ioh (abermud:inouthere-out ioh) "goes")))

  ;; Derives the text of the message to send indicating that a player or object 
  ;; is present at a location, defaulting to the string "is here".

  (define (here-msg item)
    (let ((ioh (abermud:as-inouthere item)))
      (if ioh (abermud:inouthere-here ioh) "is here")))

  ;; Moves the `abermud:item' `player' to the `abermud:item' `room', which must
  ;; have the "room" facet. An arrival message is broadcast to the current
  ;; player children of `room', and the `abermud:session' `session' is sent a
  ;; description of the room and its current inhabitants.  

  (define (abermud:enter session item player)
    (if (abermud:item-parent player)
	(leave session player))
    (let* ((room (abermud:as-room item))
	   (room-channel (abermud:room-channel room)))
      (if room-channel
	  (abermud:broadcast-output 
	   room-channel (string-append 
			 (abermud:player-name (abermud:as-player player)) 
			 " " (in-msg player) ".\n")))
      (abermud:link-item! item player)
      (if room-channel
	  (gzochi:join-channel 
	   room-channel (abermud:session-client session)))
      (abermud:look session player)))

  ;; Broadcasts the string `text' to all members of the specified player item's
  ;; current room (including the player themselves).

  (define (say player text)
    (let* ((room (abermud:as-room (abermud:item-parent player)))
	   (channel (abermud:room-channel room)))
      (abermud:broadcast-output 
       channel (string-append 
		(abermud:player-name (abermud:as-player player))
		" says " 
		(if (> (string-length text) 0) 
		    (format #f "'~a'" text) 
		    "something") 
		".\n"))))

  ;; Makes the specified item (which must have the "object" facet with the 
  ;; `can-get' flag set) a child of the specified player. The `abermud:session'
  ;; `session' is sent a descriptive message on success or failure.

  (define (get session player item)
    (let ((object (and item (abermud:as-object item)))
	  (parent (and item (abermud:item-parent item)))
	  (name (and item (abermud:item-name item))))
      (cond ((not item) 
	     (abermud:send-output session "I don't know what you mean.\n"))
	    ((or (not object) (not (memq (abermud:object-flag can-get) 
					 (abermud:object-flags object))))
	     (abermud:send-output session "You can't take that!\n"))
	    ((eq? parent player)
	     (abermud:send-output 
	      session (string-append "You are already carrying " name ".\n")))
	    ((not (eq? parent (abermud:item-parent player)))
	     (abermud:send-output session "That isn't here.\n"))
	    (else (begin
		    (abermud:unlink-item! item)
		    (abermud:link-item! player item)
		    (abermud:send-output 
		     session (string-append "You take " name ".\n")))))))

  ;; Removes the specified item from the specified player's inventory and makes
  ;; it a sibling of the player (i.e., another child of the player's parent
  ;; item, the current room). The `abermud:session' `session' is sent a
  ;; descriptive message on success or failure.

  (define (drop session player item)
    (let ((name (and item (abermud:item-name item))))
      (cond ((not item) (abermud:send-output session "That isn't here.\n"))
	    ((not (eq? (abermud:item-parent item) player))
	     (abermud:send-output 
	      session (string-append "You are not carrying " name ".\n")))
	    (else (begin (abermud:unlink-item! item)
			 (abermud:link-item! (abermud:item-parent player) item)
			 (abermud:send-output
			  session (string-append "You drop " name ".\n")))))))

  ;; Detach the specified player from its current parent item, which must have 
  ;; the "room" facet. A departure message is broadcast to other player 
  ;; children of the room after the player has been removed.

  (define (leave session player)
    (let* ((room (abermud:as-room (abermud:item-parent player)))
	   (room-channel (abermud:room-channel room)))
      (if room-channel
	  (gzochi:leave-channel room-channel (abermud:session-client session)))
      (abermud:unlink-item! player)      
      (if room-channel
	  (abermud:broadcast-output 
	   room-channel (string-append 
			 (abermud:player-name (abermud:as-player player)) 
			 " " (out-msg player) ".\n")))))

  (define (move session player direction)
    (let* ((room (abermud:as-room (abermud:item-parent player)))
	   (exit (abermud:room-exit room direction)))

      (if exit
	  (begin
	    (leave session player)
	    (abermud:send-output session (abermud:exit-message exit))
	    (abermud:enter session (abermud:exit-destination exit) player))
	  (abermud:send-output session "You can't go that way.\n"))))

  ;; Sends a description of the `abermud:item' `item' to the `abermud:session'
  ;; `session'. The text of the description depends on the facets of `item'
  ;; (which are assumed to be mutually exclusive). If `item' has the "object"
  ;; facet, the object's description is sent to `session'. 
  ;; 
  ;; If `item' has the "player" facet, the player's name is composed into a 
  ;; presence description (which may involve the "inouthere" facet if it is 
  ;; present on `item').
  ;;
  ;; If neither of these facets is present, this function is a no-op.

  (define (describe session item)
    (cond ((abermud:as-object item) => 
	   (lambda (object) 
	     (abermud:send-output 
	      session (string-append (abermud:object-text object) "\n"))))
	  ((abermud:as-player item) =>
	   (lambda (player)
	     (abermud:send-output 
	      session (string-append (abermud:player-name player) 
				     " " (here-msg item) ".\n"))))))

  ;; Processes the `inventory' command by formatting a description of the
  ;; items that are children of the player item, then sends that description
  ;; to the player's session.

  (define (inventory session player)
    (let* ((children (abermud:item-children player))
	   (output "You are carrying ")
	   (output (if children
		       (let loop ((children children) 
				  (output output)
				  (first? #t))
			 (if children
			     (let ((child (gzochi:car children))
				   (rest (gzochi:cdr children)))
			       (loop rest
				     (string-append 
				      output
				      (cond (first? "")
					    (rest ", ")
					    (else " and "))
				      (abermud:item-name child))
				     #f))
			     (string-append output ".\n")))
		       (string-append output "nothing.\n"))))
      (abermud:send-output session output)))

  ;; Processes a non-transitive form of the `look' command by setting the 
  ;; specified session's title to the short room description of the specified 
  ;; player's parent room, sending the long room description for that room to
  ;; the session, and formatting and sending the descriptive text of any 
  ;; objects in the room.
  ;;
  ;; The original AberMUD supports a form of the `look' command that takes a
  ;; direct object and provides a more detailed description thereof. The
  ;; implementaton of this functionality is left as an exercise for the reader.
  
  (define (abermud:look session player)
    (define (flannel-object? item)
      (let ((object (abermud:as-object item)))
	(and object (memq (abermud:object-flag flannel) 
			  (abermud:object-flags object)))))

    (let* ((parent (abermud:item-parent player))
	   (room (abermud:as-room parent))
	   (children (gzochi:managed-list->list 
		      (abermud:item-children parent))))
      
      (abermud:set-session-title! 
       session (abermud:room-short-description room))

      (abermud:send-output 
       session (string-append 
		(abermud:room-long-description room) 
		(string-join (map (lambda (flannel-object) 
				    (abermud:object-text 
				     (abermud:as-object flannel-object)))
				  (filter flannel-object? children)))
		"\n"))
      
      (for-each (lambda (child) (describe session child))
		(filter (lambda (child) 
			  (and (not (eq? child player)) 
			       (not (flannel-object? child))))
			children))))

  ;; Processes the `quit' command by sending a "goodbye" message to the 
  ;; specified session and then disconnecting it.

  (define (quit-game session)
    (abermud:send-output session "Goodbye......\n")
    (gzochi:disconnect (abermud:session-client session)))

  ;; Processes the `help' command by sending the contents of the "help" 
  ;; binding to the specified session.

  (define (show-help session)
    (abermud:send-output
     session (gzochi:managed-serializable-value (gzochi:get-binding "help"))))

  ;; Processes the `exits' command by inspecting the exit mappings for the
  ;; specified player's current room, and sending a formatted description of
  ;; each to the specified session.

  (define (exits session player)
    (define (exit-message direction destination)      
      (format #f "~20@a:~a\n" direction (abermud:item-name destination)))

    (let* ((parent (abermud:item-parent player))
	   (room (abermud:as-room parent))
	   (directions (enum-set-universe (abermud:make-direction-set))))

      (let loop ((directions (enum-set->list directions))
		 (found-one? #f))
		      
	(if (null? directions)
	    
	    ;; If we're out of directions and there was no exit found, let the
	    ;; player know. (This will be the case for the `Storage Grove',
	    ;; which the player must exit using the `goto' command.

	    (if (not found-one?)
		(abermud:send-output session "None....\n"))

	    (let* ((direction (car directions))
		   (exit (abermud:room-exit room direction))
		   (target (and exit (abermud:exit-destination exit))))
	      (if target
		  (begin 
		    (abermud:send-output 
		     session (exit-message direction target))
		    (loop (cdr directions) #t))
		  (loop (cdr directions) found-one?)))))))

  ;; Progressively tokenizes the specified command string based on a delimiter
  ;; set ('.', ';', ',', and whitespace). Returns two values: The next token
  ;; in the command string, and the portion of the command string following
  ;; the delimiter. 
  
  (define (break-command command)
    (let ((len (string-length command)))
      (let loop ((i 0) (head (list)))
	(if (eqv? i len)
	    (values (list->string (reverse head)) "")
	    (let ((c (string-ref command i)))
	      (if (memv c '(#\. #\; #\, #\space))
		  (values (list->string (reverse head))
			  (substring command (+ i 1)))
		  (loop (+ i 1) (cons c head))))))))

  ;; Sets up the core set of verbs understood by the system.

  (define (abermud:initialize-words)
    (for-each abermud:add-word!
	      (list (abermud:make-word "inventory" (abermud:word-type verb))
		    (abermud:make-word "look" (abermud:word-type verb))
		    (abermud:make-word "goto" (abermud:word-type verb))
		    (abermud:make-word "say" (abermud:word-type verb))
		    (abermud:make-word "quit" (abermud:word-type verb))
		    (abermud:make-word "exits" (abermud:word-type verb))
		    (abermud:make-word "take" (abermud:word-type verb))
		    (abermud:make-word "drop" (abermud:word-type verb))
		    (abermud:make-word "help" (abermud:word-type verb))
		    (abermud:make-word "north" (abermud:word-type verb))
		    (abermud:make-word "east" (abermud:word-type verb))
		    (abermud:make-word "south" (abermud:word-type verb))
		    (abermud:make-word "west" (abermud:word-type verb))
		    (abermud:make-word "up" (abermud:word-type verb))
		    (abermud:make-word "down" (abermud:word-type verb))
		    (abermud:make-word "northeast" (abermud:word-type verb))
		    (abermud:make-word "northwest" (abermud:word-type verb))
		    (abermud:make-word "southeast" (abermud:word-type verb))
		    (abermud:make-word "southwest" (abermud:word-type verb))
		    (abermud:make-word "in" (abermud:word-type verb))
		    (abermud:make-word "out" (abermud:word-type verb)))))

  ;; Parses the next `abermud:word' from the string `command' and returns two
  ;; values: The parsed word and the rest of the command. If the `type'
  ;; argument is given, it must be a symbol in the `abermud:word-type'
  ;; enumeration; if the parsed word is not of the specified type, the values
  ;; returned will be `#f' and the original `command' argument (as if nothing
  ;; could be parsed).

  (define* (next-word command #:optional type)
    (let-values (((word rest) (break-command command)))
      (if (> (string-length word) 0)
	  (let ((found-word (abermud:find-word word type)))
	    (if found-word (values found-word rest) (values #f command)))
	  (values #f command))))

  ;; Convenience function for parsing the next verb from the specified command
  ;; string.

  (define (next-verb command) (next-word command (abermud:word-type verb)))

  ;; Convenience function for parsing the next noun from the specified command
  ;; string.

  (define (next-noun command) (next-word command (abermud:word-type noun)))

  ;; Convenience function for parsing the next adjective from the specified 
  ;; command string.

  (define (next-adjective command)
    (next-word command (abermud:word-type adjective)))

  (define (read-item command contextual-items)
    (define (qualify noun contextual-items)
      (let loop ((contextual-items contextual-items))
	(and contextual-items
	     (let ((contextual-item (gzochi:car contextual-items)))
	       (if (abermud:word-equal? 
		    noun (abermud:item-noun contextual-item))
		   contextual-item
		   (loop (gzochi:cdr contextual-items)))))))

    (let*-values (((adj rest) (next-adjective command))
		  ((noun rest) (next-noun rest)))

      (values (cond (adj (abermud:find-item adj noun))
		    (contextual-items (qualify noun contextual-items))
		    (else (abermud:find-item noun)))
	      rest)))

  ;; Parses the contents of the `rest' argument to locate a target object to
  ;; pass as one of the arguments to the `get' function. The child items of the
  ;; specified player's parent room are used to qualify incompletely-specified
  ;; item names.

  (define (dispatch-get session player rest)
    (let ((local-items (abermud:item-children (abermud:item-parent player)))) 
      (let-values (((item rest) (read-item rest local-items)))
	(get session player item))))

  ;; Parses the contents of the `rest' argument to locate a target object to
  ;; pass as one of the arguments to the `drop' function. The items in the 
  ;; specified player's inventory are used to qualify incompletely-specified
  ;; item names.

  (define (dispatch-drop session player rest)
    (let ((inventory-items (abermud:item-children player)))
      (let-values (((item rest) (read-item rest inventory-items)))
	(drop session player item))))

  ;; Parses the contents of the `rest' argument to look up a target room to
  ;; pass as one of the arguments to the `abermud:enter' function.

  (define (dispatch-goto session player rest)
    (let-values (((item rest) (read-item rest #f)))
      (if (and item (abermud:as-room item))
	  (abermud:enter session item player)
	  (abermud:send-output session "I don't understand.\n"))))

  ;; The primary command dispatcher for gzochi abermud. Reads the next verb
  ;; from the string `command' and dispatches to the appropriate handler or
  ;; reports an error the specified `abermud:session' if a matching word cannot
  ;; be found.

  (define (abermud:dispatch-command session command)
    (define player (abermud:session-player session))

    (let-values (((head rest) (next-verb command)))
      (if head
	  (let ((command 
		 (string->symbol (string-downcase (abermud:word-text head)))))
	    (case command
	      ((inventory) (inventory session player))
	      ((look) (abermud:look session player))
	      ((goto) (dispatch-goto session player rest))
	      ((say) (say player rest))
	      ((take) (dispatch-get session player rest))	    
	      ((drop) (dispatch-drop session player rest))
	      ((quit) (quit-game session))
	      ((help) (show-help session))
	      
	      ((north east south west
		northeast southeast northwest southwest
		in out up down) (move session player command))
	      ((exits) (exits session player))
	    
	      (else (abermud:send-output 
		     session "Sorry, But I don't recognise that verb.\n"))))

	  (abermud:send-output session "What?\n"))))
)
