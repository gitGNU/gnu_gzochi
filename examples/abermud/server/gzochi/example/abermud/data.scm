;; data.scm --- Data structures for gzochi AberMUD example game

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

(library (gzochi example abermud data)
  (export abermud:word-type
      
	  abermud:direction
	  abermud:make-direction-set
 
          abermud:make-word
	  abermud:word-text
	  abermud:word-word-type
	  abermud:add-word!
	  abermud:find-words
	  abermud:find-word
	  abermud:find-noun
	  abermud:find-adjective
	  abermud:word-equal?

	  abermud:make-term
	  abermud:term?
	  abermud:term-adjective
	  abermud:term-noun

          abermud:make-item
	  abermud:item-adjective
	  abermud:item-children
	  abermud:item-noun
	  abermud:item-parent
	  abermud:item-name

	  abermud:add-item!
	  abermud:find-item
	  abermud:link-item!
	  abermud:unlink-item!

	  abermud:set-item-parent!

	  abermud:player
	  abermud:make-player
	  abermud:player?
	  abermud:player-name
	  abermud:player-password
	  abermud:set-player-password!

	  abermud:room
	  abermud:make-room
	  abermud:room?
	  abermud:room-short-description
	  abermud:room-long-description
	  abermud:room-channel
	  abermud:room-exits
	  abermud:room-exit
	  abermud:add-room-exit!

	  abermud:exit
	  abermud:exit-message
	  abermud:exit-direction
	  abermud:exit-destination
	  abermud:make-exit

	  abermud:set-exit-destination!

	  abermud:object
	  abermud:make-object
	  abermud:object?
	  abermud:object-text
	  abermud:object-flags
	  abermud:object-flag

	  abermud:inouthere
	  abermud:make-inouthere
	  abermud:inouthere?
	  abermud:inouthere-in
	  abermud:inouthere-out
	  abermud:inouthere-here

	  abermud:add-facet!
	  abermud:as-facet
	  abermud:as-inouthere
	  abermud:as-object
	  abermud:as-player
	  abermud:as-room)

  (import (only (guile) gensym)
	  (gzochi)
	  (gzochi data)
	  (ice-9 optargs)
	  (rnrs)
	  (srfi :1))

  ;; Enumeration of word types: Nouns, verbs, and adjectives.

  (define-enumeration abermud:word-type 
    (noun verb adjective) 
    abermud:make-word-type-set)

  ;; Enumeration of flags for object facets. A `flannel' is an object whose
  ;; description is merged into the overall description of a room. Only objects
  ;; whose flags include `can-get' may be picked up by players.

  (define-enumeration abermud:object-flag 
    (flannel can-get) 
    abermud:make-object-flag-set)

  ;; Enumeration of the possible directions of player movement.
  
  (define-enumeration abermud:direction
    (north 
     east 
     south 
     west 
     up 
     down 
     northeast 
     southeast 
     northwest 
     southwest 
     in 
     out)
    abermud:make-direction-set)
  
  ;; The word type. This record type is not managed because unqiue instances of
  ;; it do not need to be tracked and they are immutable once created.

  (define-record-type (abermud:word abermud:make-word abermud:word?)
    (fields text word-type))

  ;; ...However, words do need to be stored as fields in other, managed record
  ;; types. These are the serialization helpers for the `abermud:word' type.

  (define (abermud:write-word port word)
    (if word
	(begin
	  (gzochi:write-boolean port #t)
	  (gzochi:write-string port (abermud:word-text word))
	  (gzochi:write-symbol port (abermud:word-word-type word)))
	(gzochi:write-boolean port #f)))

  (define (abermud:read-word port)
    (and (gzochi:read-boolean port)
	 (abermud:make-word (gzochi:read-string port)
			    (gzochi:read-symbol port))))

  (define abermud:word-serialization 
    (gzochi:make-serialization abermud:write-word abermud:read-word))

  ;; Custom hash and equality functions for the `abermud:word' type.

  (define (abermud:word-hash word) 
    (string-hash (abermud:word-text word)))
  
  (define (abermud:word-equal? w1 w2)    
    (and (abermud:word? w1)
	 (abermud:word? w2)
	 (equal? (abermud:word-text w1) (abermud:word-text w2))
	 (eq? (abermud:word-word-type w1) (abermud:word-word-type w2))))

  ;; Convenience function to wrap a word with a `gzochi:managed-serializable'
  ;; wrapper so that it can be treated as a managed record, attaching the
  ;; required serialization helpers.

  (define (wrap-word word)
    (gzochi:make-managed-serializable
     word 
     (gzochi:make-callback 'abermud:write-word '(gzochi example abermud data))
     (gzochi:make-callback 'abermud:read-word '(gzochi example abermud data))))

  ;; Returns the table of known words (keyed by word text) as a 
  ;; `gzochi:managed-hashtable' record, lazily initializing it if it is not
  ;; already bound.
  
  (define (get-words)
    (guard (ex ((gzochi:name-not-bound-condition? ex)
		(let ((words
		       (gzochi:make-managed-hashtable
			(gzochi:make-callback 
			 'string-ci-hash '(rnrs hashtables))
			(gzochi:make-callback 'string-ci=? '(rnrs unicode)))))
		  (gzochi:set-binding! "words" words)
		  words)))

      (gzochi:get-binding "words")))

  ;; Adds a new word to the word table. The word table is keyed by the word
  ;; text; the values are managed pairs that form a managed list of words that
  ;; share the same text but may have different word types. E.g., "oak" is both
  ;; a noun and an adjective in the default AberMUD universe.

  (define (abermud:add-word! word) 
    (let* ((text (abermud:word-text word))
	   (words (get-words))
	   (word-list
	    (if (gzochi:managed-hashtable-contains? words text)
		(let ((lst (gzochi:managed-hashtable-ref words text #f)))
		  (gzochi:cons (wrap-word word) lst))
		(gzochi:managed-list (wrap-word word)))))

      (gzochi:managed-hashtable-set!
       words text word-list
       #:key-serializer 
       (gzochi:make-callback 'gzochi:write-string '(gzochi io)) 
       #:key-deserializer
       (gzochi:make-callback 'gzochi:read-string '(gzochi io)))))

  (define (find-words words text)
    (gzochi:managed-hashtable-ref words text #f))

  ;; Finds all of the words associated with the specified word text. There may
  ;; be zero, one, or many, depending on how many word types share the 
  ;; specified text.

  (define (abermud:find-words text)
    (let ((word-list (find-words (get-words) text)))
      (and word-list 
	   (map gzochi:managed-serializable-value 
		(gzochi:managed-list->list word-list)))))

  ;; Returns the a word structure that matches the specified text and word 
  ;; type, or `#f' if one has not been registered.

  (define (abermud:find-word text type)
    (let ((word-list (find-words (get-words) text)))
      (let loop ((word-list word-list))
	(and word-list
	     (let ((word 
		    (gzochi:managed-serializable-value (gzochi:car word-list))))
	       (if (eq? (abermud:word-word-type word) type)
		   word (loop (gzochi:cdr word-list))))))))

  ;; Convenience function to find in the word table the adjective with the 
  ;; specified text.

  (define (abermud:find-adjective text)
    (abermud:find-word text (abermud:word-type adjective)))

  ;; Convenience function to find in the word table the verb with the specified
  ;; text.
  
  (define (abermud:find-noun text)
    (abermud:find-word text (abermud:word-type noun)))

  ;; Nouns with an optional adjective are called "terms" in gzochi abermud, and
  ;; can be used as unique identifiers for game items. The following functions
  ;; support various operations over terms: Serialization, hashing, and
  ;; equality testing.

  (define-record-type (abermud:term abermud:make-term abermud:term?)
    (fields adjective noun)
    (protocol (lambda (p)
		(lambda* (word1 #:optional word2)
		  (if word2 (p word1 word2) (p #f word1))))))

  (define (abermud:write-term port term)
    (if (abermud:term-adjective term)
	(begin 
	  (gzochi:write-boolean port #t)
	  (gzochi:write-string port
	   (abermud:word-text (abermud:term-adjective term))))
	(gzochi:write-boolean port #f))
    (gzochi:write-string port (abermud:word-text (abermud:term-noun term))))
  
  (define (abermud:read-term port)
    (if (gzochi:read-boolean port)
	(abermud:make-term 
	 (abermud:make-word 
	  (gzochi:read-string port) (abermud:word-type adjective))
	 (abermud:make-word 
	  (gzochi:read-string port) (abermud:word-type noun)))
	(abermud:make-term 
	 (abermud:make-word 
	  (gzochi:read-string port) (abermud:word-type noun)))))

  (define (abermud:term-hash term)    
    (if (abermud:term-adjective term)
	(bitwise-ior (bitwise-arithmetic-shift-left
		      (abermud:word-hash (abermud:term-adjective term)) 32)
		     (abermud:word-hash (abermud:term-noun term)))
	(abermud:word-hash (abermud:term-noun term))))

  (define (abermud:term-equal? term1 term2)
    (and (abermud:term? term1)
	 (abermud:term? term2)
	 (or (and (not (abermud:term-adjective term1))
		  (not (abermud:term-adjective term2)))
	     (abermud:word-equal? 
	      (abermud:term-adjective term1) (abermud:term-adjective term2)))
	 (abermud:word-equal? 
	  (abermud:term-noun term1) (abermud:term-noun term2))))

  ;; The base game item type for gzochi abermud. Everything in the game world
  ;; is an item, including players: Finer-grained properties and type 
  ;; polymorphism are implemented by way of zero or more "facets" to which a
  ;; particular item instance may be "cast" (see `abermud:as-facet'). Game
  ;; items have links to parent and child items, and optionally include a noun
  ;; (or a noun-and-adjective combination) that uniquely identifies them.

  (gzochi:define-managed-record-type 
   (abermud:item abermud:make-item abermud:item?)

   (fields (immutable adjective (serialization abermud:word-serialization))
	   (immutable noun (serialization abermud:word-serialization))

           (mutable parent abermud:item-parent abermud:set-item-parent!)
	   (mutable children abermud:item-children abermud:set-item-children!)
	   (mutable next abermud:item-next abermudmud:set-item-next!)
	   (immutable facets))

   (nongenerative abermud:item)
   (protocol (lambda (n)
	       (lambda (adjective noun)
		 (let ((p (n)))
		   (p adjective noun #f #f #f
		      (gzochi:make-managed-vector 10)))))))

  ;; Add `child' to the set of children of `parent' and make `parent' the
  ;; parent item of `child'.

  (define (abermud:link-item! parent child)
    (abermud:set-item-children! 
     parent (gzochi:cons child (abermud:item-children parent)))
    (abermud:set-item-parent! child parent))

  ;; Remove `child' from the set of children of its parent and set its parent
  ;; to `#f'.

  (define (abermud:unlink-item! child)
    (let ((old-parent (abermud:item-parent child)))
      (if old-parent
	  (abermud:set-item-children!
	   old-parent (gzochi:list->managed-list 
		       (remq child (gzochi:managed-list->list 
				    (abermud:item-children old-parent)))))))
    (abermud:set-item-parent! child #f))

  ;; Returns a formatted string containing the adjective-noun pair for the
  ;; specified item.

  (define (abermud:item-name item)
    (let ((adj (abermud:item-adjective item))
	  (noun (abermud:item-noun item)))
      (if adj
	  (string-append (abermud:word-text adj) " " (abermud:word-text noun))
	  (abermud:word-text noun))))

  ;; Returns the table of known items (keyed by term) as a 
  ;; `gzochi:managed-hashtable' record, lazily initializing it if it is not
  ;; already bound.
  
  (define (get-items)
    (guard (ex ((gzochi:name-not-bound-condition? ex)
		(let ((items 
		       (gzochi:make-managed-hashtable
			(gzochi:make-callback 
			 'abermud:term-hash '(gzochi example abermud data))
			(gzochi:make-callback 
			 'abermud:term-equal? 
			 '(gzochi example abermud data)))))
		  (gzochi:set-binding! "items" items)
		  items)))

      (gzochi:get-binding "items")))

  ;; Adds a new item to the item table. The item table is keyed by 
  ;; `abermud:term' records, and the `abermud:word' arguments to this function
  ;; are used to form a term key. If `word2' is given, it must be a noun and
  ;; `word1' must be an adjective; otherwise, `word1' must be a noun.

  (define* (abermud:add-item! item word1 #:optional word2)
    (let ((term (abermud:make-term word1 word2))
	  (items (get-items)))
      (if (gzochi:managed-hashtable-contains? items term)
	  (raise (make-assertion-violation))
	  (gzochi:managed-hashtable-set! 
	   items term item 
	   #:key-serializer 
	   (gzochi:make-callback 'abermud:write-term 
				 '(gzochi example abermud data)) 
	   #:key-deserializer 
	   (gzochi:make-callback 'abermud:read-term 
				 '(gzochi example abermud data))))))

  ;; Returns the item identified by the specified words. If `word2' is given,
  ;; it must be a noun and `word1' must be an adjective; otherwise, `word1' 
  ;; must be noun.

  (define* (abermud:find-item word1 #:optional word2)
    (gzochi:managed-hashtable-ref 
     (get-items) (abermud:make-term word1 word2) #f))
    
  ;; The following managed record type definitions model facets, additional 
  ;; data types that can be associated with `abermud:item' records.

  ;; The presence descriptor facet. Includes (optionally empty) string fields
  ;; to use as alternatives to the system's default description string when the
  ;; item arrives at the same location as the player; departs from the player's
  ;; location; or is present at the player's location, respectivey.

  (gzochi:define-managed-record-type
   (abermud:inouthere abermud:make-inouthere abermud:inouthere?)
   (fields (immutable in (serialization gzochi:string-serialization))
	   (immutable out (serialization gzochi:string-serialization))
	   (immutable here (serialization gzochi:string-serialization)))
   (nongenerative abermud:inouthere))	   

  ;; The object facet. Represents a non-player object in the gzochi abermud
  ;; game world. Includes a text description and a set of flags that describe
  ;; some additional properties, in the form of a list of symbols. E.g., only 
  ;; objects with the `can-get' flag can be picked up by players.

  (gzochi:define-managed-record-type
   (abermud:object abermud:make-object abermud:object?)   
   (fields (immutable text (serialization gzochi:string-serialization))
	   (immutable flags (serialization 
			     (gzochi:make-uniform-list-serialization 
			      gzochi:symbol-serialization))))
   (nongenerative abermud:object))

  ;; The player facet. Represents a player in the gzochi abermud game world,
  ;; independent of whether an active gzochi client session is "attached" to
  ;; them.

  (gzochi:define-managed-record-type 
   (abermud:player abermud:make-player abermud:player?)
   
   (fields (immutable name (serialization gzochi:string-serialization))
	   (mutable password
		    abermud:player-password 
		    abermud:set-player-password!
		    (serialization gzochi:string-serialization)))

   (nongenerative abermud:player)
   (protocol (lambda (n) (lambda (name) (let ((p (n))) (p name ""))))))

  ;; The room facet. Rooms act as containers (via their parent-child 
  ;; relationships) with other items in the game world. A room has a short text
  ;; description, which is displayed in the title bar for clients who are
  ;; currently in the room; a long text description, which is displayed to 
  ;; clients whose associated players enter the roomm; a gzochi channel that is
  ;; used to broadcast visible events to all members of the room; and a vector
  ;; of exit descriptors that connect the room to other items.

  (gzochi:define-managed-record-type
   (abermud:room abermud:make-room abermud:room?)

   (fields (immutable short-description
		      (serialization gzochi:string-serialization))
	   (immutable long-description
		      (serialization gzochi:string-serialization))
	   channel
	   exits)
   (nongenerative abermud:room)
   (protocol (lambda (n)
	       (lambda (short-description long-description)
		 (let ((p (n)))
		   (p short-description 
		      long-description 
		      (gzochi:create-channel (symbol->string (gensym)))
		      (gzochi:make-managed-vector 12)))))))
  
  ;; Convert the symbolic representation of an exit direction to the index of
  ;; a directional exit in a room facet's exit vector.

  (define (direction->exit-index direction)
    (case direction
      ((north) 0)
      ((east) 1)
      ((south) 2)
      ((west) 3)
      ((up) 4) 
      ((down) 5)
      ((northeast) 6)
      ((southeast) 7)
      ((northwest) 8)
      ((southwest) 9)
      ((in) 10)
      ((out) 11)
      (else (raise (make-assertion-violation)))))

  ;; Models an exit from an `abermud:room' facet. Exits have a destination, in
  ;; the form of a reference to another `abermud:item', and a text message,
  ;; optionally empty, which is displayed to clients as they exit.

  (gzochi:define-managed-record-type 
   (abermud:exit abermud:make-exit abermud:exit?)

   (fields (immutable destination)
	   (immutable message (serialization gzochi:string-serialization)))

   (nongenerative abermud:exit)
   (protocol (lambda (n)
	       (lambda* (destination #:optional message)
		 (let ((p (n))) (p destination (or message "")))))))

  ;; Returns the exit record for the specified room going in the specified
  ;; direction, or `#f' if no such exit exists.

  (define (abermud:room-exit room direction)
    (gzochi:managed-vector-ref
     (abermud:room-exits room) (direction->exit-index direction)))
	  
  ;; Adds an exit to the specified room facet whose destination (by way of
  ;; the specified direction) is the specified `abermud:item' item. If the
  ;; optional `text' argument is specified, it must be a string giving the
  ;; exit message for the new exit.

  (define* (abermud:add-room-exit! room direction item #:optional text)
    (gzochi:managed-vector-set!
     (abermud:room-exits room) 
     (direction->exit-index direction) 
     (abermud:make-exit item text)))

  ;; Convert a facet type, in the form of a managed record type descriptor, to
  ;; the index used to store that facet in an `abermud:item' record's facet
  ;; vector.

  (define (facet-type->index facet-type)
    (let ((uid (gzochi:managed-record-type-uid facet-type)))
      (case uid
	((abermud:room) 0)
	((abermud:object) 1)
	((abermud:player) 2)
	((abermud:inouthere) 3)
	(else (raise (condition 
		      (make-message-condition 
		       (string-append 
			"Unsupported facet type " (symbol->string uid)))
		      (make-assertion-violation)))))))

  ;; Adds the specified facet to the specified `abermud:item' record.
  
  (define (abermud:add-facet! item facet)
    (or (gzochi:managed-record? facet)
	(raise (condition 
		(make-message-condition "Item facets must be managed.")
		(make-assertion-violation))))

    (let* ((facets (abermud:item-facets item))
	   (facet-type (gzochi:managed-record-rtd facet))
	   (index (facet-type->index facet-type)))
      (if (gzochi:managed-vector-ref facets index)
	  (raise (condition
		  (make-message-condition 
		   (string-append 
		    "Facet of type " 
		    (gzochi:managed-record-type-name facet-type) 
		    " already present."))
		  (make-assertion-violation))))

      (gzochi:managed-vector-set! facets index facet)))
  
  ;; "Casts" the specified item to the specified facet type.

  (define (abermud:as-facet item facet-type)
    (gzochi:managed-vector-ref 
     (abermud:item-facets item) (facet-type->index facet-type)))

  ;; Convenience function for accessing the presence description facet of an 
  ;; item.

  (define (abermud:as-inouthere item)
    (abermud:as-facet item abermud:inouthere))

  ;; Convenience function for accessing the "object" facet of an item.

  (define (abermud:as-object item)
    (abermud:as-facet item abermud:object))

  ;; Convenience function for accessing the "player" facet of an item.

  (define (abermud:as-player item)
    (abermud:as-facet item abermud:player))

  ;; Convenience function for accessing the "room" facet of an item.
  
  (define (abermud:as-room item)
    (abermud:as-facet item abermud:room))
)
