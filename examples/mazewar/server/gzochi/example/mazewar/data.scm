;; data.scm --- Data structures for gzochi mazewar example game

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

(library (gzochi example mazewar data)
  (export 
          ;; Tiles are the most basic units of a maze. They model the maze grid
          ;; itself and also serve as the "walls" between walkable spaces in
          ;; the maze.

          mazewar:make-tile mazewar:tile? mazewar:tile-x mazewar:tile-y 
	  mazewar:tile-north mazewar:tile-south mazewar:tile-east 
	  mazewar:tile-west mazewar:set-tile-north! mazewar:set-tile-south! 
	  mazewar:set-tile-east! mazewar:set-tile-west! mazewar:next 
	  mazewar:left mazewar:right

	  ;; A space is a sub-type of tile capable of holding a single player.

	  mazewar:space? mazewar:make-space mazewar:space-player 
	  mazewar:set-space-player!

	  ;; Players and missiles can be oriented in one of the four cardinal
	  ;; directions (north, south, east, and west). This enum definition
	  ;; provides some type safety around that concept.

	  mazewar:orientation
	  mazewar:make-orientation-set
	  mazewar:orientation-index

	  ;; Players are the user avatars in Mazewar.

	  mazewar:make-player mazewar:player? mazewar:player-space
	  mazewar:player-orientation mazewar:player-missile mazewar:player-name
	  mazewar:player-score mazewar:player-session 
	  mazewar:set-player-orientation! mazewar:set-player-missile!
	  mazewar:set-player-space! mazewar:set-player-score!

	  ;; Missiles are fired by players and travel in a straight line until
	  ;; they hit a non-space tile (i.e. a wall) or reach a space 
	  ;; containing a player.

	  mazewar:make-missile mazewar:missile? mazewar:missile-space
	  mazewar:set-missile-space! mazewar:missile-orientation
	  mazewar:missile-player
	  
	  ;; The maze structure stores some global state about the maze and
	  ;; current game, such as the current set of players and a serialized
	  ;; version of the maze to use for bootstrapping new clients.

	  mazewar:make-maze mazewar:maze? mazewar:maze-players 
	  mazewar:set-maze-players! mazewar:maze-spaces mazewar:maze-bytes)

  (import (gzochi data)
	  (gzochi io)
	  (rnrs))

  (define-enumeration mazewar:orientation 
    (north south east west) mazewar:make-orientation-set)
  (define mazewar:orientation-index 
    (enum-set-indexer (mazewar:make-orientation-set north south east west)))

  (gzochi:define-managed-record-type 
   (mazewar:tile mazewar:make-tile mazewar:tile?)

    (fields (immutable x (serialization gzochi:integer-serialization))
	    (immutable y (serialization gzochi:integer-serialization))

	    (mutable north mazewar:tile-north mazewar:set-tile-north!)
	    (mutable south mazewar:tile-south mazewar:set-tile-south!)
	    (mutable east mazewar:tile-east mazewar:set-tile-east!)
	    (mutable west mazewar:tile-west mazewar:set-tile-west!)))

  (gzochi:define-managed-record-type
   (mazewar:space mazewar:make-space mazewar:space?)
   
    (parent mazewar:tile)
    (fields (mutable player mazewar:space-player mazewar:set-space-player!)))

  ;; The following procedures perform relative orientation translations. The 
  ;; `left' procedure returns the cardinal direction to the left of the 
  ;; specified orientation.

  (define (mazewar:left orientation)
    (case orientation
      ((north) (mazewar:orientation west))
      ((south) (mazewar:orientation east))
      ((east) (mazewar:orientation north))
      ((west) (mazewar:orientation south))))

  ;; The `right' procedure returns the cardinal direction to the right of the
  ;; specified orientation..

  (define (mazewar:right orientation)
    (case orientation
      ((north) (mazewar:orientation east))
      ((south) (mazewar:orientation west))
      ((east) (mazewar:orientation south))
      ((west) (mazewar:orientation north))))

  (define (mazewar:next space orientation)
    (case orientation
      ((north) (mazewar:tile-north space))
      ((south) (mazewar:tile-south space))
      ((east) (mazewar:tile-east space))
      ((west) (mazewar:tile-west space))))

  (gzochi:define-managed-record-type 
   (mazewar:player mazewar:make-player mazewar:player?)

    (fields (mutable space mazewar:player-space mazewar:set-player-space!)
	    (mutable orientation
		     mazewar:player-orientation
		     mazewar:set-player-orientation!
		     (serialization gzochi:symbol-serialization))
	    (mutable missile
		     mazewar:player-missile 
		     mazewar:set-player-missile!)
	    (mutable score mazewar:player-score mazewar:set-player-score!
		     (serialization gzochi:integer-serialization))

	    (immutable name (serialization gzochi:string-serialization))
	    session))

  (gzochi:define-managed-record-type 
   (mazewar:missile mazewar:make-missile mazewar:missile?)

    (fields (immutable orientation (serialization gzochi:symbol-serialization))
	    (immutable player)
	    
	    (mutable space mazewar:missile-space mazewar:set-missile-space!)))

  (gzochi:define-managed-record-type 
   (mazewar:maze mazewar:make-maze mazewar:maze?)
   
    (fields (immutable spaces)
	    (immutable bytes (serialization gzochi:bytevector-serialization))
    
            (mutable players mazewar:maze-players mazewar:set-maze-players!)))

)
