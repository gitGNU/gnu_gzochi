;; maze.scm --- Maze-generation routines for gzochi mazewar example game

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

(library (gzochi example mazewar maze)
  (export mazewar:prim)
  (import (only (guile) random seed->random-state current-time)
	  (gzochi example mazewar data)
	  (rnrs))

  ;; The random state object to be used by maze generation routines.
  
  (define *mazewar-random-state* (seed->random-state (current-time)))

  (define (tile-at vec x y) (vector-ref (vector-ref vec y) x))
  (define (set-tile-at! vec x y tile) (vector-set! (vector-ref vec y) x tile))  
  (define (generative-tile-at vec x y)
    (define (make-empty-tile x y) 
      (mazewar:make-tile x y #f #f #f #f))
  
    (or (tile-at vec x y)
	(let ((tile (make-empty-tile x y))) (set-tile-at! vec x y tile) tile)))

  ;; Creates the maze vector and fills it with connected "wall" tiles.

  (define (make-empty-maze width height)
    (define vec (make-vector height #f))
    (let loop ((y (- height 1)))
      (or (< y 0) (begin (vector-set! vec y (make-vector width #f))
			 (loop (- y 1)))))

    (let loop ((y (- height 1)))
      (or (< y 0)
	  (begin 
	    (let loop2 ((x (- width 1)))
	      (or (< x 0)
		  (let ((tile (generative-tile-at vec x y)))		  
		    (if (> x 0)
			(let ((x1 (generative-tile-at vec (- x 1) y)))
			  (mazewar:set-tile-east! x1 tile)
			  (mazewar:set-tile-west! tile x1)))
		    (if (> y 0)
			(let ((y1 (generative-tile-at vec x (- y 1))))
			  (mazewar:set-tile-south! y1 tile)
			  (mazewar:set-tile-north! tile y1)))
		    (if (< x (- width 1))
			(let ((x1 (generative-tile-at vec (+ x 1) y)))
			  (mazewar:set-tile-west! x1 tile)
			  (mazewar:set-tile-east! tile x1)))
		    (if (< y (- height 1))
			(let ((y1 (generative-tile-at vec x (+ y 1))))
			  (mazewar:set-tile-north! y1 tile)
			  (mazewar:set-tile-south! tile y1)))
		    (loop2 (- x 1)))))
	    (loop (- y 1)))))
    vec)

  ;; Generates a new maze of the specified width and height, in the form of a 
  ;; vector of vectors of tile records. This implementation uses an algorithm 
  ;; developed by Robert C. Prim, based on Wikipedia's description here: 
  ;; http://en.wikipedia.org/wiki/Maze_generation_algorithms
  ;;
  ;; The general idea is to find adjacent "wall" tiles at random and convert
  ;; them to spaces, continuing until there are no walls thicker than a single
  ;; tile in more than one dimension.

  (define (mazewar:prim width height)
    (define maze (make-empty-maze width height))

    ;; Returns a space with an empty player field and all other fields set to
    ;; their values from the specified tile.

    (define (tile->space tile)
      (mazewar:make-space
       (mazewar:tile-x tile)
       (mazewar:tile-y tile)
       (mazewar:tile-north tile)
       (mazewar:tile-south tile)
       (mazewar:tile-east tile)
       (mazewar:tile-west tile)
       #f))

    ;; For a specified tile, find its adjacent tiles that have not yet been
    ;; converted to spaces (i.e., are still walls).

    (define (mutable-walls tile)
      (filter (lambda (tile-pair)
		(and tile-pair (not (mazewar:space? (cdr tile-pair)))))
	      (list (and (> (mazewar:tile-x tile) 1)
			 (cons tile (mazewar:tile-west tile)))
		    (and (> (mazewar:tile-y tile) 1)
			 (cons tile (mazewar:tile-north tile)))
		    (and (< (mazewar:tile-x tile) (- width 2))
			 (cons tile (mazewar:tile-east tile)))
		    (and (< (mazewar:tile-y tile) (- height 2))
			 (cons tile (mazewar:tile-south tile))))))

    ;; "Set" a tile in the maze, by adding it to the maze vector and updating
    ;; the adjacency fields of its neighbors.

    (define (install-tile! y-vec tile)
      (set-tile-at! y-vec (mazewar:tile-x tile) (mazewar:tile-y tile) tile)
      
      (if (mazewar:tile-north tile)
	  (mazewar:set-tile-south! (mazewar:tile-north tile) tile))
      (if (mazewar:tile-east tile)
	  (mazewar:set-tile-west! (mazewar:tile-east tile) tile))
      (if (mazewar:tile-south tile)
	  (mazewar:set-tile-north! (mazewar:tile-south tile) tile))
      (if (mazewar:tile-west tile)
	  (mazewar:set-tile-east! (mazewar:tile-west tile) tile)))
    
    (define starting-tile
      (tile->space 
       (tile-at maze 
		(+ (random (- width 2) *mazewar-random-state*) 1) 
		(+ (random (- height 2) *mazewar-random-state*) 1))))

    (define starting-walls (mutable-walls starting-tile))

    (define (prim tiles walls wall-count)
      (define (loop wall orientation walls wall-count)
	(let* ((opp (mazewar:next wall orientation)))	  
	  (if (memq opp tiles)
	      (prim tiles walls wall-count)
	      (let ((space1 (tile->space wall)))
		(install-tile! maze space1)
		(if (case orientation
		      ((north) (> (mazewar:tile-y opp) 0))
		      ((south) (< (mazewar:tile-y opp) (- height 1)))
		      ((east) (< (mazewar:tile-x opp) (- width 1)))
		      ((west) (> (mazewar:tile-x opp) 0)))

		    (let* ((space2 (tile->space opp))
			   (m-walls (mutable-walls space2)))
		      (install-tile! maze space2)
		      (prim (cons space1 (cons space2 tiles))
			    (append walls m-walls)
			    (+ wall-count (length m-walls))))
		    (prim (cons space1 tiles) walls wall-count))))))
	
      (if (not (null? walls))
	  (let* ((wall-pair 
		  (list-ref walls (random wall-count *mazewar-random-state*)))
		 (space (car wall-pair))
		 (wall (cdr wall-pair))
		 (walls (remq wall-pair walls))
		 (wall-count (- wall-count 1)))
	       
	    (cond ((< (mazewar:tile-x space) (mazewar:tile-x wall))
		   (loop wall (mazewar:orientation east) walls wall-count))
		  ((< (mazewar:tile-y space) (mazewar:tile-y wall))
		   (loop wall (mazewar:orientation south) walls wall-count))
		  ((> (mazewar:tile-x space) (mazewar:tile-x wall))
		   (loop wall (mazewar:orientation west) walls wall-count))
		  ((> (mazewar:tile-y space) (mazewar:tile-y wall))
		   (loop wall (mazewar:orientation north) walls wall-count))))))

    (install-tile! maze starting-tile)
    (prim (list starting-tile) starting-walls (length starting-walls))

    maze)
)
