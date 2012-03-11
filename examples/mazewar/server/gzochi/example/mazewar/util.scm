;; util.scm --- Utility functions for gzochi mazewar example game

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

(library (gzochi example mazewar util)
  (export bytevector-concat)
  (import (rnrs base)
	  (rnrs bytevectors)
	  (srfi :1))

  ;; Returns a new bytevector whose length is the sum of the lengths of its
  ;; arguments and whose contents are their contents concatenated in order.

  (define (bytevector-concat . bytevectors)
    (let* ((bytevector-lengths (map bytevector-length bytevectors))
	   (dest-len (fold + 0 bytevector-lengths))
	   (dest (make-bytevector dest-len)))
      (let loop ((bytevectors bytevectors)
		 (bytevector-lengths bytevector-lengths)
		 (offset 0))
	(if (null? bytevectors)
	    dest
	    (let ((len (car bytevector-lengths)))
	      (bytevector-copy! (car bytevectors) 0 dest offset len)
	      (loop (cdr bytevectors) 
		    (cdr bytevector-lengths) 
		    (+ offset len)))))))
)
