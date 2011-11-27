;; gzochi/io.scm: Public exports for common I/O and serialization procedures
;; Copyright (C) 2011 Julian Graham
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

(library (gzochi io)
  (export gzochi:read-integer
	  gzochi:read-boolean
	  gzochi:read-string
	  gzochi:read-symbol

	  gzochi:write-integer
	  gzochi:write-boolean
	  gzochi:write-string
	  gzochi:write-symbol

	  gzochi:integer-serialization
	  gzochi:boolean-serialization
	  gzochi:string-serialization
	  gzochi:symbol-serialization
	  
	  gzochi:make-uniform-list-serialization

	  gzochi:serialization
	  gzochi:serialization?
	  gzochi:make-serialization
	  gzochi:serialization-serializer
	  gzochi:serialization-deserializer)

  (import (rnrs))

  (define-record-type (gzochi:serialization 
		       gzochi:make-serialization
		       gzochi:serialization?)
    (fields serializer deserializer))

  (define (read-varint port)
    (define (read-varint-inner port tally septets)
      (let* ((b (get-u8 port))
             (tally (bitwise-ior (bitwise-arithmetic-shift-left 
                                  (bitwise-bit-field b 0 7) (* septets 7)) 
                                 tally)))
        (if (bitwise-bit-set? b 7)
            (read-varint-inner port tally (+ septets 1))
            tally)))
    (read-varint-inner port 0 0))
  
  (define (write-varint port varint)
    (let ((b (bitwise-bit-field varint 0 7)))
      (if (> varint 127)
          (begin (put-u8
		  port (bitwise-ior (bitwise-arithmetic-shift-left 1 7) b))
                 (write-varint port (bitwise-arithmetic-shift-right varint 7)))
          (put-u8 port b))))

  (define gzochi:read-integer read-varint)
  (define gzochi:write-integer write-varint)
  (define gzochi:integer-serialization 
    (gzochi:make-serialization gzochi:write-integer gzochi:read-integer))

  (define (gzochi:read-boolean port) (if (eqv? (read-varint port) 0) #f #t))
  (define (gzochi:write-boolean port val) (write-varint port (if val 1 0)))
  (define gzochi:boolean-serialization
    (gzochi:make-serialization gzochi:write-boolean gzochi:read-boolean))

  (define (gzochi:read-string port)
    (utf8->string (get-bytevector-n port (read-varint port))))
  (define (gzochi:write-string port str)
    (write-varint port (string-length str))
    (put-bytevector port (string->utf8 str)))
  (define gzochi:string-serialization
    (gzochi:make-serialization gzochi:write-string gzochi:read-string))
 
  (define (gzochi:read-symbol port) (string->symbol (gzochi:read-string port)))
  (define (gzochi:write-symbol port sym) 
    (gzochi:write-string port (symbol->string sym)))
  (define gzochi:symbol-serialization
    (gzochi:make-serialization gzochi:write-symbol gzochi:read-symbol))

  (define (gzochi:make-uniform-list-serialization serialization)
    (define serializer (gzochi:serialization-serializer serialization))
    (define deserializer (gzochi:serialization-deserializer serialization))

    (gzochi:make-serialization
     (lambda (port lst)
       (gzochi:write-integer port (length lst))
       (for-each (lambda (l) (serializer port l)) lst))
     (lambda (port)
       (let ((lst-length (gzochi:read-integer port)))
	 (let loop ((i 0) (lst '()))
	   (if (< i lst-length)
	       (loop (+ i 1) (cons (deserializer port) lst))
	       (reverse lst)))))))
)
