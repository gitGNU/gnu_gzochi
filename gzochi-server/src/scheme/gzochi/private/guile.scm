;; gzochi/private/guile.scm: GNU Guile interface support code for gzochid 
;; Copyright (C) 2013 Julian Graham
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

(library (gzochi private guile)
  (export gzochi:add-to-load-path 
	  gzochi:exception-printer 
	  gzochi:make-handler 
	  gzochi:make-thunk)
  (import (only (guile) add-to-load-path format newline filter)
	  (rnrs))

  (define (gzochi:add-to-load-path path)
    (add-to-load-path path))

  (define (gzochi:exception-printer port condition)
    (let ((components (simple-conditions condition)))
      (if (null? components)
          (format port "Empty condition object")
          (let loop ((i 1) (components components))
            (cond ((pair? components)
                   (format port "  ~a. " i)
                   (format-simple-condition port (car components))
		   (newline port)
                   (loop (+ i 1) (cdr components))))))))

  (define (format-simple-condition port condition)
    (define (print-rtd-fields rtd field-names)
      (let ((n-fields (vector-length field-names)))
        (do ((i 0 (+ i 1)))
            ((>= i n-fields))
          (format port "      ~a: ~s"
                  (vector-ref field-names i)
                  ((record-accessor rtd i) condition))
          (unless (= i (- n-fields 1))
            (newline port)))))
    (let ((condition-name (record-type-name (record-rtd condition))))
      (let loop ((rtd (record-rtd condition))
                 (rtd.fields-list '())
                 (n-fields 0))
        (cond (rtd
               (let ((field-names (record-type-field-names rtd)))
                 (loop (record-type-parent rtd)
                       (cons (cons rtd field-names) rtd.fields-list)
                       (+ n-fields (vector-length field-names)))))
              (else
               (let ((rtd.fields-list
                      (filter (lambda (rtd.fields)
                                (not (zero? (vector-length (cdr rtd.fields)))))
                              (reverse rtd.fields-list))))
                 (case n-fields
                   ((0) (format port "~a" condition-name))
                   ((1) (format port "~a: ~s"
                                condition-name
                                ((record-accessor (caar rtd.fields-list) 0)
                                 condition)))
                   (else
                    (format port "~a:\n" condition-name)
                    (let loop ((lst rtd.fields-list))
                      (when (pair? lst)
                        (let ((rtd.fields (car lst)))
                          (print-rtd-fields (car rtd.fields) (cdr rtd.fields))
                          (when (pair? (cdr lst))
                            (newline port))
                          (loop (cdr lst)))))))))))))
  
  (define (gzochi:make-handler procedure exception-var)
    (lambda (ex) (procedure exception-var ex)))

  (define (gzochi:make-thunk procedure args)
    (lambda () (apply procedure args)))
)
