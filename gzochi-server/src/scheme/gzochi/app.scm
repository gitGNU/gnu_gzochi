;; gzochi/app.scm: Public exports for general gzochi application support 
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

(library (gzochi app)
  (export gzochi:make-callback
	  gzochi:callback?
	  gzochi:callback-module
	  gzochi:callback-procedure
	  gzochi:callback-data)

  (import (rnrs)
	  (gzochi data)
	  (gzochi io))
  
  (gzochi:define-managed-record-type 
    (gzochi:callback gzochi:make-callback gzochi:callback?)
    
    (fields (immutable procedure (serialization gzochi:symbol-serialization))
	    (immutable module (serialization
			       (gzochi:make-uniform-list-serialization 
				gzochi:symbol-serialization)))
	    (immutable data))
    (nongenerative gzochi:callback)
    (protocol 
     (lambda (n)
       (lambda (procedure module . args)
	 (let ((p (n))) 
	   (if (null? args)
	       (p procedure module #f)
	       (let ((arg (car args)))
		 (or (and (gzochi:managed-record? arg) (null? (cdr args)))
		     (raise
		      (condition
		       (make-assertion-violation)
		       (make-message-condition
			"Callback data must be a single managed record or #f."
			))))
		 (p procedure module arg)))))))
    (sealed #t))
)
