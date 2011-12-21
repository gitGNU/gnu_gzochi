;; gzochi/private/channel.scm: Private infrastructure for gzochi channel API 
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

(library (gzochi private channel)
  (export gzochi:create-channel
	  gzochi:get-channel

          gzochi:make-channel
	  gzochi:channel?
	  gzochi:channel-name
	  gzochi:channel-oid

	  gzochi:join-channel
	  gzochi:leave-channel
	  gzochi:close-channel
	  gzochi:send-channel-message)

  (import (gzochi client)
	  (gzochi conditions)
	  (gzochi data)
	  (gzochi io)
	  (rnrs))

  (gzochi:define-managed-record-type
   (gzochi:channel gzochi:make-channel gzochi:channel?)
   
   (nongenerative gzochi:channel)
   (fields (immutable oid (serialization gzochi:integer-serialization))
	   (immutable name (serialization gzochi:string-serialization))))

  (define (gzochi:create-channel name)
    (or (string? name) 
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition name))))

    (and (primitive-get-channel name)
	 (raise (gzochi:make-name-exists-condition name)))
    (primitive-create-channel name))

  (define (gzochi:get-channel name)
    (or (string? name) 
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition name))))

    (or (primitive-get-channel name) 
	(raise (gzochi:make-name-not-bound-condition name))))

  (define (gzochi:join-channel channel session)
    (or (gzochi:channel? channel)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))
    (or (gzochi:client-session? session)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))

    (primitive-join-channel channel session))

  (define (gzochi:leave-channel channel session)
    (or (gzochi:channel? channel)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))
    (or (gzochi:client-session? session)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))

    (primitive-leave-channel channel session))

  (define (gzochi:send-channel-message channel msg)
    (or (gzochi:channel? channel)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))
    (or (bytevector? msg)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition msg))))
    
    (primitive-send-channel-message channel msg))

  (define (gzochi:close-channel channel)
    (or (gzochi:channel? channel)
	(raise (condition (make-assertion-violation)
			  (make-irritants-condition channel))))

    (primitive-close-channel channel))
  
  (define primitive-create-channel #f)
  (define primitive-get-channel #f)
  (define primitive-join-channel #f)
  (define primitive-leave-channel #f)
  (define primitive-send-channel-message #f)
  (define primitive-close-channel #f)
)
