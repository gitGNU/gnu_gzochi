;; gzochi/client/protocol.scm: Protocol constants for gzochi reference client
;; Copyright (C) 2014 Julian Graham
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

(library (gzochi client protocol)
  (export gzochi:protocol/login-request
	  gzochi:protocol/login-success
	  gzochi:protocol/login-failure

	  gzochi:protocol/logout-request
	  gzochi:protocol/logout-success

	  gzochi:protocol/session-disconnected
	  gzochi:protocol/session-message)
  (import (rnrs base))

  (define gzochi:protocol/login-request #x10)
  (define gzochi:protocol/login-success #x11)
  (define gzochi:protocol/login-failure #x12)

  (define gzochi:protocol/logout-request #x20)
  (define gzochi:protocol/logout-success #x21)

  (define gzochi:protocol/session-disconnected #x30)
  (define gzochi:protocol/session-message #x31)
)
