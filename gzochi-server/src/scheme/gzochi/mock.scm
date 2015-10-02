;; gzochi/mock.scm: Mock infrastructure for gzochi API
;; Copyright (C) 2015 Julian Graham
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

(library (gzochi mock)
  (export)
  (import (gzochi data mock))

  ;; This library has no exports; it exists solely to override some internal
  ;; wiring of the gzochid container to make unit testing gzochi application
  ;; Scheme code easier. Do not import this library into your application
  ;; directly! Doing so will likely break your application. Import this library
  ;; into test code only.
)
