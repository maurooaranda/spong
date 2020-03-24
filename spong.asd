;; Copyright (C) 2020  Mauro Aranda

;; S-Pong is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; S-Pong is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with S-Pong.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :com.the-blackbeard.spong-system (:use :asdf :cl))
(in-package :com.the-blackbeard.spong-system)

(defsystem #:spong
  :name "S-pong"
  :author "Mauro Aranda <maurooaranda@gmail.com"
  :version "1.0"
  :maintainer "Mauro Aranda <maurooaranda@gmail.com"
  :licence "GPLv3"
  :description "A Pong implementation in Common Lisp"
  :long-description ""
  :depends-on (:lispbuilder-sdl :lispbuilder-sdl-mixer :lispbuilder-sdl-ttf)
  :components ((:file "packages")
	       (:file "random-x" :depends-on ("packages"))
	       (:file "globals" :depends-on ("packages"))
	       (:file "shape" :depends-on ("packages"))
	       (:file "rectangle" :depends-on ("packages" "shape"))
	       (:file "audio-res" :depends-on ("packages"))
	       (:file "sprite" :depends-on ("packages"))
	       (:file "ball" :depends-on ("packages" "rectangle" "globals"
						     "random-x"))
	       (:file "paddle" :depends-on ("packages" "rectangle"))
	       (:file "player" :depends-on ("packages" "paddle" "globals"
						       "ball"))
	       (:file "powerup" :depends-on ("packages" "sprite" "player"))
	       (:file "spong" :depends-on ("packages" "rectangle" "audio-res"
						      "sprite" "powerup"))))

