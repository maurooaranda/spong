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
;; along with this S-Pong.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:com.the-blackbeard.spong)

;; An audio resource class to group some logic.
(defclass audio-fx ()
  ((fx :initarg :fx-name :initform (error "Must provide a fx name.")
       :accessor fx-name)
   (resource :initarg :resource :initform (error "Must provide a resource.")
	     :accessor resource)))
