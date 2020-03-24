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

(in-package #:com.the-blackbeard.spong)

;;; The S-Pong Paddle class.
(defclass paddle (rectangle)
  ((y :initform (/ *window-height* 2))
   (width :initform *paddle-width*) (height :initform *paddle-height*)
   (acceleration :initarg :speed :initform *paddle-acceleration*
		 :accessor acceleration)
   (vel-x :initarg :dx :initform 0 :accessor vel-x)
   (vel-y :initarg :dy :initform 0 :accessor vel-y)
   (color :initform sdl:*white*)
   (serve-op :initarg :serve-op :accessor serve-op)))

(defmethod accelerate ((paddle paddle) direction)
  (setf (vel-y paddle) (cond
			 ((eq direction :up) (- (acceleration paddle)))
			 ((eq direction :down) (acceleration paddle))
			 (t 0))))

(defmethod update ((paddle paddle) dt)
  ;; Adjust in case of paddle hitting the wall.
  (setf (pos-y paddle) (round
			(if (< (vel-y paddle) 0)
			    (max 0 (+ (pos-y paddle) (* (vel-y paddle) dt)))
			    (min (- *window-height* (height paddle))
				 (+ (pos-y paddle) (* (vel-y paddle) dt)))))))
