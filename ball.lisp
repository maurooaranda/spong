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

;;; The S-Pong Ball class.
(defclass spong-ball (square)
  ((x :initform (- (/ *window-width* 2) *ball-length*))
   (y :initform (- (/ *window-height* 2) *ball-length*))
   (vel-x :initarg :dx :initform (random-negate *ball-initial-x-speed*)
	  :accessor vel-x)
   (vel-y :initarg :dy :initform (random-sim 50) :accessor vel-y)
   (color :initform sdl:*white*)))

(defmethod reset ((ball spong-ball))
  (setf (pos-x ball) (/ *window-width* 2))
  (setf (pos-y ball) (/ *window-height* 2))
  (setf (vel-x ball) (random-negate *ball-initial-x-speed*))
  (setf (vel-y ball) (random-sim 50)))

(defmethod update ((ball spong-ball) dt)
  (setf (pos-x ball) (round (+ (pos-x ball) (* (vel-x ball) dt))))
  (setf (pos-y ball) (round (+ (pos-y ball) (* (vel-y ball) dt)))))

(defgeneric collide-p (spong-ball obj)
  (:documentation "Check collision between two objects."))

(defmethod collide-p ((ball spong-ball) (obj rectangle))
  (let ((dt (sdl:dt)))
    (cond
      ((and (not
	     (or (> (pos-x ball) (+ (pos-x obj) (width obj)))
		 (> (pos-x obj) (+ (pos-x ball) (width ball)))))
	    (not
	     (or (> (pos-y ball) (+ (pos-y obj) (height obj)))
		 (> (pos-y obj) (+ (pos-y ball) (height ball)))))))
      ((let ((next-x (+ (pos-x ball) (* (vel-x ball) dt)))
	     (next-y (+ (pos-y ball) (* (vel-y ball) dt))))
	 (and (not
	       (or (> next-x (+ (pos-x obj) (width obj)))
		   (> (pos-x obj) (+ next-x (width ball)))))
	      (not
	       (or (> next-y (+ (pos-y obj) (height obj)))
		   (> (pos-y obj) (+ next-y (height ball)))))))))))
