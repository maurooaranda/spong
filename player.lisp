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

;;; The S-Pong Player class.
;; This is a player that controls an object (in S-Pong, a paddle), and that has
;; a score.
(defclass player ()
  ((score :initarg :score :initform 0 :accessor current-score)))

(defclass spong-player (player paddle)
  ((name :accessor player-name)
   (hits :initform 0 :accessor hits)
   (up-key :initarg :up-key :accessor up-key)
   (down-key :initarg :down-key :accessor down-key)
   (modifier :initarg :modifier :initform nil :accessor modifier)))

(defmethod initialize-instance :after ((player spong-player) &key position serve-op)
  (setf (player-name player) (if (or (not position) (eq position :left))
				 "Left Player" "Right Player"))
  (let ((x (if (or (not position) (eq position :left)) 10
	       (- *window-width* 10 *paddle-width*))))
    (setf (pos-x player) x)
    (setf (serve-op player) serve-op)))

(defmethod accelerate ((player spong-player) direction)
  (call-next-method player
		    (or direction
			(cond ((and (up-key player)
				    (sdl:key-held-p (up-key player)))
			       :up)
			      ((and (down-key player)
				    (sdl:key-held-p (down-key player)))
			       :down)))))

(defmethod hit ((player spong-player))
  (sdl-mixer:play-sample (getf *sounds* :paddle-hit))
  (incf (hits player))
  ;; Shift the ball, so it doesn't bounce repeatedly with the paddle.
  (setf (pos-x *ball*)
	(funcall (serve-op player) (pos-x *ball*) (width player))))

(defmethod add-score ((player player))
  (incf (current-score player)))

(defmethod score ((player spong-player))
  (sdl-mixer:play-sample (getf *sounds* :score))
  (add-score player)
  (mapcar #'(lambda (p) (setf (hits p) 0)) *players*)
  (setf *current-modifier* nil))

(defmethod reset ((player spong-player))
  (reset-score player))

(defmethod reset-score ((player spong-player) &optional (new-score 0))
  (setf (current-score player) new-score))

(defmethod won-p ((player player) objective)
  (>= (current-score player) objective))

(defmethod draw-winner ((winner spong-player))
  (sdl:draw-string-solid-* (format nil "~a wins!" (player-name winner))
			   (- (/ *window-width* 2) 75) 60))

(defgeneric after-opponent-hit (spong-player spong-ball))

(defmethod after-opponent-hit ((p spong-player) (ball spong-ball))
  (declare (ignore p ball))
  nil)
  
(defclass left-player (spong-player)
  ((up-key :initform :sdl-key-w)
   (down-key :initform :sdl-key-s)))

(defmethod serve ((player left-player))
  (values (+ *ball-initial-x-speed*) (random-sim 50)))

(defmethod hit :after ((player left-player))
  (multiple-value-bind (dx dy)
      (if (and (modifier player) (modifies-p (modifier player) :hit))
	  (modify (modifier player) :hit :player player)
	  (default-hit player))
    (setf (vel-x *ball*) dx)
    (setf (vel-y *ball*) dy)))
  
(defmethod score :after ((player left-player))
  (setf *serving-player* *player-right*))

(defclass right-player (spong-player)
  ((up-key :initform :sdl-key-up)
   (down-key :initform :sdl-key-down)))

(defmethod serve ((player right-player))
  (values (- *ball-initial-x-speed*) (random-sim 50)))

(defmethod default-hit ((player left-player))
  (values (- (max (- *ball-max-x-speed*) (* 1.03 (vel-x *ball*))))
	  (let ((new-dy (random-ab 10 150))
		(topp (> (pos-y *ball*)
			 (+ (pos-y player) (/ (height player) 2)))))
	    (if (< (vel-y *ball*) 0)
		(if topp new-dy (- new-dy))
		(if topp new-dy (- new-dy))))))

(defmethod default-hit ((player right-player))
  (values (- (min *ball-max-x-speed* (* 1.03 (vel-x *ball*))))
	  (let ((new-dy (random-ab 10 150))
		(topp (> (pos-y *ball*)
			 (+ (pos-y player) (/ (height player) 2)))))
	    (if (< (vel-y *ball*) 0)
		(if topp new-dy (- new-dy))
		(if topp new-dy (- new-dy))))))

(defmethod hit :after ((player right-player))
  (multiple-value-bind (dx dy)
      (if (and (modifier player) (modifies-p (modifier player) :hit))
	  (modify (modifier player) :hit :player player)
	  (default-hit player))
    (setf (vel-x *ball*) dx)
    (setf (vel-y *ball*) dy)))

(defmethod score :after ((player right-player))
  (setf *serving-player* *player-left*))

(defclass right-player-ai (right-player)
  ((im-ball :accessor imaginary-ball :initform nil)
   (up-key :initform nil)
   (down-key :initform nil)))  

(defmethod after-opponent-hit ((ai right-player-ai) (ball spong-ball))
  (setf (imaginary-ball ai)
	(make-instance 'spong-ball
		       :x (pos-x ball) :y (pos-y ball) :side (width ball)
		       :dx (+ 5 (vel-x ball)) :dy (vel-y ball))))

(defmethod stop ((ai right-player-ai))
  (setf (imaginary-ball ai) nil))

(defmethod hit ((ai right-player-ai))
  (stop ai)
  (call-next-method ai))

(defmethod accelerate ((ai right-player-ai) direction)
  (declare (ignore direction))
  (let ((im (imaginary-ball ai)))
    (call-next-method ai
		      (when im
			(update im (sdl:dt))
			;; Handle imaginary ball collision with the wall.
			(cond
			  ((<= (pos-y im) 0)
			   (setf (pos-y im) 0)
			   (setf (vel-y im) (- (vel-y im))))
			  ((>= (pos-y im) (- *window-height* (height im)))
			   (setf (pos-y im) (- *window-height* (height im)))
			   (setf (vel-y im) (- (vel-y im)))))
			(cond ((> (pos-y im) (+ (pos-y ai) (/ (height ai) 2)))
			       :down)
			      ((< (pos-y im) (+ (pos-y ai) (/ (height ai) 2)))
			       :up))))))
