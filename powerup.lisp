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

(defclass powerup ()
  ((listeners :initarg :listeners :initform nil :accessor listeners)))

(defmethod modifies-p ((powerup powerup) event)
  (assoc event (listeners powerup)))

(defmethod modify ((powerup powerup) event &rest args)
  (funcall (cdr (assoc event (listeners powerup))) powerup args))

(defgeneric activate (powerup spong-player))

(defmethod activate ((powerup powerup) (player spong-player))
  (when (modifier player)
    (expire (modifier player) player)))

(defgeneric expire (powerup spong-player))

(defclass expirable-powerup (powerup)
  ((life :initarg :life :accessor life)))

(defmethod decrease-life ((powerup expirable-powerup) &optional (delta 1))
  (decf (life powerup) delta))

(defmethod expire-check-p ((powerup expirable-powerup))
  (<= (life powerup) 0))

(defmethod expire ((powerup expirable-powerup) (player spong-player))
  (setf (modifier player) nil))

(defclass defensive-powerup (expirable-powerup)
  ((prev-height :initarg :prev-height :accessor prev-height)
   (new-height :initarg :new-height :initform 30 :accessor new-height)))

(defmethod initialize-instance :after ((powerup defensive-powerup) &key)
  (setf (listeners powerup)
	(acons :hit
	       #'(lambda (powerup &rest player)
		   (decrease-life powerup)
		   (when (expire-check-p powerup)
		     (expire powerup (getf (car player) :player)))
		   (default-hit (getf (car player) :player)))
	       (listeners powerup))))

(defmethod activate :after ((powerup defensive-powerup) (player spong-player))
  (setf (modifier player) powerup)
  (setf (prev-height powerup) (height player))
  (setf (height player) (new-height powerup)))

(defmethod expire :after ((powerup defensive-powerup) (player spong-player))
  (setf (height player) (prev-height powerup)))

(defclass quick-powerup (expirable-powerup)
  ((prev-acc :initarg :prev-acc :accessor prev-acc)
   (new-acc :initarg :new-acc :initform 240 :accessor new-acc)))

(defmethod initialize-instance :after ((powerup quick-powerup) &key)
  (setf (listeners powerup)
	(acons :hit
	       #'(lambda (powerup &rest player)
		   (decrease-life powerup)
		   (when (expire-check-p powerup)
		     (expire powerup (getf (car player) :player)))
		   (default-hit (getf (car player) :player)))
	       (listeners powerup))))

(defmethod activate :after ((powerup quick-powerup) (player spong-player))
  (setf (modifier player) powerup)
  (setf (prev-acc powerup) (acceleration player))
  (setf (acceleration player) (new-acc powerup)))

(defmethod expire :after ((powerup quick-powerup) (player spong-player))
  (setf (acceleration player) (prev-acc powerup)))

(defclass power-powerup (expirable-powerup)
  nil)

(defmethod activate :after ((powerup power-powerup) (player spong-player))
  (setf (modifier player) powerup))

(defmethod power-hit ((powerup power-powerup) (player left-player))
  (values (- (max (- *ball-max-x-speed*) (* 1.5 (vel-x *ball*))))
	  (let ((new-vel (random-ab 10 150)))
	    (if (< (vel-y *ball*) 0) (- new-vel) new-vel))))

(defmethod power-hit ((powerup power-powerup) (player right-player))
  (values (- (min *ball-max-x-speed* (* 1.5 (vel-x *ball*))))
	  (let ((new-vel (random-ab 10 150)))
	    (if (< (vel-y *ball*) 0) (- new-vel) new-vel))))

(defmethod initialize-instance :after ((powerup power-powerup) &key)
  (setf (listeners powerup)
	(acons :hit
	       #'(lambda (powerup &rest player)
		   (decrease-life powerup)
		   (when (expire-check-p powerup)
		     (expire powerup (getf (car player) :player)))
		   (power-hit powerup (getf (car player) :player)))
	       (listeners powerup))))
