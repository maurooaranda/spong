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

(defclass sprite (rectangle)
  ((filename :initarg :file :accessor filename)
   (image :accessor image)
   (x :accessor pos-x)
   (y :accessor pos-y)
   (width :accessor width)
   (height :accessor height)))

(defmethod initialize-instance :after ((s sprite) &key)
  (setf (image s) (sdl:load-image (merge-pathnames (filename s) *asset-dir*)))
  (setf (width s) (sdl:width (image s)))
  (setf (height s) (sdl:width (image s))))

(defmethod draw ((s sprite) &key x y)
  (setf (pos-x s) x)
  (setf (pos-y s) y)
  (sdl:draw-surface-at-* (image s) x y))
