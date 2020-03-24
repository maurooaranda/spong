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

(defparameter *asset-dir*
  (make-pathname :host (pathname-host #.(or *compile-file-truename*
					    *load-truename*))
		 :directory (pathname-directory #.(or *compile-file-truename*
						      *load-truename*)))
  "Directory where we find assets for the game.")

;;; Settings.
(defparameter *window-width* 640 "Width of the game window.")
(defparameter *window-height* 480
  "Height of the game window.  Try a 4:3 or a 16:9 relation")

(defparameter *paddle-acceleration* 200
  "The default paddle speed change, when moving vertically.
The paddle accelerates its velocity to either 200 (positive or negative),
or 0, effectively stoping.")
(defparameter *paddle-width* 8 "Width of the paddle.")
(defparameter *paddle-height* 20 "Height of the paddle.")
(defparameter *ball-length* 4 "Lenght of the ball, which is a square.")
(defparameter *ball-initial-x-speed* 200
  "The initial x-velocity for the ball.")
(defparameter *ball-max-x-speed* 450
  "Maximum x-velocity for the ball.
If set too high, can cause tunneling.")

(defparameter *score-objective* 10 "Goals to win S-Pong.")

;; Font definitions.  The font `*ttf-spong-font*' is the default font,
;; and the rest are used within `sdl:with-font'.
(let ((f (merge-pathnames "font.ttf" *asset-dir*)))
  (defparameter *ttf-spong-font* (make-instance 'sdl:ttf-font-definition
					       :size 16 :filename f))
  (defparameter *ttf-score-font* (make-instance 'sdl:ttf-font-definition
						:size 32 :filename f))
  (defparameter *ttf-small-font* (make-instance 'sdl:ttf-font-definition
						:size 8 :filename f)))

(defparameter *frame-rate* 60 "Set this frame rate for the game.")

;;; Variables used by S-Pong.
;; Like in Pong, S-Pong has two players: Left and Right.
;; Players control paddles and interact with the ball to score points.
(defvar *player-left* nil "The left player of S-Pong.")
(defvar *player-right* nil "The right player of S-Pong.")
(defvar *ball* nil "The S-Pong ball.")
(defvar *serving-player* nil
  "The player that will serve the ball when the next point begins.")
(defvar *winner* nil "The player that has won the last S-Pong match.")
(defvar *game-objects* nil "All S-Pong objects.")
(defvar *players* nil "All players.")

(defvar *sounds* nil
  "A plist with the sound effects to play.
Each keyword is the action, and the value is the sound effect corresponding
to the action.")
(defvar *modifiers* nil "Modifier sprites, that spice up the game.")
(defvar *current-modifier* nil "The modifier that currently can be obtained.")
(defparameter *mod-rate-appearence* nil
  "Hits to count until a modifier appears.")
