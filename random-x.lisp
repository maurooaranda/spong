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

;;; Misc functions.
(defun random-sim (n)
  "Return a random number, between -N and N."
  (- (random (1+ (* 2 n))) n))

(defun random-ab (a b)
  "Return a random number between A and B.  Expects A to be smaller than B."
  (+ a (random (1+ (- b a)))))

(defun random-negate (n)
  "Negate a number 50% of the time."
  (or (and (eql (random 2) 1) n) (- n)))
