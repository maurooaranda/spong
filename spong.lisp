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

;;; S-Pong misc functions.
(defun set-serving-player ()
  "Set a new serving player, randomly.  Modifies `*serving-player*'.
This function is used when the serving player should be random: after the user
press RET."
  (setf *serving-player* (if (eql (random 2) 0) *player-left* *player-right*)))

;;; A game class.
(defclass game ()
  ((name :initarg :game-name :accessor game-name)
   (state :initform 'start :accessor game-state)))

(defun init-audio (&key (freq 22050) (chann 1) (cs 1024) (sfx nil))
  "Helper S-Pong function to initialise the audio system.
Initialise all sounds and modify `*sounds*' accordingly."
  (sdl-mixer:init-mixer)
  (if (sdl-mixer:open-audio :frequency freq :channels chann :chunksize cs)
      (setf *sounds* (when sfx
		       (mapcan #'(lambda (a)
				   (list (fx-name a)
					 (sdl-mixer:load-sample
					  (sdl:create-path (resource a)
							   *asset-dir*))))
			       sfx)))
      (error "Could not load mixer.")))

(defmethod init ((game game))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (init-audio :sfx (mapcar #'(lambda (fx) (make-instance 'audio-fx
							   :fx-name (car fx)
							   :resource (cadr fx)))
			     '((:paddle-hit "paddle_hit.wav")
			       (:wall-hit "wall_hit.wav")
			       (:score "score.wav")
			       (:powerup "powerup.wav"))))
    (game-load game)
    (game-draw game)
    (sdl:update-display)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key) (process-key game key))
      (:video-expose-event () (sdl:update-display))
      (:idle () (process-event game)))))

(defmethod game-load ((game game))
  (setf *random-state* (make-random-state t))
  (sdl:window *window-width* *window-height* :title-caption (game-name game))
  (setf (sdl:frame-rate) *frame-rate*)
  (sdl:initialise-default-font *ttf-spong-font*)
  (setf sdl:*default-color* sdl:*white*)
  (setf *player-left* (make-instance 'left-player :position :left :serve-op '+))
  (setf *player-right* (make-instance 'right-player-ai :position :right
				      :serve-op '-))
  (setf *ball* (make-instance 'spong-ball :side *ball-length*))
  (setf *game-objects* (list *ball* *player-left* *player-right*))
  (setf *players* (remove *ball* *game-objects*))
  (setf *current-modifier* nil)
  (setf *modifiers*
	(pairlis
	 '(defensive-powerup quick-powerup power-powerup)
	 (mapcar #'(lambda (f) (make-instance 'sprite :file f))
		 '("defensive.bmp" "quick.bmp" "power.bmp"))))
  (setf *mod-rate-appearence* (random-ab 2 10)))

(defmethod process-key ((game game) key)
  (cond
    ((sdl:key-pressed-p :SDL-KEY-ESCAPE) (sdl:push-quit-event))
    ((sdl:key-pressed-p :SDL-KEY-RETURN)
     (cond
       ((eq (game-state game) 'start)
	(setf (game-state game) 'serve)
	(set-serving-player))
       ((eq (game-state game) 'done)
	(setf (game-state game) 'serve)
	(mapcar #'reset *game-objects*))
       (t
	(setf (game-state game) 'serve)
	(setf *current-modifier* nil)
	(mapcar #'(lambda (p) (when (modifier p) (expire (modifier p) p)))
		*players*)
	(set-serving-player)
	(reset *ball*))))))

(defun draw-score ()
  "Draw score to the screen, with font `*ttf-score-font*'."
  (sdl:with-font (f *ttf-score-font*)
    (let ((str (format nil "~{~a    ~a~}" (mapcar #'current-score *players*))))
      (sdl:draw-string-solid-* str (- (/ *window-width* 2) 50)
			       (/ *window-height* 3)))))

(defun draw-splash ()
  "Draw text like a splash screen."
  (sdl:draw-string-solid-* "Hello S-Pong!" (- (/ *window-width* 2) 40) 20))

(defun draw-fps ()
  "Draw FPS to the screen, with font `*ttf-small-font*'."
  (sdl:with-font (f *ttf-small-font*)
    (sdl:draw-string-solid-* (write-to-string (truncate (sdl:average-fps)))
			      20 10 :color sdl:*green*)))

(defmethod game-draw ((game game))
  (sdl:clear-display sdl:*black* :surface sdl:*default-display*)
  (cond ((eq (game-state game) 'done)
	 (draw-score)
	 (draw-winner *winner*))
	(t
	 (when(eq (game-state game) 'start)
	   (draw-splash))
	 (draw-score)
	 (draw-fps)
	 (mapcar #'render *game-objects*)
	 (and *current-modifier* (draw (modifier-sprite *current-modifier*)
				       :x (/ *window-width* 2)
				       :y (/ *window-height* 2))))))

(defun modifier-type (modifier)
  (car modifier))

(defun modifier-sprite (modifier)
  (cdr modifier))

(defun opponent (player)
  (car (remove player *players*)))

(defmethod process-event ((game game))
  (mapcar #'(lambda (p) (accelerate p nil)) *players*)
  (let ((dt (sdl:dt)))
    ;; Allow moving the paddles in any state.
    (mapcar #'(lambda (player) (update player dt)) *players*)
    (cond
      ;; Serve.
      ((eq (game-state game) 'serve)
       (multiple-value-bind (dx dy) (serve *serving-player*)
	 (setf (vel-x *ball*) dx) (setf (vel-y *ball*) dy))
       (after-opponent-hit (opponent *serving-player*) *ball*)
       (setf (game-state game) 'play))
      ;; Update ball velocity, collide and score.
      ((eq (game-state game) 'play)
       (cond ; Collide with a modifier or with the paddles.
	 ((and *current-modifier*
	       (collide-p *ball* (modifier-sprite *current-modifier*)))
	  (activate (make-instance (modifier-type *current-modifier*) :life 10)
		    (if (> (vel-x *ball*) 0) *player-left* *player-right*))
	  (sdl-mixer:play-sample (getf *sounds* :powerup))
	  (setf *current-modifier* nil)
	  (setf *mod-rate-appearence* (random-ab 2 10)))
	 ((<= (pos-y *ball*) 0)
	  (sdl-mixer:play-sample (getf *sounds* :wall-hit))
	  (setf (pos-y *ball*) 0)
	  (setf (vel-y *ball*) (- (vel-y *ball*))))
	 ((>= (pos-y *ball*) (- *window-height* (height *ball*)))
	  (sdl-mixer:play-sample (getf *sounds* :wall-hit))
	  (setf (pos-y *ball*) (- *window-height* (height *ball*)))
	  (setf (vel-y *ball*) (- (vel-y *ball*))))
	 (t
	  (let ((hitter (cond ((and (< (vel-x *ball*) 0)
				    (collide-p *ball* *player-left*))
			       *player-left*)
			      ((and (> (vel-x *ball*) 0)
				    (collide-p *ball* *player-right*))
			       *player-right*))))
	    (when hitter
	      (hit hitter)
	      (after-opponent-hit (opponent hitter) *ball*)
	      (or *current-modifier*
		  (and (eql 0 (mod (apply #'+ (mapcar #'hits *players*))
				   *mod-rate-appearence*))
		       (setf *current-modifier*
			     (nth (random (length *modifiers*))
				  *modifiers*))))))))
       (let ((scorer (cond ((< (pos-x *ball*) 0) *player-right*)
			   ((> (pos-x *ball*) *window-width*) *player-left*))))
	 (when scorer
	   (score scorer)
	   (mapcar #'(lambda (p) (when (modifier p) (expire (modifier p) p)))
		   *players*)
	   (setf *mod-rate-appearence* (random-ab 2 10))
	   (when (eq scorer *player-left*) (stop *player-right*))
	   (if (won-p scorer *score-objective*)
	       (progn
		 (setf (game-state game) 'done)
		 (setf *winner* scorer))
	       (progn
		 (setf (game-state game) 'serve)
		 (reset *ball*)))))
       (update *ball* dt))))
  (game-draw game)
  (sdl:update-display))

(defmethod destroy ((game game))
  (sdl-mixer:close-audio)
  (sdl-mixer:quit-mixer))

(defun spong ()
  "Play S-Pong."
  (let ((spong (make-instance 'game :game-name "S-Pong")))
    (unwind-protect 
	 (init spong)
      (destroy spong))))

;; (sb-ext:save-lisp-and-die "spong" :toplevel #'spong :executable t)
