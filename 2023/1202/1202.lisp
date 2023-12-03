;; Cubeset is a plist of number of colors
;; Store a game as a list of cubesets
;; Store all games as list of game number and game

(unless (boundp '+whitespace+)
  (defconstant +whitespace+ '(#\space #\tab #\linefeed #\return #\page)))

(set-syntax-from-char #\: #\Space)
(set-syntax-from-char #\, #\Space)

(defun read-gameset (stream)
  (handler-case (reverse (read-delimited-list #\; stream))
    (end-of-file () nil)))

(defun read-game-data (stream)
  (declare (optimize debug))
  (let ((line (concatenate 'string (read-line stream) ";")))
    (with-input-from-string (str-stream line)
      (do* ((gameset (read-gameset str-stream)
		     (read-gameset str-stream))
	    (result (list gameset)
		    (cons gameset result)))
	   ((null gameset) (reverse (cdr result)))))))

(defun read-game (stream)
  (declare (optimize debug))
  (handler-case 
      (let (game-number)
	;; Game
	(let ((game (read stream)))
	  (assert (equalp game 'game)))

	;; Game number
	(setf game-number (read stream))
	(assert (integerp game-number))

	(list game-number (read-game-data stream)))
    (end-of-file nil)))

(defun read-all-games (stream)
  (do* ((game (read-game stream) (read-game stream))
	(games (list game) (cons game games)))
       ((null game) (reverse (cdr games)))))

(defparameter *max-cubes* '(red 12 green 13 blue 14))

(defun is-gameset-valid (gameset)
  (and (>= (getf *max-cubes* 'red) (getf gameset 'red 0))
       (>= (getf *max-cubes* 'green) (getf gameset 'green 0))
       (>= (getf *max-cubes* 'blue) (getf gameset 'blue 0))))

(defun is-game-valid (game)
  (destructuring-bind (number iterations) game
    (if (every #'is-gameset-valid iterations) number 0)))

#|
(is-game-valid '(1 ((RED 4 BLUE 3) (BLUE 6 GREEN 2 RED 1) (GREEN 2))))
(is-game-valid '(1 ((RED 12 BLUE 3) (BLUE 15 GREEN 2 RED 1) (GREEN 2))))
|#

(defun part-1-read-file (filename)
  (with-open-file (in filename)
    (let ((games (read-all-games in)))
      (reduce #'+ (mapcar #'is-game-valid games)))))

(defun max-color-value (iterations color)
  (apply #'max
	 (mapcar (lambda (gameset)
		   (getf gameset color 0))
		 iterations)))

(defun game-power (game)
  (destructuring-bind (number iterations) game
    (declare (ignore number))
    (* (max-color-value iterations 'red)
       (max-color-value iterations 'green)
       (max-color-value iterations 'blue))))
	    
#|
(game-power '(1 ((RED 4 BLUE 3) (BLUE 6 GREEN 2 RED 1) (GREEN 2))))
(game-power '(1 ((RED 12 BLUE 3) (BLUE 15 GREEN 2 RED 1) (GREEN 2))))
|#

  
(defun part-2-read-file (filename)
  (with-open-file (in filename)
    (let ((games (read-all-games in)))
      ;; (mapcar #'game-power games))
      (reduce #'+ (mapcar #'game-power games))
      )))
