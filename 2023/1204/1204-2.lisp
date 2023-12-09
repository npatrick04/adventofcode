;; Read each card into a game object containing
;;    card number, list of winning numbers, list of numbers you have
;; scoring function to evaluate each game object

;; Redefine a colon as whitespace
;; (set-syntax-from-char #\: #\Space)

(defun parse-game-line (line)
  (declare (optimize debug))
  (with-input-from-string (in (concatenate 'string line " |"))
    (assert (eql 'card (read in)))
    (let ((card-number (read in))
	  (winning-numbers (read-delimited-list #\| in))
	  (own-numbers (read-delimited-list #\| in)))
      (list card-number
	    winning-numbers
	    own-numbers))))

#|
(parse-game-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
|#

(defun num-matches (game)
  (length (intersection (second game) (third game))))

#|
(card-point-value (parse-game-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
(card-point-value (parse-game-line "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"))
(card-point-value (parse-game-line "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"))
(card-point-value (parse-game-line "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"))
(card-point-value (parse-game-line "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"))
(card-point-value (parse-game-line "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))
|#

(defun 1204-2 (filename)
  (declare (optimize debug))
  (with-open-file (in filename)
    (let ((reader (copy-readtable)))
      (handler-case
	(progn
	  (setf *readtable* (copy-readtable))
	  (set-syntax-from-char #\: #\Space)
	  (prog1
	      (do ((line (read-line in nil nil) (read-line in nil nil))
		   (card-set (make-array 10 :fill-pointer 0 :adjustable t :initial-element 0))
		   (num-cards 0))
		  ((not line)
		   (format t "End with cardset: ~A~%" card-set)
		   (reduce #'+ card-set))
		(let ((card-data (parse-game-line line)))
		  (format t "Read card: ~A~%" card-data)
		  ;; Add winning cards
		  (let* ((wins (num-matches card-data))
			 (num (first card-data))
			 (max-size (+ num wins 1)))
		    (when (< (fill-pointer card-set) max-size)
		      (adjust-array card-set (+ max-size 2) :fill-pointer (1+ max-size)))
		    (incf (aref card-set num))
		    (format t "Have ~A of card ~A~%" (aref card-set num) num)
		    (dotimes (i wins)
		      (incf (aref card-set (+ num i 1))
			    (aref card-set num))))))
	    (setf *readtable* reader)))
	(error (c)
	  (setf *readtable* reader)
	  (error c))))))
	  
