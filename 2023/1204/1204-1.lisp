;; Read each card into a game object containing
;;    card number, list of winning numbers, list of numbers you have
;; scoring function to evaluate each game object

;; Redefine a colon as whitespace
(set-syntax-from-char #\: #\Space)

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

(defun card-point-value (game)
  (let ((num-matches (length (intersection (second game) (third game)))))
    (if (plusp num-matches)
	(expt 2 (1- num-matches))
	0)))

#|
(card-point-value (parse-game-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
(card-point-value (parse-game-line "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"))
(card-point-value (parse-game-line "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"))
(card-point-value (parse-game-line "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"))
(card-point-value (parse-game-line "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"))
(card-point-value (parse-game-line "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))
|#

(defun 1204-1 (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  sum (card-point-value (parse-game-line line)))))
