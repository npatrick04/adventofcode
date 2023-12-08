(defun cardnum (card)
  (ecase card
    (#\A 14)
    (#\K 13)
    (#\Q 12)
    (#\J 11)
    (#\T 10)
    (#\9 9)
    (#\8 8)
    (#\7 7)
    (#\6 6)
    (#\5 5)
    (#\4 4)
    (#\3 3)
    (#\2 2)))

(defun card> (card1 card2)
  (> (cardnum card1) (cardnum card2)))
#|
(sort "JA32T3QK9" #'card>)
"AKQJT9332"
|#

(defun card-chunks (hand)
  "Split the hand into number of each card"
  (declare (optimize debug))
  (let ((sorted (sort (copy-seq hand) #'card>)))
    (do* ((index 0 (1+ index))
	  (card (elt sorted index))
	  (set (list card 0))
	  (result ()))
	 ((= index 5)
	  (push set result)
	  (sort result #'> :key #'cadr))
      (setf card (elt sorted index))
      (if (char= card (first set))
	  (incf (cadr set))
	  (progn
	    (push set result)
	    (setf set (list card 1)))))))

(defun hand-type (hand)
  (declare (optimize debug))
  (let ((chunks (card-chunks hand)))
    (cond
      ((= (cadar chunks) 5) 'five-of-a-kind)
      ((= (cadar chunks) 4) 'four-of-a-kind)
      ((and (= (cadar chunks) 3)
	    (= (cadadr chunks) 2))
       'full-house)
      ((and (= (cadar chunks) 3)
	    (= (cadadr chunks) 1))
       'three-of-a-kind)
      ((and (= (cadar chunks) 2)
	    (= (cadadr chunks) 2))
       'two-pair)
      ((and (= (cadar chunks) 2)
	    (= (cadadr chunks) 1))
       'one-pair)
      (t 'high-card))
    ))

(defun hand> (hand1 hand2)
  (declare (optimize debug))
  (let ((ht1 (hand-type hand1))
	(ht2 (hand-type hand2))
	(card-type-value '((five-of-a-kind . 7)
			   (four-of-a-kind . 6)
			   (full-house . 5)
			   (three-of-a-kind . 4)
			   (two-pair . 3)
			   (one-pair . 2)
			   (high-card . 1))))
    (if (eq ht1 ht2)
	;; Check for high-card
	(do ((cmp1 (map 'list #'card> hand1 hand2) (cdr cmp1))
	     (cmp2 (map 'list #'card> hand2 hand1) (cdr cmp2)))
	    ((or (car cmp1) (car cmp2)) (car cmp1)))
	(> (cdr (assoc ht1 card-type-value))
	   (cdr (assoc ht2 card-type-value))))))


(defun read-hand-line (stream)
  (let ((hand (make-string 5)))
    (read-sequence hand stream)
    (let ((bid (read stream)))
      (list hand bid))))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for hand-line = (handler-case (read-hand-line in)
			    (error () nil))
	  while hand-line
	  ;;do (print hand-line)
	  collect hand-line)))

(defun 1207-1 (filename)
  (let* ((input (read-input filename))
	 (sorted (sort input #'hand> :key #'car))
	 (max-rank (length sorted))
	 (rank-set (loop for i from max-rank downto 1
				collect i)))
    (reduce #'+
	    (mapcar #'*
		    (mapcar #'cadr sorted)
		    rank-set))))
    
