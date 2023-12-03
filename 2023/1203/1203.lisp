;; Store each part number with x start, x end, y
;; Store each part symbol with x, y
;;
;; Collect a list of part-numbers, and part-symbols
;;
;; Generate a 2D array of part pointers that refer to the part number
;; Generate a 2D array of valid windows
;; Mask the part pointers by valid windows

(defclass part-number ()
  ((value :accessor value :initarg :value)
   (x-start :accessor x-start :initarg :x-start)
   (x-stop :accessor x-stop :initarg :x-stop)
   (y :accessor y :initarg :y)))
(defmethod print-object ((part part-number) stream)
  (print-unreadable-object (part stream :type t)
    (format stream "~A {~A,~A-~A}"
	    (value part)
	    (y part)
	    (x-start part)
	    (x-stop part))))

(defclass part-symbol ()
  ((part-symbol :accessor part-symbol :initarg :part-symbol)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))
(defmethod print-object ((symb part-symbol) stream)
  (print-unreadable-object (symb stream :type t)
    (format stream "~A {~A, ~A}"
	    (part-symbol symb)
	    (y symb)
	    (x symb))))

(defparameter *parts* ())
(defparameter *part-symbols* ())

(defun print-things ()
  (format t "~{~A~%~}" (reverse *parts*))
  (format t "~{~A~%~}" (reverse *part-symbols*)))

(defun reset-parts ()
  (setf *parts* ()
	*part-symbols* ()))

(defun parse-part-line (y line)
  (declare (optimize debug))
  (do* ((index 0 (1+ index))
	(char (elt line index) (if (= index (length line))
				   #\.
				   (elt line index)))
	(partial-part 0)
	(partial-part-offset nil)
	(partial-part-size 0))
       ((= index (length line)) 
	(when partial-part-offset
	  (let ((part (make-instance 'part-number
				     :value partial-part
				     :x-start partial-part-offset
				     :x-stop (+ partial-part-offset partial-part-size -1)
				     :y y)))
	    (push part *parts*))))
    (if (digit-char-p char)
	(progn
	  (setf partial-part (+ (* partial-part 10) (digit-char-p char)))
	  (incf partial-part-size)
	  (if (not partial-part-offset)
	      (setf partial-part-offset index)))
	(progn
	  ;; If was generating a part number
	  (when partial-part-offset
	    (let ((part (make-instance 'part-number
				       :value partial-part
				       :x-start partial-part-offset
				       :x-stop (+ partial-part-offset partial-part-size -1)
				       :y y)))
	      (push part *parts*)
	      (setf partial-part 0
		    partial-part-size 0
		    partial-part-offset nil)))

	  ;; If a symbol
	  (when (char/= char #\.)
	    (let ((part-symbol (make-instance 'part-symbol
					      :part-symbol char
					      :x index
					      :y y)))
	      (push part-symbol *part-symbols*)))))))

#|
(parse-part-line 1 ".......36....368.*...............*....*.........*..88......%866.......135.........*..................515.682.....114...%...........*.....768")
|#

(defun generate-mask ()
  ;; Get max extends of part numbers and symbols
  (let ((max-x (1+ (max (apply #'max (mapcar #'x-stop *parts*))
			(apply #'max (mapcar #'x *part-symbols*)))))
	(max-y (1+ (max (apply #'max (mapcar #'y *parts*))
			(apply #'max (mapcar #'y *part-symbols*))))))
    (let ((mask (make-array (list max-y max-x) :initial-element nil)))
      (dolist (symb *part-symbols* mask)
	(loop for y from (max 0 (1- (y symb))) to (1+ (y symb))
	      do
		 (loop for x from (max 0 (1- (x symb))) to (1+ (x symb))
		       do (setf (aref mask y x) symb)))))))

(defun print-mask (mask)
  (loop for y below (array-dimension mask 0)
	do (progn (loop for x below (array-dimension mask 1)
			do (let ((symb (aref mask y x)))
			     (if (typep symb 'part-symbol)
				 (format t "~A" (part-symbol symb))
				 (format t "."))))
		  (terpri))))

(defun generate-part-filter (mask)
  (let ((part-filter (make-array (array-dimensions mask) :initial-element nil)))
    (dolist (part *parts* part-filter)
      (loop for x from (x-start part) to (x-stop part)
	    do (setf (aref part-filter (y part) x) part)))))

(defun find-masked-parts ()
  (let* ((mask (generate-mask))
	 (parts (generate-part-filter mask))
	 (dims (array-dimensions mask))
	 (result nil))
    (dotimes (y (car dims) result)
      (dotimes (x (cadr dims))
	(when (and (aref mask y x) (aref parts y x))
	  (pushnew (aref parts y x) result))))))

(defun parse-file (filename)
  (reset-parts)
  (with-open-file (in filename)
    (do ((i 0 (1+ i))
	 (line (read-line in nil nil) (read-line in nil nil)))
	((null line))
      (parse-part-line i line))))

(defun run-part1 (filename)
  (parse-file filename)
  (let ((parts (find-masked-parts)))
    (reduce #'+ (mapcar #'value parts))))
