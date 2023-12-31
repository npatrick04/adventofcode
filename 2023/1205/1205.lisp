(defun parse-seeds (line)
  (with-input-from-string (in (concatenate 'string line ")") :start 6)
    (read-delimited-list #\) in)))

(defun read-map (stream)
  (declare (optimize debug))
  (let ((lines (loop for line = (read-line stream nil nil)
		     while (and line (string/= line ""))
		     collect (substitute #\Space #\- line :test #'char=)))
	from to)
    (with-input-from-string (in (first lines))
      (setf from (read in))
      (read in)
      (setf to (read in)))
    (let ((map ()))
      (dolist (line (cdr lines) (list from to map))
	(with-input-from-string (in line)
	  (push (list (read in) (read in) (read in)) map))))))

(defun parse-file (filename)
  (with-open-file (in filename)
    (let ((seeds (parse-seeds (read-line in)))
	  (maps ()))
      (read-line in)
      (dotimes (i 7 (list seeds (reverse maps)))
	(push (read-map in) maps)))))

(defun map-source-to-dest (map value)
  (declare (optimize debug))
  (let ((the-mapping (find-if (lambda (mapping)
				(let ((src (second mapping))
				      (dist (third mapping)))
				  (<= src value (+ src dist -1))))
			      map)))
    (if the-mapping
	(let ((dest (first the-mapping))
	      (src (second the-mapping)))
	  (let ((src-diff (- value src)))
	    (+ dest src-diff)))
	value)))

(defun 1205-1 (filename)
  (destructuring-bind (seeds maps) (parse-file filename)
    (let ((min-location nil))
      (dolist (seed seeds min-location)
	(let ((mapping seed))
	  (dolist (map maps)
	    ;; (print mapping)
	    (setf mapping (map-source-to-dest (caddr map) mapping)))
	  (unless min-location
	    (setf min-location mapping))
	  (setf min-location (min min-location mapping)))))))

(defun map-dest-to-source (map value)
  (declare (optimize debug))
  (let ((the-mapping (find-if (lambda (mapping)
				(let ((dest (first mapping))
				      (dist (third mapping)))
				  (<= dest value (+ dest dist -1))))
			      map)))
    (if the-mapping
	(let ((dest (first the-mapping))
	      (src (second the-mapping)))
	  (let ((dest-diff (- value dest)))
	    (+ src dest-diff)))
	value)))

(defun get-seed-ranges (seeds)
  (do ((seeds seeds (cddr seeds))
       (result nil))
      ((null seeds) (reverse result))
    (push (list (car seeds)
		(+ (car seeds) (cadr seeds)))
	  result)))

(defun 1205-2 (filename)
  (destructuring-bind (seeds maps) (parse-file filename)
    (let ((min-location nil)
	  (seed-ranges (get-seed-ranges seeds)))
      (let ((location-ranges (mapcar #'get-seed-ranges
				     (sort (caddr (assoc 'humidity maps))
					   #'<
					   :key #'car))))
	location-ranges))))

