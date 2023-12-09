(defun read-instructions (stream)
  (do* ((char (read-char stream) (read-char stream))
	(instructions (list char) (cons char instructions)))
       ((and (char/= char #\L) (char/= char #\R)) (reverse (cdr instructions)))))

(defun read-node (stream)
  (handler-case
      (let ((name (read stream))
	    (nodes (read stream)))
	(list name nodes))
    (error () nil)))

(defun read-file (filename)
  (with-open-file (in filename)
    (let ((standard-readtable (copy-readtable))
	  (*readtable* (copy-readtable)))
      (handler-case
	  (progn
	    (set-syntax-from-char #\, #\Space)
	    (set-syntax-from-char #\= #\Space)

	    (do* ((instructions (read-instructions in))
		  (nodes (list (read-node in)) (cons (read-node in) nodes)))
		 ((null (car nodes)) (list instructions (reverse (cdr nodes))))))
	(error () (setf *readtable* standard-readtable))))))

(defun gen-hash-nodes (nodes)
  (let ((table (make-hash-table)))
    (dolist (node nodes table)
      (setf (gethash (first node) table)
	    (cons (car (second node))
		  (cadr (second node)))))))

(defun run-1208-1 (filename)
  (declare (optimize debug))
  (destructuring-bind (instructions nodes) (read-file filename)
    (let ((table (gen-hash-nodes nodes))
	  (num-instructions (length instructions))
	  (real-instructions (make-array (length instructions)
					 :initial-contents instructions)))
      (do* ((i 0 (1+ i))
	    (instr #\.
		   (aref real-instructions (mod (1- i) num-instructions)))
	    (node 'aaa
		  (if (char= instr #\L)
		      (car (gethash node table))
		      (cdr (gethash node table)))))
	   ((eq node 'zzz)
	    i)
	(format t "~A: ~A ~A~%" i instr node)
	))))
