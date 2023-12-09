(ql:quickload :lparallel)
(use-package :lparallel)
(use-package :lparallel.queue)

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

(defun run-1208-2 (filename)
  (declare (optimize debug))
  (destructuring-bind (instructions nodes) (read-file filename)
    (let ((table (gen-hash-nodes nodes))
	  (num-instructions (length instructions))
	  (real-instructions (make-array (length instructions)
					 :initial-contents instructions)))
      (let* ((starting-nodes (mapcar #'car (remove #\A nodes
						   :key (lambda (node)
							  (aref (symbol-name (first node)) 2))
						   :test-not #'eql)))
	     (num-tasks (length starting-nodes)))
	(format t "~A tasks for nodes: ~{~A~^, ~}~%" num-tasks starting-nodes)
	(setf *kernel* (make-kernel num-tasks))
	(let ((channel (make-channel))
	      (task-id 0))
	  (dolist (node starting-nodes)
	    (submit-task channel (lambda (id starting-i)
				   (do* ((i starting-i (1+ i))
					 (instr #\.
						(aref real-instructions (mod (1- i) num-instructions)))
					 (node node
					       (if (char= instr #\L)
						   (car (gethash node table))
						   (cdr (gethash node table)))))
					((char= (aref (symbol-name node) 2) #\Z)
					 (list id node i))))
			 task-id 0)
	    (incf task-id))
	    
	    (do ((task-idx (make-array num-tasks :initial-element ()))
		 (task-result (receive-result channel) (receive-result channel)))
		(nil)
	      (destructuring-bind (id node i) task-result
		(push i (aref task-idx id))
		(format t "Found ~A-index: ~A ~A~%" id node i ;; (aref task-idx id)
			)
		(when (every (lambda (indices) (member i indices)) task-idx)
		  (format t "Found i: ~A~%" i)
		  (break))
		(submit-task channel
			     (lambda (id new-i)
			       ;;(format t "Inside: ~A ~A~%" id new-i)
			       (do* ((internal-i (1+ new-i) (1+ internal-i))
				     (instr (aref real-instructions (mod (1- internal-i) num-instructions))
					    (aref real-instructions (mod (1- internal-i) num-instructions)))
				     (node (if (char= instr #\L)
					       (car (gethash node table))
					       (cdr (gethash node table)))
					   (if (char= instr #\L)
					       (car (gethash node table))
					       (cdr (gethash node table)))))
				    ((char= (aref (symbol-name node) 2) #\Z)
				     (list id node internal-i))
				 ;; (format t "in-in: ~A ~A~%" internal-i node)
))
			     id i))))))))

#|
Found 0-index: SCZ 13939 - 27878
Found 1-index: DDZ 22618 - 22618
Found 2-index: ZZZ 20777 - 20777
Found 3-index: PTZ 15517 - 31034
Found 4-index: NQZ 17621 - 35242
Found 5-index: GVZ 18673 - 37346

26579225618258
Give up on brute force, use LCM since it just repeats.
lcm 13939,22618,20777,15517,17621,18673
|#

  
