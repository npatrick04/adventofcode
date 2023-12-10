;; A* based on memory, sort of.  it's just filling cost 

(defvar *start-node* nil)
(defvar *nodes* nil)
(defvar *map* nil)
(defvar *extents* nil)
(defvar *paths* nil)
(defvar *the-path* nil)
(defvar *inside-nodes* nil)

(defun reset-game ()
  (setf *nodes* nil
	*start-node* nil
	*map* nil
	*extents* nil
	*paths* nil
	*the-path* nil
	*inside-nodes* nil))

(defclass node ()
  ((cost :accessor cost :initarg :cost :initform nil)
   (connections-costed :accessor connections-costed :initform 0)
   (symb :accessor symb :initarg :symb)
   (pos :accessor pos :initarg :pos)
   (neighbors :accessor neighbors :initarg :neighbors :initform nil)
   (inside :accessor inside :initform nil)))

(defmethod print-object ((node node) stream)
  (progn;;print-unreadable-object (node stream :type t)
    (format stream "~A"
	    (cond 
	      ((neighbors node) (symb node))
	      ((inside node) #\I)
	      (t #\O)))))

(defclass path ()
  ((nodes :accessor nodes :initarg :nodes)
   (cost :accessor cost :initarg :cost)
   (len :accessor len :initarg :len)
   (done :accessor done :initform nil)))

(defmethod print-object ((path path) stream)
  (print-unreadable-object (path stream :type t)
    (format stream "Cost ~A: ~{~A~^, ~}"
	    (cost path)
	    (mapcar #'symb (reverse (nodes path))))))

(defun print-nodes ()
  (loop for y below (second *extents*) do
    (progn
      (loop for x below (first *extents*) do
	(format t "~A" (aref *nodes* x y)))
      (terpri))))
      
;; Read in phases
;; Read entire buffer in lines, determine x from first line
;; Read number of lines as y extent
;; Generate 2D matrix of nodes by iterating across each line, placing in *nodes* array
;; iterate across matrix, connect neighbors
;; iterate from start-node through all potentially connected neighbors with a path id in breadth-first search
;; Find paths that meet at the same point

(defun read-map (filename)
  (with-open-file (in filename)
    (let ((map-lines (loop for line = (read-line in nil nil)
			   while line
			   collect line)))
      (setf *map* map-lines)
      ;; Return x/y extents
      (setf *extents* (list (length (first map-lines))
			    (length map-lines))))))

(defun generate-nodes ()
  (declare (optimize debug))
  (destructuring-bind (x-size y-size) *extents*
    (setf *nodes* (make-array *extents* :element-type 'node))
    (loop for y below y-size
	  for remaining-lines = *map* then (cdr remaining-lines) do
	    (loop for x below x-size do
	      (let* ((symb (aref (first remaining-lines) x))
		     (node (make-instance 'node
					  :pos (list x y)
					  :symb symb)))
		(setf (aref *nodes* x y) node)
		(when (char= symb #\S)
		  (setf *start-node* node)))))))

(defun connection-directions (symb)
  (case symb
    (#\S '(any))
    (#\| '(north south))
    (#\- '(east west))
    (#\L '(north east))
    (#\J '(north west))
    (#\7 '(south west))
    (#\F '(south east))
    (#\. '())))

(defmacro check-neighbor (to-dir from-dir x-calc y-calc)
  `(when (or (eq direction `any) (eq direction ',to-dir))
     (let ((new-x ,x-calc)
	   (new-y ,y-calc))
       (when (and (< -1 new-x x-size) (< -1 new-y y-size))
	 ;; (format t "Check neighbor at {~A,~A}~%" new-x new-y)
	 (let ((test-node (aref *nodes* new-x new-y)))
	   (when (intersection (connection-directions (symb test-node))
			       (list ',from-dir 'any))
	     ;; (format t "Found match with directions ~A~%" (connection-directions (symb test-node)))
	     (push test-node (neighbors this-node))))))))

(defun connect-neighbors ()
  (declare (optimize debug))
  (destructuring-bind (x-size y-size) *extents*
    (loop for y below y-size do
      (loop for x below x-size do
	(let* ((this-node (aref *nodes* x y))
	       (directions (connection-directions (symb this-node))))
	  ;; (format t "Get neighbors for node with directions ~A at {~A, ~A}~%" directions x y)
	  (dolist (direction directions)
	    (check-neighbor north south x (1- y))
	    (check-neighbor south north x (1+ y))
	    (check-neighbor east west (1+ x) y)
	    (check-neighbor west east (1- x) y)))))))

(defun fill-cost ()
  (declare (optimize debug))
  (let ((num-paths (length (neighbors *start-node*))))
    (setf (cost *start-node*) 0
	  (connections-costed *start-node*) num-paths)
    (dotimes (i num-paths)
      (push (make-instance 'path :len 1 :cost 1 :nodes (list (nth i (neighbors *start-node*))
							     *start-node*))
	    *paths*)
      (setf (cost (nth i (neighbors *start-node*))) i
	    (connections-costed (nth i (neighbors *start-node*))) 1))
    (do ()
	((every #'done *paths*))
      ;; 1 iteration of search
      (dolist (path *paths*)
	(unless (done path)
	  (let ((next-neighbor (car (remove (cadr (nodes path))
					    (neighbors (car (nodes path))))))
		(next-cost (1+ (cost path))))
	    (if (cost next-neighbor)
		;; Check the current last node to determine if it's the end
		(if (= (connections-costed (car (nodes path))) 2)
		    (progn
		      (setf (done path) t)
		      ;; (format t "Path ~A is now done because other neighbor finished it~%" path)
		      )
		    (progn
		      (setf (done path) t
			    (cost path) next-cost)
		      (push next-neighbor (nodes path))
		      (incf (connections-costed next-neighbor))
		      ;; (format t "Path ~A is now done because i'm finishing it~%" path)
		      ))
		(progn
		  (push next-neighbor (nodes path))
		  (incf (connections-costed next-neighbor))
		  (setf (cost next-neighbor) next-cost
			(cost path) next-cost)))))))))

;; Take paths from each search, combine into a single path
;; assume one side is inside, mark inside nodes as inside
;;   Nodes are inside if left of path, and do not have a cost
;; fill from all known inside nodes to mark all others as inside
;; count inside nodes

(defun single-path ()
  (setf *the-path* (make-instance 'path
				  :cost nil
				  :nodes (remove-duplicates (append (nodes (first *paths*))
								    (reverse (nodes (second *paths*))))))))

(defun opposite-direction (dir)
  (case dir
    (east 'west)
    (west 'east)
    (north 'south)
    (south 'north)))

(defun left-side (node1 node2)
  (let ((direction (mapcar #'- (pos node2) (pos node1))))
    (cond
      ((equalp direction '( 1  0)) (mapcar #'+ (pos node2) '( 0  1)))
      ((equalp direction '(-1  0)) (mapcar #'+ (pos node2) '( 0 -1)))
      ((equalp direction '( 0  1)) (mapcar #'+ (pos node2) '( 1  0)))
      ((equalp direction '( 0 -1)) (mapcar #'+ (pos node2) '(-1  0))))))
				     
(defun find-inside-nodes ()
  (declare (optimize debug))
  (do ((path (cons (car (last (nodes *the-path*))) (nodes *the-path*))
	     (cdr path)))
      ;; Until only 1 node remains
      ((not (cdr path)))
    ;; 2 nodes
    (let ((left-side (left-side (car path) (cadr path))))
      ;; Side is valid
      (when (and (< -1 (car left-side) (car *extents*))
		 (< -1 (cadr left-side) (cadr *extents*)))
	(let ((maybe-inside-node (aref *nodes* (car left-side) (cadr left-side))))
	  (when (not (cost maybe-inside-node))
	    (format t "When moving from ~A to ~A, found inside node at ~A~%"
		    (pos (first path))
		    (pos (second path))
		    (pos maybe-inside-node))
	    (setf (inside maybe-inside-node) t)
	    (pushnew maybe-inside-node *inside-nodes*)))))))
    
	  
(defun 1210-2 (filename)
  (reset-game)
  (read-map filename)
  (generate-nodes)
  (connect-neighbors)
  (fill-cost)
  (single-path)
  (find-inside-nodes))

