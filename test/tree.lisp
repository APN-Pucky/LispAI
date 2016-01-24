(defparameter ops (make-hash-table))

(setf (gethash '+ ops) 2)
(setf (gethash '- ops) 2)
(setf (gethash '* ops) 2)
(setf (gethash '/ ops) 2)
(setf (gethash 'mod ops) 2)
;;;(setf (gethash 'getms ops) 2)
;;;(setf (gethash 'sin ops) 1)
;;;(setf (gethash 'abs ops) 1)
;;;(setf (gethash 'expt ops) 2)

(defun hash-keys (hash-table)
  	(loop for key being the hash-keys of hash-table collect key))

(defun getms (x y)
	(getm (abs (mod x 4)) (abs (mod y 4))))

(defun lit () (- (random 32.0) 16.0))

(defun op ()
	(nth (random (length (hash-keys ops))) (hash-keys ops)))

;;;length!!!
(defun make-tree ()
	(if (< (random 1.0) 0.5)
		(progn
			(return-from make-tree (lit)))
		(let ((key (op))  (array '()))
			(dotimes (r (gethash key ops))
				(push (make-tree) array))
			(push key array)
			(return-from make-tree array)
			)))
(defun mutation (t1)
	(values (change-lit (change-op t1)) (change-tree t1)))

(defun change-lit (t1)
	(let ((i (random (count-leafs t1))))
                (change-leaf-at t1 i (lit))))

(defun change-tree (t1)
	(change-node-at t1 (random (count-nodes t1)) (make-tree)))
;;;Only 2 param
(defun change-op (t1)
	(let ((i (random (count-nodes t1))))
		(change-node-at t1 i (cons (op) (cdr (node-at t1 i))))))

(defun crossover (t1 t2) 	
	(let ((i (random (count-nodes t1))) (j (random (count-nodes t2))))
		(let ((tmp1 (node-at t1 i)) (tmp2 (node-at t2 j)))
			(values (change-node-at t1 i tmp2) (change-node-at t2 j tmp1)))))

(defun get-node-list (tree)
	(if (atom tree) (return-from get-node-list '()) (let ((array '()) (r '()))
				(loop for child in (rest tree) do
					(setf r (get-node-list child))
					(if r (setf array (append array r))))
				(push tree array)
				(return-from get-node-list array))))	

(defun count-elem (tree)
  (if (atom tree)
      1
      (1+ (loop for child in (rest tree)
                summing (count-elem child)))))

(defun count-nodes (tree)
  (if (atom tree)
      0
      (1+ (loop for child in (rest tree)
                summing (count-nodes child)))))

(defun count-leafs (tree)
  (if (atom tree)
      1
      (loop for child in (rest tree)
                summing (count-leafs child))))

(defun change-node-at (x index value)
  (let ((n -1))
    (labels ((traverse (node)
               (unless (atom node)
                 (incf n)
                 (when (= n index)
                   (return-from traverse value)))
               (if (atom node)
                   node
                   (cons (first node)
                         (mapcar #'traverse (rest node))))))
      (let ((new-tree (traverse x)))
        (when (< n index)
          (error "Index ~S out of bounds in tree ~S." index x))
        new-tree))))

(defun change-leaf-at (x index value)
  (let ((n -1))
    (labels ((traverse (node)
               (when (atom node)
                 (incf n)
                 (when (= n index)
                   (return-from traverse value)))
               (if (atom node)
                   node
                   (cons (first node)
                         (mapcar #'traverse (rest node))))))
      (let ((new-tree (traverse x)))
        (when (< n index)
          (error "Index ~S out of bounds in tree ~S." index x))
        new-tree))))

(defun node-at (x index)
  (let ((n index))
    (labels ((traverse (node)
               (unless (atom node)
                 (when (zerop n)
                   (return-from node-at node))
                 (decf n))
               (when (listp node)
                 (mapc #'traverse (rest node)))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

(defun leaf-at (x index)
  (let ((n index))
    (labels ((traverse (node)
               (when (atom node)
                 (when (zerop n)
                   (return-from leaf-at node))
                 (decf n))
               (when (listp node)
                 (mapc #'traverse (rest node)))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

