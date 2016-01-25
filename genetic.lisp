(load "tree.lisp")
(setf *random-state* (make-random-state t))

(defparameter ops (make-hash-table))

(setf (gethash '+ ops) 2)
(setf (gethash '- ops) 2)
(setf (gethash '* ops) 2)
;;;(setf (gethash '/ ops) 2)
;;;(setf (gethash 'mod ops) 2)
(setf (gethash 'getms ops) 2)
;;;(setf (gethash 'sin ops) 1)
;;;(setf (gethash 'abs ops) 1)
;;;(setf (gethash 'expt ops) 2)

(defun hash-keys (hash-table)
  	(loop for key being the hash-keys of hash-table collect key))

(defun getms (x y)
	(getm (mod (abs (round x)) 4) (mod (abs (round y )) 4)))

(defun lit () (- (random 32.0) 16.0))

(defun op ()
	(nth (random (length (hash-keys ops))) (hash-keys ops)))

;;;len node num
(defun make-tree-node (len)
	(when (< len 1) (return-from make-tree-node (lit)))
	(if (< (random 1.0) 0.3)
		(progn
			(return-from make-tree-node (lit)))
		(let ((key (op))  (array '()))
			(dotimes (r (gethash key ops))
				(push (make-tree-node (- len 1)) array))
			(push key array)
			(return-from make-tree-node array)
			)))

(defun make-tree (len)
	(let ((key (op))  (array '()))
                        (dotimes (r (gethash key ops))
                                (push (make-tree-node (- len 1)) array))
                        (push key array)
                        (return-from make-tree array)))


(defun change-lit (t1)
	(let ((i (random (count-leafs t1))))
                (change-leaf-at t1 i (lit))))

(defun change-tree (t1)
	(let ((i (random (count-nodes t1))))
		(change-node-at t1 i (make-tree (/ (count-nodes (node-at t1 i)) 2)))))

;;;Only 2 param
(defun change-op (t1)
	(let ((i (random (count-nodes t1))))
		(change-node-at t1 i (cons (op) (cdr (node-at t1 i))))))

(defun mutation (t1)
	(values (change-lit (change-op t1)) (change-tree t1)))

(defun crossover (t1 t2) 	
	(let ((i (random (count-nodes t1))) (j (random (count-nodes t2))))
		(let ((tmp1 (node-at t1 i)) (tmp2 (node-at t2 j)))
			(values (change-node-at t1 i tmp2) (change-node-at t2 j tmp1)))))
