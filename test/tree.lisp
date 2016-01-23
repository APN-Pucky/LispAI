(defparameter ops (make-hash-table))
(defun lit () (- (random 32.0) 16.0))

(setf (gethash '+ ops) 2)
(setf (gethash '- ops) 2)
(setf (gethash '* ops) 2)
(setf (gethash '/ ops) 2)
;;;(setf (gethash 'getm ops) 2)
(setf (gethash 'sin ops) 1)

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun make-tree-node ()
	(if (< (random 1.0) 0.5)
		(progn
			(return-from make-tree-node (lit)))
		(let ((key (nth (random (length (hash-keys ops))) (hash-keys ops))) (array '()))
			(dotimes (r (gethash key ops))
				(push (make-tree-node) array))
			(push key array)
			;;;(print array)
			(return-from make-tree-node array)
			)))

