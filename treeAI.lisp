(load "game.lisp")
(load "genetic.lisp")

(defparameter *curtree* '())

(defun calc (matrix)
	(let ((r (eval *curtree*)))
		(cond 	((< r 26) (left))
			((< r 51) (right))
			((< r 76) (up))
			(t 	  (down)))))
(defun fitness (tree)
	(if (> (count-nodes tree) 200) (return-from fitness 0))
	(setf *curtree* tree)
	(getValue 'calc))

