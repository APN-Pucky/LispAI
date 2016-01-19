(setf *random-state* (make-random-state t))
	
(defvar *debug* NIL)
(defvar *fails* 10)
(defvar *iterations* 1000.0)
(defvar *print-width* 10)
(defvar *size* 4)
(defvar *width* *size*)
(defvar *height* *size*)
(defvar *array*)
(defvar *tmp*)
(defvar *score* 0)
(defvar *sum* 0.0)

(defparameter *matrix* '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
(defparameter *matrix-save* '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
(defparameter *matrix-check* '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(defun logdbg (s)
	(if *debug* (format t "~a~%" s)))

(defun printmatrix (matrix) 
  	(format t "~{|~{ ~{~Vd~}~}|~%~}~%"
    		(mapcar #'(lambda (r) (mapcar #'(lambda (v) (list *print-width* v)) r)) matrix)))

(defun getm (x y)
	(nth x (nth y *matrix*)))

(defun setm (x y v)
	(setf (nth x (nth y *matrix*)) v))

(defun inc (v)
	(setf *score* (+ *score* v)))

(defun fillrandom ()
	(setf *array* '())
	(loop for y from 0 to (- *height* 1) do 
		(loop for x from 0 to (- *width* 1) do 
			(if (= (getm x y) 0)(progn (push y *array*) (push x *array*)))))
	(setf *tmp* (* (random (/ (length *array*) 2)) 2))
	(setm (nth *tmp* *array*) (nth (+ 1 *tmp*) *array*) (if (< (random 1.0) 0.9) 2 4)))

(defun clone (clone)
	(loop for y from 0 to (- *height* 1) do 
		(setf (nth y  clone) (copy-list (nth y *matrix*))))) 

(defun initmatrix ()
	(loop for y from 0 to (- *height* 1) do 
		(loop for x from 0 to (- *width* 1) do 
			(setm x y 0))))

(defun startgame ()
	(initmatrix)
	(setf *score* 0)
	(clone *matrix-save*)
	(clone *matrix-check*)
	(fillrandom)
	(fillrandom))

(defun move-right ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from (- *width* 2) downto 0 do
                        (if (> (getm x y) 0)
                        (progn
                            	(setf *tmp* (loop for z from (- *width* 1) downto (+ x 1) do
                                (if (= (getm z y) 0)(return z))))
                                (if *tmp*
                                (progn
                                   	(setm *tmp* y (getm x y))
                                        (setm x y 0)
                               )
			))))))

(defun move-left ()
        (loop for y from 0 to (- *height* 1) do
                (loop for x from 1 to (- *width* 1) do
                        (if (> (getm x y) 0)
                        (progn
                                (setf *tmp* (loop for z from 0 to (- x 1) do
                                (if (= (getm z y) 0)(return z))))
                                (if *tmp*
                                (progn
                                        (setm *tmp* y (getm x y))
                                        (setm x y 0)
                               )
                        ))))))
 
(defun move-down ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from (- *width* 2) downto 0 do
                        (if (> (getm y x) 0)
                        (progn
                            	(setf *tmp* (loop for z from (- *width* 1) downto (+ x 1) do
                                (if (= (getm y z) 0)(return z))))
                                (if *tmp*
                                (progn
                                   	(setm y *tmp* (getm y x))
                                        (setm y x 0)
                               )
			)))))) 

(defun move-up ()
        (loop for y from 0 to (- *height* 1) do
                (loop for x from 1 to (- *width* 1) do
                        (if (> (getm y x) 0)
                        (progn
                                (setf *tmp* (loop for z from 0 to (- x 1) do
                                (if (= (getm y z) 0)(return z))))
                                (if *tmp*
                                (progn
                                        (setm y *tmp* (getm y x))
                                        (setm y x 0)
                               )
                        ))))))

(defun add-right ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from (- *width* 1) downto 1 do
		(if (not (= (getm x y) 0)) (if (= (getm x y) (getm (- x 1) y)) (progn (setm x y (* (getm x y) 2)) (setm (- x 1) y 0) (inc (getm x y))))))))
	       
(defun add-left ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from 0 to (- *width* 2) do
		(if (not (= (getm x y) 0)) (if (= (getm x y) (getm (+ x 1) y)) (progn (setm x y (* (getm x y) 2)) (setm (+ x 1) y 0) (inc (getm x y))))))))

(defun add-down ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from (- *width* 1) downto 1 do
		(if (not (= (getm y x) 0)) (if (= (getm y x) (getm y (- x 1) )) (progn (setm y x (* (getm y x) 2)) (setm y (- x 1) 0) (inc (getm y x))))))))
	
(defun add-up ()
	(loop for y from 0 to (- *height* 1) do
                (loop for x from 0 to (- *width* 2) do
		(if (not (= (getm y x) 0)) (if (= (getm y x) (getm y (+ x 1))) (progn (setm y x (* (getm y x) 2)) (setm y (+ x 1) 0) (inc (getm y x))))))))

(defun check ()
	(clone *matrix-check*))

(defun checkfill ()
	;;;(print(not (equal *matrix-check* *matrix*)) )
	(if (not (equal *matrix-check* *matrix*)) (progn (fillrandom))))
	
(defun right ()
	(logdbg "right")
	(check)
	(move-right)
	(add-right)
	(move-right)
	(checkfill))

(defun left ()
	(logdbg "left")
	(check)
	(move-left)
	(add-left)
	(move-left)
	(checkfill))

(defun down ()
	(logdbg "down")
	(check)
	(move-down)
	(add-down)
	(move-down)
	(checkfill))

(defun up ()
	(logdbg "up")
	(check)
	(move-up)
	(add-up)
	(move-up)
	(checkfill))

(defun getValue (func)
	(setf *sum* 0)
		(loop for j from 1 to *iterations* do
			(startgame)
			(let ((i 0))
        			(loop
                			(logdbg i)
                			(if (= i *fails*) (progn (setf i 0) (if (equal *matrix-save* *matrix*) (return) (clone *matrix-save*))))
                		(funcall func *matrix*)
                		(setf i (+ i 1))
			))
			;;;(format t "~a~%" *sum*)
			(setf *sum* (+ *sum* *score*))
			
		)	
	(/ *sum* *iterations*)
)
