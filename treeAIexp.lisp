(setf *random-state* (make-random-state t))

(load "game.lisp")
(load "genetic.lisp")


(defparameter *tree1* '(-
 (GETMS
  (GETMS
   (+
    (-
     (* (* (+ -3.84935 -5.596092) (GETMS -9.318817 8.096485))
        (- (* 4.0683975 (- 12.622986 -9.598026)) -15.416599))
     (* (* 7.180435 (* -0.76106644 -5.5836143))
        (GETMS (GETMS -2.9298592 -13.437523) (+ 1.9598694 -5.4607964))))
    13.104748)
   (*
    (- (+ (* -0.115448 2.1490326) -1.7047615)
       (* (- 15.472214 5.552868) 15.435394))
    (+ (GETMS (- -6.4072456 14.55798) (+ -8.501381 7.149845))
       (* 9.598976 6.9232483))))
  (GETMS -6.9882813 -12.083107))
 (*
  (+
   (* (GETMS 4.746731 (+ (- -15.313335 -10.799007) 7.307972))
      (GETMS (+ 3.252697 -4.186943)
             (GETMS (* -3.1394043 -15.285042) (GETMS 5.959667 -1.8295174))))
   -7.9482117)
  (- 8.553925
     (-
      (* (* (+ -3.84935 -5.596092) (GETMS -9.318817 8.096485))
         (- (* 4.0683975 (- 12.622986 -9.598026)) -15.416599))
      (* (* 7.180435 (* -0.76106644 -5.5836143))
         (GETMS (GETMS -2.9298592 -13.437523) (+ 1.9598694 -5.4607964))))))) 


)

(defparameter *tree2* '())
(defparameter *tree3* '())
(defparameter *tree4* '())
(defparameter *tree5* '())
(defparameter *tree6* '())
(defparameter *tree7* '())
(defparameter *tree8* '())
(defparameter *tree9* '())
(defparameter *treet1* (make-tree 7))
(defparameter *treet2* (make-tree 7))
(defparameter *treee1* (make-tree 4))
(defparameter *treee2* (make-tree 5))

(defun calc (matrix)
	(let ((r (eval *tree1*)))
		(cond 	((< r 26) (left))
			((< r 51) (right))
			((< r 76) (up))
			(t 	  (down)))))

(format t "2 ~a~%" (getValue 'calc))

(setf *tree2* *tree1*)
(setf *tree1* (mutation *tree1*))
(format t "3 ~a~%" (getValue 'calc))

(setf *tree3* *tree1*)
(setf *tree1* (crossover *tree1* *tree2*))
(format t "4 ~a~%" (getValue 'calc))

(setf *tree4* *tree1*)
(setf *tree1* (crossover *treet2* *tree2*))
(format t "5 ~a~%" (getValue 'calc))

(setf *tree5* *tree1*)
(setf *tree1* (crossover *treet1* *tree2*))
(format t "6 ~a~%" (getValue 'calc))

(setf *tree6* *tree1*)
(setf *tree1* (crossover *tree4* *treee1*))
(format t "7 ~a~%" (getValue 'calc))

(setf *tree7* *tree1*)
(setf *tree1* (crossover *tree3* *treee2*))
(format t "8 ~a~%" (getValue 'calc))

(setf *tree8* *tree1*)
(setf *tree1* (crossover *tree6* *treee2*))
(format t "9 ~a~%" (getValue 'calc))

(setf *tree9* *tree1*)
(setf *tree1* (crossover *tree1* *tree3*))
(format t "1 ~a~%" (getValue 'calc))
