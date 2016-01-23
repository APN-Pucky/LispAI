(defparameter *operators* (list (operator (+ float float) float)
                                      (operator (- float float) float)
                                      (operator (* float float) float)
                                      (operator (sin float) float)))

(defparameter *literals* (list (literal (float)
                                       (- (random 32.0) 16.0))
                                     (literal (float)
                                       '*x*)))

(defparameter *target-expr* '(+ 7 (sin (expt (* *x* 2 pi) 2))))

(defvar *x*)

(defun evaluate (gp expr target-expr)
        (declare (ignore gp))
        (/ 1
           (1+
            ;; Calculate average difference from target.
            (/ (loop for x from 0d0 to 10d0 by 0.5d0
                     summing (let ((*x* x))
                               (abs (- (eval expr)
                                       (eval target-expr)))))
               21))
           ;; Penalize large expressions.
           (let ((min-penalized-size 40)
                 (size (count-nodes expr)))
             (if (< size min-penalized-size)
                 1
                 (exp (min 120 (/ (- size min-penalized-size) 10d0)))))))

(defun randomize (gp type expr)
        (if (and (numberp expr)
                 (< (random 1.0) 0.5))
            (+ expr (random 1.0) -0.5)
            (random-gp-expression gp (lambda (level)
                                       (<= 3 level))
                                  :type type)))

(defun run ()
        (let ((*print-length* nil)
              (*print-level* nil)
              (gp (make-instance
                   'gp
                   :toplevel-type 'real
                   :operators *operators*
                   :literals *literals*
                   :population-size 1000
                   :copy-chance 0.0
                   :mutation-chance 0.5
                   :evaluator (lambda (gp expr)
                                (evaluate gp expr *target-expr*))
                   :randomizer 'randomize
                   :selector (lambda (gp fitnesses)
                               (declare (ignore gp))
                               (hold-tournament fitnesses :n-contestants 2))
                   :fittest-changed-fn
                   (lambda (gp fittest fitness)
                     (format t \"Best fitness until generation ~S: ~S for~%  ~S~%\"
                             (generation-counter gp) fitness fittest)))))
          (loop repeat (population-size gp) do
            (add-individual gp (random-gp-expression gp (lambda (level)
                                                          (<= 5 level)))))
          (loop repeat 1000 do
            (when (zerop (mod (generation-counter gp) 20))
              (format t \"Generation ~S~%\" (generation-counter gp)))
            (advance gp))
          (destructuring-bind (fittest . fitness) (fittest gp)
            (format t \"Best fitness: ~S for~%  ~S~%\" fitness fittest))))




(defclass expression-class ()
  ((result-type
    :initarg :result-type
    :reader result-type
    :documentation "Expressions belonging to this expression class
    must evaluate to a value of this lisp type.")
   (weight
    :initform 1
    :initarg :weight
    :reader weight
    :documentation "The probability of an expression class to be
    selected from a set of candidates is proportional to its
    weight."))
  (:documentation "An object of EXPRESSION-CLASS defines two things:
  how to build a random expression that belongs to that expression
  class and what lisp type those expressions evaluate to."))

(defun random-expression-class (expression-classes)
  (random-element expression-classes :key #'weight))

(defclass operator (expression-class)
  ((name
    :initarg :name
    :reader name
    :documentation "A symbol that's the name of the operator.")
   (argument-types
    :initarg :argument-types
    :reader argument-types
    :documentation "A list of lisp types. One for each argument of
    this operator."))
  (:documentation "Defines how the symbol NAME in the function
  position of a list can be combined arguments: how many and of what
  types. The following defines `+` as an operator that adds two
  `FLOAT`s:

      (make-instance 'operator 
                     :name '+
                     :result-type float
                     :argument-types '(float float))

  See the macro [OPERATOR][macro] for a shorthand for the above.

  Currently no lambda list keywords are supported and there is no way
  to define how an expression with a particular operator is to be
  built. See RANDOM-EXPRESSION."))

(defmethod print-object ((operator operator) stream)
  (print-unreadable-object (operator stream :type t)
    (format stream "(~S ~{~S~^ ~}) ~S" (name operator)
            (argument-types operator) (result-type operator))))

(defmacro operator ((name &rest arg-types) result-type &key (weight 1))
  "Syntactic sugar for instantiating operators. The example given for
  [OPERATOR][class] could be written as:

      (operator (+ float float) float)

  See [WEIGHT][(reader expression-class)] for what WEIGHT means."
  `(make-instance 'operator :name ',name :result-type ',result-type
                  :argument-types ',arg-types
                  :weight ,weight))

(defclass literal (expression-class)
  ((builder
    :initarg :builder
    :reader builder
    :documentation "A function of no arguments that returns a random
    literal that belongs to its literal class."))
  (:documentation "This is slightly misnamed. An object belonging to
  the LITERAL class is not a literal itself, it's a factory for
  literals via its BUILDER function. For example, the following
  literal builds bytes:

      (make-instance 'literal
                     :result-type '(unsigned-byte 8)
                     :builder (lambda () (random 256)))

  In practice, one rarely writes it out like that, because the LITERAL
  macro provides a more convenient shorthand."))

(defmethod print-object ((literal literal) stream)
  (print-unreadable-object (literal stream :type t :identity t)
    (format stream "~S" (result-type literal))))

(defmacro literal ((result-type &key (weight 1)) &body body)
  "Syntactic sugar for defining literal classes. The example given for
  [LITERAL][class] could be written as:

      (literal ((unsigned-byte 8))
        (random 256))

  See [WEIGHT][(reader expression-class)] for what WEIGHT means."
  `(make-instance 'literal :result-type ',result-type
                  :weight ,weight
                  :builder (lambda () ,@body)))

(defun random-expression (operators literals type terminate-fn)
  "Return an expression built from OPERATORS and LITERALS that
  evaluates to values of TYPE. TERMINATE-FN is a function of one
  argument: the level of the root of the subexpression to be generated
  in the context of the entire expression. If it returns T then a
  [LITERAL][class] will be inserted (by calling its BUILDER function),
  else an [OPERATOR][class] with all its necessary arguments.

  The algorithm recursively generates the expression starting from
  level 0 where only operators and literals with a RESULT-TYPE that's
  a subtype of TYPE are considered and one is selected with the
  unnormalized probability given by its WEIGHT. On lower levels, the
  ARGUMENT-TYPES specification of operators is similarly satisfied and
  the resulting expression should evaluate without without a type
  error.

  The building of expressions cannot backtrack. If it finds itself in
  a situation where no literals or operators of the right type are
  available then it will fail with an error."
  (labels ((expression-classes-of-type (expression-classes type)
             (loop for expression-class in expression-classes
                   when (subtypep (result-type expression-class) type)
                     collect expression-class))
           (random-literal (type)
             (let ((literals (expression-classes-of-type literals type)))
               (assert literals () "No literals of type ~S available."
                       type)
               (funcall (builder (random-expression-class literals)))))
           (random-operator (type level)
             (let ((operators (expression-classes-of-type operators type)))
               (if (endp operators)
                   (random-literal type)
                   (let ((operator (random-expression-class operators)))
                     (cons (name operator)
                           (mapcar #'(lambda (type)
                                       (random-operator-or-literal
                                        type (1+ level)))
                                   (argument-types operator)))))))
           (random-operator-or-literal (type level)
             (if (funcall terminate-fn level)
                 (random-literal type)
                 (random-operator type level))))
    (random-operator-or-literal type 0)))




(defclass genetic-programming (evolutionary-algorithm)
  ((operators
    :initarg :operators
    :reader operators
    :documentation "The set of [OPERATOR][class]s from which (together
    with [LITERAL][class]s) individuals are built.")
   (literals
    :initarg :literals
    :reader literals
    :documentation "The set of [LITERAL][class]s from which (together
    with [OPERATOR][class]s) individuals are built.")
   (toplevel-type
    :initform t
    :initarg :toplevel-type
    :reader toplevel-type
    :documentation "The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol REAL. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.")
   (randomizer
    :initarg :randomizer
    :reader randomizer
    :documentation "Used for mutations, this is a function of three
    arguments: the GP object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.")
   (selector
    :initarg :selector
    :reader selector
    :documentation "A function of two arguments: the GP object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it copying, mutation or crossover.
    Typically, this defers to HOLD-TOURNAMENT.")
   (copy-chance
    :initform 0
    :initarg :copy-chance
    :accessor copy-chance
    :documentation "The probability of the copying reproduction
    operator being chosen. Copying simply creates an exact copy of a
    single individual.")
   (mutation-chance
    :initform 0
    :initarg :mutation-chance
    :accessor mutation-chance
    :documentation "The probability of the mutation reproduction
    operator being chosen. Mutation creates a randomly altered copy of
    an individual. See RANDOMIZER.")
   (keep-fittest-p
    :initform t
    :initarg :keep-fittest-p
    :accessor keep-fittest-p
    :documentation "If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings."))
  (:documentation "The GENETIC-PROGRAMMING class defines the search
  space, how mutation and recombination occur, and hold various
  parameters of the evolutionary process and the individuals
  themselves."))

(defun random-gp-expression (gp terminate-fn &key (type (toplevel-type gp)))
  "Creating the initial population by hand is tedious. This
  convenience function calls RANDOM-EXPRESSION to create a random
  individual that produces GP's TOPLEVEL-TYPE. By passing in another
  TYPE one can create expressions that fit somewhere else in a larger
  expression which is useful in a RANDOMIZER function."
  (random-expression (operators gp) (literals gp) type terminate-fn))

(defmethod advance ((gp genetic-programming))
  (calculate-fitnesses gp)
  (incf (slot-value gp 'generation-counter))
  (breed gp)
  (rotatef (population gp) (nursery gp)))


;;; Calculate the fitnesses of the current generation. Place them into
;;; FITNESSES. Update FITTEST if necessary.
(defun calculate-fitnesses (gp)
  (let ((fitnesses (fitnesses gp))
        (evaluator (evaluator gp))
        (mass-evaluator (mass-evaluator gp))
        (population (population gp))
        (fittest (fittest gp))
        (changedp nil)
        (fitness-key (fitness-key gp)))
    (unless (= (length fitnesses) (length population))
      (setq fitnesses (make-array (length population)))
      (setf (slot-value gp 'fitnesses) fitnesses))
    (fill fitnesses nil)
    (if mass-evaluator
        (funcall mass-evaluator gp population fitnesses)
        (map-into fitnesses (lambda (individual)
                              (funcall evaluator gp individual))
                  population))
    (loop for individual across population
          for fitness across fitnesses
          do (let ((real-fitness (funcall fitness-key fitness)))
               (when (or (null fittest)
                         (< (funcall fitness-key (cdr fittest))
                            real-fitness))
                 (setq fittest (cons individual fitness))
                 (setf (slot-value gp 'fittest) fittest)
                 (setq changedp t))))
    (when (and changedp (fittest-changed-fn gp))
      (funcall (fittest-changed-fn gp) gp
               (car (fittest gp)) (cdr (fittest gp))))))

(defun breed (gp)
  (when (null (nursery gp))
    (setf (slot-value gp 'nursery)
          (make-array 0 :adjustable 0 :fill-pointer t)))
  (let* ((population (population gp))
         (n (population-size gp))
         (copy-chance (copy-chance gp))
         (mutation-chance (mutation-chance gp))
         (nursery (nursery gp)))
    (setf (fill-pointer nursery) 0)
    (when (keep-fittest-p gp)
      (let ((i (nth-value 1 (max-position/vector (fitnesses gp)
                                                 :key (fitness-key gp)))))
        (vector-push-extend (aref population i) nursery)))
    (loop while (< (length nursery) n) do
      (let* ((what (random 1.0)))
        (cond ((< what copy-chance)
               (copy-some gp population nursery))
              ((< what (+ copy-chance mutation-chance))
               (mutate-some gp population nursery))
              (t
               (crossover-some gp population nursery)))))))

(defun copy-some (gp population nursery)
  (let ((selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (vector-push-extend x nursery))))

(defun mutate-some (gp population nursery)
  (let ((selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (vector-push-extend (mutate-expression gp x) nursery))))

(defun crossover-some (gp population nursery)
  (let ((n (population-size gp))
        (selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (let ((y (aref population (funcall selector gp fitnesses))))
        (multiple-value-bind (a b) (crossover-expressions gp x y)
          (vector-push-extend a nursery)
          (when (< (length nursery) n)
            (vector-push-extend b nursery)))))))

(defun mutate-expression (gp x)
  (let* ((xs (count-nodes x))
         (xi (random xs))
         (type (node-expected-type gp x xi)))
    (change-node-at x xi (funcall (randomizer gp) gp type (node-at x xi)))))

(defun crossover-expressions (gp x y)
  (loop for i upfrom 1 do
    (when (zerop (mod i 1000))
      (warn "Crossover having trouble with ~A ~A ~A~%" gp x y))
    ;; Literals more likely to be selected than operators because
    ;; there are more of them in the tree. To counter this, following
    ;; Koza, choose and internal node 90% of the time.
    (let* ((x-internal (and (not (atom x)) (< (random 1.0) 0.9)))
           (y-internal (and (not (atom y)) (< (random 1.0) 0.9)))
           (xs (count-nodes x :internal x-internal))
           (ys (count-nodes y :internal y-internal))
           (xi (random xs))
           (yi (random ys))
           (xin (node-at x xi :internal x-internal))
           (yin (node-at y yi :internal y-internal))
           (xi-type (node-expected-type gp x xi :internal x-internal))
           (yi-type (node-expected-type gp y yi :internal y-internal))
           (xi-expected-type (node-expected-type gp x xi
                                                 :internal x-internal))
           (yi-expected-type (node-expected-type gp y yi
                                                 :internal y-internal)))
      (when (and (subtypep xi-type yi-expected-type)
                 (subtypep yi-type xi-expected-type))
        (return-from crossover-expressions
          (values (change-node-at x xi yin :internal x-internal)
                  (change-node-at y yi xin :internal y-internal)))))))

;;; Return the type its parent expects of node XI of X.
(defun node-expected-type (gp x xi &key internal)
  (multiple-value-bind (parent index)
      (parent-of-node-at x xi :internal internal)
    (if parent
        (elt (argument-types
              (find-operator gp (first (node-at x parent :internal internal))))
             index)
        (toplevel-type gp))))

(defun find-operator (gp name)
  (or (find name (operators gp) :key #'name)
      (error "Operator ~S is undefined." name)))

(defun hold-tournament (fitnesses &key select-contestant-fn n-contestants key)
  (let* ((n (length fitnesses))
         (size (min n-contestants n))
         (v (make-array size :adjustable t :fill-pointer 0))
         (key (or key #'identity)))
    (loop while (< (length v) size) do
      (let ((i (if select-contestant-fn
                   (funcall select-contestant-fn fitnesses)
                   (random n))))
        (unless (find i v)
          (insert-into-sorted-vector
           i v
           :test (lambda (x y)
                   (> (funcall key (aref fitnesses x))
                      (funcall key (aref fitnesses y))))))))
    (aref v 0)))

(defun map-tree (fn tree)
  (funcall fn tree)
  (when (listp tree)
    (dolist (child (rest tree))
      (funcall fn child))))

(defmacro dotree ((node tree &optional result-form) &body body)
  `(progn (map-tree (lambda (,node) ,@body) ,tree)
          ,result-form))

(defun count-nodes (tree &key internal)
  "Count the nodes in the sexp TREE. If INTERNAL then don't count the
  leaves."
  (if (atom tree)
      (if internal 0 1)
      (1+ (loop for child in (rest tree)
                summing (count-nodes child :internal internal)))))

(defun node-at (x index &key internal)
  (let ((n index))
    (labels ((traverse (node)
               (unless (and internal (atom node))
                 (when (zerop n)
                   (return-from node-at node))
                 (decf n))
               (when (listp node)
                 (mapc #'traverse (rest node)))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

(defun parent-of-node-at (x index &key internal)
  (let ((n 0)
        *parent-index*
        *child-index*)
    (declare (special *parent-index* *child-index*))
    (labels ((traverse (node)
               (unless (and internal (atom node))
                 (when (= n index)
                   (return-from parent-of-node-at (values *parent-index*
                                                          *child-index*)))
                 (incf n))
               (when (listp node)
                 (let ((*parent-index* (1- n))
                       (*child-index* 0))
                   (declare (special *parent-index* *child-index*))
                   (dolist (child (rest node))
                     (traverse child)
                     (incf *child-index*))))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

(defun change-node-at (x index value &key internal)
  (let ((n -1))
    (labels ((traverse (node)
               (unless (and internal (atom node))
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
(defun max-position/vector (vector &key (test #'<) key)
  "Return the maximum value (according to TEST) in VECTOR and its index."
  (let ((has-max-p nil)
        max
        pos)
    (dotimes (i (length vector) (values max pos))
      (let ((x (if key
                   (funcall key (aref vector i))
                   (aref vector i))))
        (when (or (not has-max-p)
                  (funcall test max x))
          (setq has-max-p t
                max x
                pos i))))))

(defun insert-into-sorted-vector (item vec &key
                                  (max-length (array-total-size vec))
                                  (test #'<))
  (let* ((len (length vec))
         (pos (or (position item vec :test test) len)))
    (cond ((< len max-length)
           (vector-push-extend nil vec)
           (replace vec vec :start1 (1+ pos) :start2 pos :end2 len)
           (setf (aref vec pos) item))
          ((<= max-length len)
           (unless (zerop pos)
             (replace vec vec :start1 0 :start2 1 :end2 pos)
             (setf (aref vec (1- pos)) item))))))

(defun random-element (seq &key (key #'identity)
                       (start 0) (end (length seq))
                       (sum (sum-seq seq :key key :start start :end end)))
  (let ((x (random (float sum 0d0))))
    (do* ((i start (1+ i))
          (e (elt seq i) (elt seq i))
          (s (funcall key e) (+ s (funcall key e))))
         ((or (<= x s) (>= i (1- end))) (values e i)))))

(defun sum-seq (seq &key (key #'identity) (start 0) (end (length seq)))
  (if (typep seq 'list)
      (loop repeat (- end start)
            for l = (nthcdr start seq) then (cdr l)
            sum (funcall key (car l)))
      (loop for i upfrom start below end
            sum (funcall key (aref seq i)))))

