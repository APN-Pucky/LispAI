(defun make-bin-tree-leaf (e) (return-from make-bin-tree-leaf e))
(defun make-bin-tree-node (e b1 b2) (list e b1 b2))
(defun bin-tree-leaf-element (l) (first l))
(defun bin-tree-node-element (n) (first n))
(defun bin-tree-node-left (n) (second n))
(defun bin-tree-node-right (n) (third n))
(defun bin-tree-leaf-p (b) (and (listp b) (= (list-length b) 1)))
(defun bin-tree-node-p (b) (and (listp b) (= (list-length b) 3)))

