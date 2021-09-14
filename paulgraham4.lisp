;; Exercise 1
(defun quarter-turn (arr)
  (let* ((dim (array-dimensions arr))
         (rows (car dim))
         (cols (cadr dim))
         (transposed (make-array dim :initial-element nil)))
      (if (eql cols rows)
          (do ((i 0 (+ 1 i)))
              ((>= i rows) transposed)
              (do ((j 0 (+ 1 j)))
                  ((>= j cols))
                  (setf (aref transposed j (- rows i 1)) (aref arr i j))))
          nil)))



;; Exercise 2
(defun copy-list-2 (lst)
  (reverse (reduce (lambda (acc e) (cons e acc)) lst :initial-value nil)))

(defun reverse-2 (lst)
  (reduce (lambda (acc e) (cons e acc)) lst :initial-value nil))

;; Exercise 3
(defstruct node 
  (left nil) (mid nil) (right nil) val)

(defun copy-node (node)
  (if (null node)
      nil
      (make-node 
        :val (node-val node)
        :left (copy-node (node-left node))
        :mid (copy-node (node-mid node))
        :right (copy-node (node-right node)))))

(defun node-member-p (obj node)
  (and node 
       (if (eql (node-val node) obj)
           t
           (or (node-member-p obj (node-left node))
               (node-member-p obj (node-mid node))
               (node-member-p obj (node-right node))))))

(setf *tree*
  (make-node
    :val 3
    :left (make-node :val 5)
    :mid (make-node :val 'a :right (make-node :val 12))
    :right (make-node :val 99 :mid (make-node :val 16) :left (make-node :val 77))))

;; Exercise 4
(defstruct (bst (:print-function
                   (lambda (n s d)
                     (format s "#<~A>" (bst-val n)))))
  val (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-bst :val obj)
      (let ((val (bst-val bst)))
        (if (funcall < obj val)
            (make-bst
              :val val
              :l (bst-insert obj (bst-l bst) <)
              :r (bst-r bst))
            (make-bst
              :val val
              :r (bst-insert obj (bst-r bst) <)
              :l (bst-l bst))))))

(defun bst-to-list-desc (bst acc)
  (if (null bst)
      acc
      (let* ((l-acc (bst-to-list-desc (bst-l bst) acc)) 
             (m-acc (cons (bst-val bst) l-acc))
             (r-acc (bst-to-list-desc (bst-r bst) m-acc)))
        r-acc)))

;; Exercise 5
(defun bst-adjoin (obj bst <)
  (if (null bst)
      (make-bst :val obj)
      (let ((val (bst-val bst)))
        (if (eql obj val)
            bst
            (if (funcall < obj val)
                (make-bst
                  :val val
                  :l (bst-insert obj (bst-l bst) <)
                  :r (bst-r bst))
                (make-bst
                  :val val
                  :r (bst-insert obj (bst-r bst) <)
                  :l (bst-l bst)))))))

;; Exercise 6
(defun assoc-to-hash (alist)
  (let ((table (make-hash-table)))
    (progn
      (mapcar (lambda (a)
                (setf (gethash (car a) table) (cdr a)))
              alist)
      table)))

(defun hash-to-assoc (table)
  (let ((alist nil))
    (progn
      (maphash (lambda (k v)
               (setf alist (cons (cons k v) alist)))
             table)
      alist)))
