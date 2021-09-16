;; Exercise 1

(defun some-fun (str &key (test #'identity) (start 0))
  (format t str))

;; Exercise 2
(defun bin-search (obj vec &key
                       (key #'identity)
                       (test #'eql)
                       (start 0)
                       (end (- (length vec) 1)))
  (finder obj vec start end key test))

(defun finder (obj vec start end key test)
  (let ((range (- end start)))
    (if (>= 0 range)
        (let ((current (aref vec start)))
          (if (funcall test obj (funcall key current))
              current
              nil))
        (let* ((mid (+ start (round (/ range 2))))
               (current (aref vec mid))
               (obj2 (funcall key current)))
          (if (< obj obj2)
              (finder obj vec start (- mid 1) key test)
              (if (> obj obj2)
                  (finder obj vec (+ mid 1) end key test)
                  current))))))

;; Exercise 3

(defun arg-count (&rest args)
  (length args))

;; Exercise 4
(defun most (fn lst)
  (labels 
    ((most-rec (lst max submax)
       (destructuring-bind 
         ((&optional (current nil) &rest tail) 
          (max max-score)
          (submax submax-score)) 
         (list lst max submax)
         (if (null current)
             (values max submax)
             (let* ((current-score (funcall fn current))
                    (new-max 
                      (cond ((or (not max-score)
                                 (> current-score max-score))
                             (list current current-score))
                            (t (list max max-score))))
                    (new-submax 
                      (cond ((not max-score) (list submax submax-score))
                            ((> current-score max-score) (list max max-score))
                            ((or (not submax-score) 
                                 (> current-score submax-score)) 
                             (list current current-score))
                            (t (list submax submax-score)))))
               (most-rec tail new-max new-submax))))))
    (most-rec lst (list nil nil) (list nil nil))))


;; Exercise 5
(defun filter (fn lst)
  (remove-if (lambda (x) (not (funcall fn x))) lst))

(defun filter2 (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun remove-if2 (fn lst)
  (filter2 (lambda (x) (not (funcall fn x))) lst))

;; Exercise 6
(let ((greatest nil))
  (defun greatest-arg (x)
    (if (or (not greatest) (> x greatest))
        (setf greatest x)
        greatest)))


;; Exercise 7
(let ((last-arg nil))
  (defun is-arg-greater (x)
    (let ((result (cond ((and last-arg (> x last-arg)) t)
                        (t nil))))
      (setf last-arg x)
      result)))


;; Exercise 8
(defun expensive (x)
  (format t "Dont call me, I'm expensive. Value: ~A~%" x)
  (* x x))

;; Memoization
(let ((memo-table nil))
  (defun frugal (x)
    (let ((memo-result (assoc x memo-table)))
      (if memo-result
          (cdr memo-result)
          (let ((result (expensive x))) 
            (progn
              (setf memo-table (cons (cons x result) memo-table))
              result))))))

;; Exercise 9
(defun apply-octal (fn arg &rest args)
  (let ((*print-base* 8))
    (apply fn arg args)))

(defun print-nums (&rest nums)
  (mapc #'print nums))
