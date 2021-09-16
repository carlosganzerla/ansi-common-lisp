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
