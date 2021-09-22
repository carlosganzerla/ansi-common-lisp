;; Exercise 1
(defun is-non-decreasing (lst)
  (let ((prev nil))
    (dolist (e lst)
      (when (and (not (null prev)) (> e prev))
        (return t))
      (setf prev e))))

;; Exercise 2
(defun get-coins (cents &optional (coins (list 25 10 5 1)))
  (labels 
    ((get-coins-inner (coins remaining acc)
       (if coins
           (let* ((coin (car coins))
                  (ratio (/ remaining coin))
                  (usable (floor ratio)))
             (get-coins-inner (cdr coins) 
                              (- remaining (* coin usable)) 
                              (cons usable acc)))
           (nreverse acc))))
    (get-coins-inner coins cents nil)))

;; Exercise 8
;; It seems SBCL uses 63 bits (maybe I'm missing the 64th bit.)

;; Exercise 9
;; Seemingly 2 for SBCL
