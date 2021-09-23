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

;; Exercise 3
(defun run-contest (&optional 
                     (singers 10) 
                     (contestants '(:wigglies :wobblies)))
  (let ((prizes nil))
    (do ((i singers (- i 1)))
        ((<= i 0) prizes)
      (let* ((singer (random-choice contestants))
             (prize (assoc singer prizes)))
        (if prize
            (incf (cdr prize))
            (setf prizes (cons (cons singer 1) prizes)))))))


(defun random-choice (lst)
  (if lst
      (nth (random (length lst)) lst)
      nil))

;; The committee seems legit.

;; Exercise 4

(defun slope (p1 p2)
  (destructuring-bind ((x1 y1) (x2 y2)) (list p1 p2)
    (let ((xd (- x2 x1)))
     (if (zerop xd)
         nil
         (/ (- y2 y1) xd)))))

(defun y-intercept (p1 p2)
  (destructuring-bind ((x1 y1) (x2 y2)) (list p1 p2)
    (let ((xd (- x1 x2)))
      (if (zerop xd)
          nil
          (/ (- (* x1 y2) (* x2 y1)) xd)))))

(defun in-range (x y s)
  (destructuring-bind ((x1 y1) (x2 y2)) s
    (and (>= x x1) (>= y y1) (>= x2 x) (>= y2 y))))

;; todo finish this shit later
(defun intersect (s1 s2)
  (destructuring-bind ((p11 p12) (p21 p22)) (list s1 s2)
    (let* ((slope-1 (slope p11 p12))
           (slope-2 (slope p21 p22))
           (y-intercept-1 (y-intercept p11 p12))
           (y-intercept-2 (y-intercept p21 p22))
           (sloped (if (and slope-1 slope-2) 
                       (- slope-1 slope-2)
                       0)))
      (if (zerop sloped)
          nil
          (cond ((and y-intercept-1 y-intercept-2)
                 (let* ((x (/ (- y-intercept-2 y-intercept-1) sloped))
                        (y (+ (* slope-1 x) y-intercept-1)))
                        (if (and (in-range x y s1) (in-range x y s2))
                            (values x y)
                            nil)))
                ((null y-intercept-1)))))))




;; Exercise 8
;; It seems SBCL uses 64 bits aka 8 bytes

;; Exercise 9
;; Seemingly 2 for SBCL
