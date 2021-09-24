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
    (and (>= x (min x1 x2)) (>= y (min y1 y2))
          (>= (max x1 x2) x) (>= (max y1 y2) y))))

(defun get-y (x slope y-intercept)
  (values x (+ (* slope x) y-intercept)))


(defun intersect (s1 s2)
  (destructuring-bind ((p11 p12) (p21 p22)) (list s1 s2)
    (let* ((slope-1 (slope p11 p12))
           (slope-2 (slope p21 p22))
           (y-intercept-1 (y-intercept p11 p12))
           (y-intercept-2 (y-intercept p21 p22)))
      (cond ((and (not y-intercept-1) (not y-intercept-2)) nil)
            ((not y-intercept-1) (get-y (car p11) slope-2 y-intercept-2))
            ((not y-intercept-2) (get-y (car p21) slope-1 y-intercept-1))
            ((and slope-1 slope-2)
             (let ((sloped (- slope-1 slope-2)))
               (when (not (zerop sloped))
                 (get-y (/ (- y-intercept-2 y-intercept-1) sloped)
                        slope-1
                        y-intercept-1))))))))

(defun intersect-range (s1 s2)
  (multiple-value-bind (x y) (intersect s1 s2)
    (when (and (in-range x y s1) (in-range x y s2))
      (values x y))))


;; Exercise 5
;; This way is dumb, there's a better way but 2 lazy to search for it
(defun root (f min max e)
  (do ((i min (+ i e)))
      ((or (>= e (funcall f i) (- e))
           (> i max)) 
       i)))

(defun fx (x)
  (+ (* 0.3123 x x) (- (* 2.313 x)) .0654))

;; Exercise 6

(defun horner (x &rest args)
  (labels
    ((evaluate (coeffs acc)
       (if (not coeffs)
           acc
           (evaluate (cdr coeffs) (+ (* x acc) (car coeffs))))))
    (let ((a (car args)) (b (cadr args)))
      (cond ((not args) 0)
        ((not b) a)
        (t (evaluate (cddr args) (+ (* x a) b)))))))

;; Exercise 8
;; It seems SBCL uses 64 bits aka 8 bytes

;; Exercise 9
;; Seemingly 2 for SBCL
