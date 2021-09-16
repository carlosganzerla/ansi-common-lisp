;; Exercise 1

;; a
((lambda (y)
   ((lambda (x)
      (cons x x))
    (car y))) (list 1 2 3 4))

;; b
((lambda (x z)
  ((lambda (w)
    ((lambda (y)
      (cons w y))
     (+ w z)))
   (car x)))
 (list 3 2 1)
 6)

;; Exercise 2
(defun mistery (x y)
  (cond ((null y) nil)
        ((eql (car y) x) 0)
        (t (let ((z (mistery x (cdr y))))
            (+ z 1)))))

;; Exercise 3
(defun square-arg (x)
  (when (or (> x 5)
            (<= x 0))
    (* x x)))

;; Exercise 4
(defconstant month
             #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun month-num (m y)
  (+ (case (- m 1)
        (0 0)
        (1 31)
        (2 59)
        (3 90)
        (4 120)
        (5 151)
        (6 181)
        (7 212)
        (8 243)
        (9 273)
        (10 304)
        (11 334)
        (12 365)
        (otherwise (throw 'gg "wrong month mtf")))
     (if (and (> m 2) (leap? y)) 1 0)))

;; Exercise 5

(defun precedes (obj vec)
  (defun inner-precedes (acc idx)
    (cond ((>= idx (length vec)) acc)
          ((eql idx 0) (inner-precedes acc (+ idx 1)))
          (t (let ((current (aref vec idx)) (prev (aref vec (- idx 1))))
               (inner-precedes 
                 (cond ((eql current obj) (cons prev acc))
                       (t acc))
                 (+ idx 1))))))
  (inner-precedes nil 0))

(defun iter-precedes (obj vec)
  (let ((lst nil))
   (do ((i 1 (+ i 1))) 
       ((>= i (length vec)) lst)
       (if (eql (aref vec i) obj)
           (setf lst (cons (aref vec (- i 1)) lst))))))

;; Exercise 6
(defun intersperse (obj lst)
  (defun inner-intersperse (acc lst)
    (let ((rest (cdr lst))) 
      (cond ((null lst) acc)
            (t (let ((added (cons (car lst) acc))) 
                 (inner-intersperse 
                   (cond (rest (cons obj added))
                         (t added)) 
                   rest))))))
  (reverse (inner-intersperse nil lst)))

(defun iter-intersperse (obj lst)
  (let ((result nil))
   (dolist (e lst (reverse (cdr result)))
     (setf result (cons obj (cons e result))))))

;; Exercise 7

(defun succ-1-rec (lst)
  (progn 
    (defun inner (lst curr next)
      (cond ((null next) t)
            ((and (eql (+ curr 1) next) (inner (cdr lst) next (car lst))) t)
            (t nil)))
    (let ((curr (car lst)) (next (cadr lst))) 
      (if (null next)
          nil
          (inner (cddr lst) curr next)))))

(defun succ-1-iter (lst)
  (let ((curr (car lst)) (next (cadr lst)) (rest (cddr lst))) 
    (if (null next) 
        nil
        (let ((failed nil))
          (do ((i 0)) ((or failed (null next)) (not failed))
                (setf failed (not (eql (+ curr 1) next)))
                (setf curr next)
                (setf next (car rest))
                (setf rest (cdr rest)))))))

(defun succ-1-mapc (lst)
  (let ((result nil)) 
    (progn (mapc (lambda (x y)
                   (if (or (null x) (null y) (not (eql (+ x 1) y)))
                       (return-from succ-1-mapc nil)
                       (setf result t))) 
                 lst (cdr lst))
           result)))

;; Exercise 8
(defun max-min (vec)
  (let ((len (length vec)))
    (progn
      (defun inner-max-min (idx max min)
        (if (>= idx len)
            (values max min)
            (let ((current (svref vec idx)))
              (inner-max-min (+ idx 1) 
                             (cond ((> current max) current)
                                   (t max))
                             (cond ((< current min) current)
                                   (t min)))))))
    (if (eql 0 len)
        nil
        (inner-max-min 0 (svref vec 0) (svref vec 0)))))

;; Exercise 9
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (let* ((paths (append (cdr queue) (new-paths path node net)))
                    (end-path (find end paths 
                                    :test (lambda (e p) (eql (car p) e)))))
                (if end-path
                    (progn
                      (format t "~A~%" queue)
                      (return-from bfs end-path))
                    (bfs end paths net))))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net)))) 
