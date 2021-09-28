;; Macro utils

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

;; Exercise 1
(defvar *x* 'a)
(defvar *y* 'b)
(defvar *z* '(c d))

`(,*z* ,*x* *z*) ; a
`(*x* ,*y* ,@*z*) ; b
`((,@*z* ,*x*) *z*) ; c

;; Exercise 2
(defmacro my-if (condition if-form else-form)
  `(cond (,condition ,if-form)
         (t ,else-form)))

(my-if (> (random 3) 1)
       "bulbassaur"
       (print "not good brah"))

;; Exercise 3
(defmacro nth-expr (n &rest exprs)
  (if (integerp n)
      (nth n exprs)
      `(case ,n
       ,@(let ((i -1))
           (mapcar #'(lambda(x) `(,(incf i) ,x)) exprs)))))

(nth-expr 2 (print 1) (print 2) (print 3))

(let ((i 2))
  (nth-expr i (print 1) (print 2) (print 3)))

;; Exercise 4
(defmacro ntimes (n &body body)
  (with-gensyms (times rec-fun)
    `(let ((,times ,n))
       (labels ((,rec-fun (n)
                  (when (> n 0)
                    ,@body
                    (,rec-fun (- n 1)))))
         (,rec-fun ,times)))))

(ntimes 3
  (print 1)
  (print 2))

(let ((i 3))
  (ntimes i (print "lol")))

;; Exercise 5
(defmacro n-of (n expr)
  `(mapcar (lambda (x) ,expr) (make-list ,n)))

(let ((i 0) (n 4))
  (n-of n (incf i))) 

;; Exercise 6
(defmacro with-value-reverting (vars &body body)
  (let 
   (
    (prog1 
       (progn ,@body) 
       ,@(mapcar (lambda (var sym) `(setf ,var ,sym)) vars syms)))))

(let ((i 3) (j 5))
  (with-value-reverting (i j) 
    (print i)
    (setf i 32)
    (print i)
    (print j)
    (setf j -3)
    (print j))
  (print i)
  (print j))

;; Exercise 7

;; Exercise 8
