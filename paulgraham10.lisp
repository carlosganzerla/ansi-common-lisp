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
(defmacro retain (params &body body)
  `((lambda ,params ,@body) ,@params))

(let ((i 3) (j 5))
  (retain (i j) 
    (print i)
    (setf i 32)
    (print i)
    (print j)
    (setf j -3)
    (print j))
  (print i)
  (print j)
  nil)

;; Exercise 7

;; This call evaluates lst twice, so if the evaluation of ,lst changes the
;; list, it won't work as expected.

(defmacro my-push (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

(let ((lst (list 1 2 5)))
  (my-push (pop lst) (cdr (nreverse lst))))

(let ((lst (list 1 2 5)))
  (push (pop lst) (cdr (nreverse lst))))

;; Exercise 8

(defmacro my-double (x)
  (with-gensyms (val)
    `(let ((,val ,x))
       (setf ,x (* ,val 2)))))

(let ((x 1) (y 3) (lst (list 10 2 3 4)))
  (my-double x)
  (my-double y)
  (my-double (car lst))
  (print x)
  (print y)
  (print lst)
  nil)
