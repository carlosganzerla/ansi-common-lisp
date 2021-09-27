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
  (nth (1- n) exprs))

(nth-expr 2 (print 1) (print 2) (print 3))


;; Exercise 4
(defmacro ntimes-rec (n &body body)
  (labels ((rec (x)
             `(when ,(> x 0)
                 ,@body 
                 ,(rec (- x 1)))))
    (rec n)))

(defmacro puta (x)
  `(,x))

(let ((pika 3)) 
  (puta pika))

(ntimes-rec 0 
  (print 1)
  (print 2))

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
           ,@body)))) 

;; Exercise 5
(defmacro n-of (n expr)
  `(mapcar (lambda (x) ,expr) (make-list ,n)))

(let ((i 0) (n 4))
  (n-of n (incf i))) 

(let ((i 3))
  (ntimes-rec i (print "lol")))


;; Exercise 6
(defmacro revert-values (vars &body body)
  
  )

  
