(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(defun gen-list (val len)
  (defun inner (acc)
    (if (eql (length acc) len)
      acc
      (inner (cons val acc))))
  (inner ()))

(defun mystery (x y)
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (let ((z (mystery x (cdr y))))
        (and z (+ z 1))))))

(car (car (cdr '( a (b c) d))))

(if t 13 (/ 1 0))
(apply #'list 1 nil)

(defun has-list (x)
  (and (not (null x))
       (if (listp (car x))
         t
         (has-list (cdr x)))))

(defun print-dots-rec (x)
  (if (> x 0)
    (progn
      (format t ".")
      (print-dots-rec (- x 1)))
    nil))

(defun print-dots-iter (len)
  (do ((i 0 (+ i 1)))
    ((>= i len) nil)
    (format t ".")))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
    ((> i end) 'done)
    (format t "~A ~k~l" i (* i i))) )

(defun ocurrences-rec (lst sym count)
  (if (null lst)
    count
    (ocurrences-rec (cdr lst) sym (if (eql (car lst) sym)
                                      (+ count 1)
                                      count))))
(defun occurrences-iter (lst sym)
  (let ((cnt 0))
    (dolist (e lst)
      (if (eql e sym)
          (setf cnt (+ cnt 1))
          nil))
    cnt))

(adjoin 'a '(a b c))

(defun our-length (1st)
  (let ((len 0))
    (dolist (obj 1st)
      (setf len (+ len 1)))
    len)) 
