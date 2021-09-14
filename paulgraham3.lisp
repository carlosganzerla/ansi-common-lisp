(defvar *min* nil)
(setf *min* '(( a b c) (b c) (c d)))
(cdr (assoc 'a *min*))

;; Ex 2.

(defun new-union (lst1 lst2)
  (if (null lst2)
          lst1
          (new-union
            (if (member (car lst2) lst1)
                lst1
                (append lst1 (list (car lst2))))
            (cdr lst2))))

;; Ex 3.
(defun occurrences (lst)
  (let ((result (occurrences-loop lst nil))
        (sort-fn (lambda (ele tup) (> (cdr ele) (cdr tup)))))
    (sort result sort-fn)))

(defun occurrences-loop (lst acc)
  (if (null lst)
      acc
      (let ((current (car lst))
            (finder (lambda (ele tup) (eql ele (car tup)))))
        (if (member current (mapcar #'car acc))
            (occurrences-loop
              (cdr lst)
              (cons
                (cons current (+ 1 (cdr (find current acc :test finder))))
                (remove current acc :test finder)))
            (occurrences-loop (cdr lst) (cons (cons current 1) acc))))))

;; Ex 4
;; Returns NIL cuz tha shit uses eql
(member '(a) '((a) (b))) ; nil
(member '(a) '((a) (b)) :test #'equal) ; ((A) (B))

;; Ex 5
;; Using mapcar
(defun pos+ (lst)
  (let* ((pos -1)
        (adder (lambda (ele)
                 (setf pos (+ pos 1))
                 (+ ele pos))))
    (mapcar adder lst)))

;; iterative
(defun pos+ (lst)
  (let ((pos 0) (acc nil))
    (dolist (ele lst)
      (setf acc (cons (+ pos ele) acc))
      (setf pos (+ pos 1)))
    (reverse acc)))

;; Recursive
(defun pos+ (lst)
  (reverse (loop-pos+ lst nil 0)))

(defun loop-pos+ (lst acc pos)
  (if (null lst)
      acc
      (loop-pos+ (cdr lst) (cons (+ (car lst) pos) acc) (+ pos 1))))

;; Ex 6
;; Just exchange cdr for car and car for cdr

;; Ex 7
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
          (if (eql next elt)
              (compr elt (+ n 1) (cdr lst))
              (cons (n-elts elt n)
                    (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt)) 

;; Ex 8
(defun show-dots (lst)
  (let ((head (car lst)) (tail (cdr lst))) 
    (if (null tail)
        (format t "(~A . NIL)" head)
        (progn
          (format t "(~A . " head)
          (show-dots-rec tail)
          (format t ")")))))

;; Ex 9
(defun longest-path (start end net)
  (let ((paths (search-net start (cdr (assoc start net)) end nil nil net))
        (sort-fn (lambda (x y) (> (length x) (length y)))))
    (car (sort paths sort-fn))))

(defun search-net (node neighbors end path paths net)
  (if (eql node end)
      (cons (cons node path) paths)
      (reduce 
        (lambda (acc neighbor)
          (if (member neighbor path) 
              acc
              (append 
                (search-net 
                  neighbor 
                  (cdr (assoc neighbor net)) 
                  end
                  (cons node path)
                  acc
                  net)
                acc)))
        neighbors
        :initial-value nil)))
