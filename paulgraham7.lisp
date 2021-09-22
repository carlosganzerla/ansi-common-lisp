;; Exercise 1

(defun read-all-lines (path)
  (with-open-file (str path :direction :input)
    (let ((lines nil))
      (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql line 'eof) (nreverse lines))
          (setf lines (cons line lines))))))

;; Exercise 2

(defun read-all-expressions (path)
  (with-open-file (str path :direction :input)
    (let ((exprs nil))
      (do ((expr (read str nil 'eof) (read str nil 'eof)))
          ((eql expr 'eof) (nreverse exprs))
          (setf exprs (cons expr exprs))))))

;; Exercise 3
(defun replace-comments (input-path output-path)
  (with-open-file (in input-path :direction :input)
    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede)
      (labels ((next-char ()
                 (peek-char nil in nil 'eof))
               (current-char (next)
                 (cond ((eql next #\%) (progn (read-line in nil 'eof) nil))
                       (t (read-char in nil 'eof)))))
        (do* ((next (next-char) (next-char))
               (current (current-char next) (current-char next)))
          ((eql current 'eof))
          (if current
            (write-char current out)
            (write-char #\Newline out)))))))

;; Exercise 4
(defun print-floats (arr)
  (destructuring-bind (rows &optional cols) (array-dimensions arr)
    (labels 
      ((print-floats-loop (idx)
         (if (> rows idx)
             (progn
               (format t "~,2F~10t~,2F~%" (aref arr idx 0) (aref arr idx 1))
               (print-floats-loop (1+ idx))))))
      (cond ((eql 2 cols) (print-floats-loop 0))
            (t nil)))))

;; Remaking replacer instead of exercise 5 and 6.

;; TODO: Do this shit later (someday, IDK)

(defun replace-file (input-file old output-file new)
  
  )

(defun replace-str (in out old new)
  (let* ((match-pos 0) 
         (len (length new))
         (buffer (make-array len))
         (first-match (svref old 0)))
    (do ((next (read-char in nil 'eof) (read-char in nil 'eof)))
        ((eql next 'eof))
          (cond ((eql (aref old match-pos) next)
                 (setf (svref buffer match-pos) next)
                 (setf match-pos (1+ match-pos))
                 (cond ((eql match-pos len)
                        (princ new out)
                        (setf match-pos 0))))
                ((eql match-pos 0) (princ next out))
                (t (flush-buffer out next buffer))
                
                )
          

        )
    )
  
)

(defun flush-buffer (out next buffer)
  (flush out buffer 0)
  (do ((pos 1 (1+ pos))
       ((not ()))
       )
      
      )
  )

(defun flush (out buffer idx)
  (princ (svref buffer idx) out)
  (setf (svref buffer idx) nil))
