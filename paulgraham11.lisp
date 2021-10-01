;; Exercise 1
(defmacro with-stuff (cls &rest symbols)
  `(defclass ,cls () 
     ,(mapcar (lambda (sym) 
                `(,sym :accessor ,sym
                       :initarg ,(intern (string sym) "KEYWORD")
                       :initform 1))
              symbols)))

(with-stuff rectangle height width)

(with-stuff circle radius)

(defmethod area ((x rectangle))
  (* (height x) (width x)))

(defmethod area ((x circle))
  (* pi (expt (radius x) 2)))

(defmethod area (x)
  (format t "~A have no mtfking area bishhh" x))

;; Exercise 2 IGNORED

;; Exercise 3
;; for a
;; A, C, D, E, F, G, H, Standard Class, T

;; for b
;; B, D, E, F, G, H, C, Standard Class, T


;; Exercise 4

;; Note, won't work because we assume that some helpers exist.
(defun most-spec-app-meth (fn args)
  (let* ((meths (methods fn)) 
        (specs (mapcar #'specilizations meths))
        (arg-spec (mapcar #'cadr args))
        (found-spec (find arg-spec specs :test #'equal)))
    (when found-spec
      (car (precedence found-spec)))))


;; Exercise 5
(defvar *counter* 0)

(defmethod area :before (x)
  (incf *counter*))

;; Exercise 6
;; If we wanted to change the behavior of a method of a given class based
;; on its arguments, without specialization it would suck because we would
;; have to add some predicates to do that. This makes the code hard to change


