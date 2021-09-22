(defpackage :my-package
  (:use :cl)
  (:export #:abc))


(in-package :my-package)

(defun abc ()
  (print "waddap"))
