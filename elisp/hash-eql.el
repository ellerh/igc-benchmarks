;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun hash-integers (n)
  (let ((table (make-hash-table :test 'eql)))
    (dotimes (i n)
      (setf (gethash i table) i))
    (maphash (lambda (key value)
	       (cl-incf (gethash key table) value))
	     table)))

(defun main ()
  (dotimes (_ 40)
    (hash-integers 100000))
  (kill-emacs))

;; EOF
