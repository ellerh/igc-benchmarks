;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun hash-strings (n)
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (i n)
      (setf (gethash (number-to-string i) table) i))
    (maphash (lambda (key value)
	       (cl-incf (gethash key table) value))
	     table)))

(defun main ()
  (dotimes (_ 80)
    (hash-strings 100000))
  (kill-emacs))

;; EOF
