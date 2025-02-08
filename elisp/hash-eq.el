;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun hash-conses (n)
  (let ((table (make-hash-table :test 'eq)))
    (dotimes (i n)
      (let ((key (cons i i)))
	(setf (gethash key table) i)))
    (maphash (lambda (key value)
	       (cl-incf (gethash key table) value))
	     table)))

(defun main ()
  (dotimes (_ 150)
    (hash-conses 100000))
  (kill-emacs))

;; EOF
