;;; -*- lexical-binding: t -*-

;; Naive port of fprint/pretty from the Garbiel benchmarks.

(require 'cl-lib)

(defvar *fprint-test-atoms*
  '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67
    mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12
    wxyzab23 xyzabc34 123456ab 234567bc 345678cd
    456789de 567890ef 678901fg 789012gh 890123hi))

(defun fprint-init-aux (m n atoms)
  (cond ((zerop m) (pop atoms))
	(t (cl-do ((i n (- i 2))
		   (a ()))
	       ((< i 1) a)
	     (push (pop atoms) a)
	     (push (fprint-init-aux (1- m) n atoms) a)))))

(defun fprint-init (m n atoms)
  (let ((atoms (cl-subst () () atoms)))
    (cl-do ((a atoms (cdr a)))
	((null (cdr a)) (rplacd a atoms)))
    (fprint-init-aux m n atoms)))

(defun fprint/pretty (pattern file)
  (with-temp-file file
    (let ((print-circle t)
	  (print-level 100))
      (print pattern (current-buffer)))))

(defun main ()
  (let ((pattern (fprint-init 10 8 *fprint-test-atoms*)))
    (dotimes (_ 5)
      (let ((file (make-temp-file "fprint.")))
	(unwind-protect
	    (fprint/pretty pattern file)
	  (delete-file file)))))
  (kill-emacs))
