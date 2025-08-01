;; -*- lexical-binding:t -*-

;; The nth taxicab number, typically denoted Ta(n) or Taxicab(n), is
;; defined as the smallest integer that can be expressed as a sum of
;; two positive integer cubes in n distinct ways. The most famous
;; taxicab number is 1729 = Ta(2) = 1³ + 12³ = 9³ + 10³, also known as
;; the Hardy–Ramanujan number.
;;
;; This benchmark generates a table of sums of pairs

(require 'cl-lib)

(defun make-pair-iter () (cons 1 1))

(defun pair-iter-next (state)
  (let ((a (car state))
	(b (cdr state)))
    (cond ((= a b)
	   (setcar state 1)
	   (setcdr state (1+ b))
	   state)
	  (t
	   (setcar state (1+ a))
	   state))))

(defun sum-of-cubes (a b) (+ (* a a a) (* b b b)))

(defun taxicab (n)
  (let* ((table (make-hash-table))
	 (candidate nil))
    (named-let loop ((pair (make-pair-iter)))
      (let* ((a (car pair))
	     (b (cdr pair))
	     (sum (sum-of-cubes a b))
	     (list (cons (cons a b) (gethash sum table '())))
	     ;;(m (1+ (gethash sum table 0)))
	     )
	(when (= (length list) n)
	  (when (or (not candidate) (< sum candidate))
	    (setq candidate sum)))
	(cond ((and (= a 1) candidate)
	       (cons candidate (gethash candidate table)))
	      (t
	       (when (= a 1)
		 (maphash (lambda (sum2 _)
			    (when (< sum2 sum)
			      (remhash sum2 table)))
			  table))
	       (puthash sum list table)
	       (loop (pair-iter-next pair))))))))

(defun test ()
  (cl-assert (equal (taxicab 2)
		    '(1729 (9 . 10)))))

(defun main ()
  (dotimes (_ 10)
    (taxicab 3))
  (kill-emacs))
