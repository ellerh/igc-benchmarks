;; -*- lexical-binding:t -*-
;;; PRIMES -- Compute primes less than 100, written by Eric Mohr.

(require 'cl-lib)

(defun interval-list (m n) (cl-loop for i from m upto n collect i))

(defun remove-multiples (n l)
  (cl-loop for e in l
	   unless (= (mod e n) 0)
	   collect e))

(defun sieve (l)
  (named-let loop ((l l) (result '()))
    (cond ((null l) (reverse result))
	  (t (loop (remove-multiples (car l) (cdr l))
		   (cons (car l) result))))))

(defun primes<= (n)
  (sieve (interval-list 2 n)))

(defun test ()
  (cl-assert (equal (primes<= 100)
		    '(2 3 5 7 11 13 17 19 23 29 31 37 41
			43 47 53 59 61 67 71 73 79 83 89 97))))

(defun main ()
  (primes<= 100000)
  (kill-emacs))
