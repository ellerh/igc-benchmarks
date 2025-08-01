;; -*- lexical-binding:t -*-
;;; STRING -- One of the Kernighan and Van Wyk benchmarks.

;; This tests string-append and substring, and very little else.

(require 'cl-lib)

(defvar *s* "abcdef")

(defun grow ()
  (setq *s* (concat "123" *s* "456" *s* "789"))
  (setq *s* (concat (substring *s* (/ (length *s*) 2) (length *s*))
		    (substring *s* 0 (+ 1 (/ (length *s*) 2)))))
  *s*)

(defun trial (n)
  (cl-do ((i 0 (+ i 1)))
      ((> (length *s*) n) (length *s*))
    (grow)))

(defun my-try (n)
  (cl-do ((i 0 (+ i 1)))
      ((>= i 10) (length *s*))
    (setq *s* "abcdef")
    (trial n)))

(defun test () (cl-assert (equal (my-try 500000) 524278)))

(defun main ()
  (my-try 10000000)
  (kill-emacs 0))
