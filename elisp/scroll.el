;; -*- lexical-binding: t -*-

(defun repeat-with-timer (repeat fun finally)
  (cond ((> repeat 0)
	 (run-with-idle-timer 0.01 nil
			      (lambda ()
				(funcall fun)
				(repeat-with-timer (1- repeat) fun finally))))
	(t
	 (funcall finally))))

(defun enqueue-unread-events ()
  (setq unread-command-events
	(append (make-list 20 ?\C-v)
		(apply #'append (make-list 5 '(? ?v) )))))

(defun start (n)
  (cond ((= n 0) (kill-emacs))
	(t
	 (find-file (expand-file-name "src/xdisp.c" source-directory))
	 (repeat-with-timer 100 #'enqueue-unread-events
			    (lambda ()
			      (kill-buffer "xdisp.c")
			      (start (1- n)))))))

(defun main ()
  (start 3))

