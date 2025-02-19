;; -*- lexical-binding: t -*-

(defun repeat-with-timer (repeat fun finally)
  (cond ((> repeat 0)
	 (run-with-idle-timer 0.01 nil
			      (lambda ()
				(funcall fun)
				(repeat-with-timer (1- repeat) fun finally))))
	(t
	 (funcall finally))))

(defun enqueue-unread-events (buffer)
  (pop-to-buffer buffer)
  (delete-other-windows)
  (setq unread-command-events
	(append (make-list 20 ?\C-v)
		(apply #'append (make-list 5 '(? ?v) )))))

(defun start (n)
  (cond ((= n 0) (kill-emacs))
	(t
	 (find-file (expand-file-name "src/xdisp.c" source-directory))
	 (let ((buffer (current-buffer)))
	   (repeat-with-timer 100
			      (lambda () (enqueue-unread-events buffer))
			      (lambda ()
				(kill-buffer buffer)
				(start (1- n))))))))

(defun main ()
  (start 3))

