;; -*- lexical-binding: t -*-

(defun repeat-with-timer (repeat fun finally)
  (cond ((> repeat 0)
	 (run-with-idle-timer 0.1 nil
			      (lambda ()
				(funcall fun)
				(repeat-with-timer (1- repeat) fun finally))))
	(t
	 (funcall finally))))

(defun enqueue-unread-events ()
  (setq unread-command-events (make-list 20 ?\C-v)))

(defun main ()
  (find-file (expand-file-name "src/xdisp.c" source-directory))
  (repeat-with-timer 50 #'enqueue-unread-events #'kill-emacs))
