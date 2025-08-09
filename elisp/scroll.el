;; -*- lexical-binding: t -*-

(defun enqueue-unread-events (buffer)
  (pop-to-buffer buffer)
  (delete-other-windows)
  (setq unread-command-events
	(append (make-list 20 ?\C-v)
		(apply #'append (make-list 5 '(? ?v) )))))

(defun scroll-timer-function (state buffer done)
  (let ((timer (car state))
	(n (cdr state)))
    (cond ((and (= n 0) (not unread-command-events))
	   (cancel-timer timer)
	   (kill-buffer buffer)
	   (funcall done))
	  ((not unread-command-events)
	   (enqueue-unread-events buffer)
	   (setcdr state (1- n))))))

(defun start (n done)
  (find-file (expand-file-name "src/xdisp.c" source-directory))
  (let* ((buffer (current-buffer))
	 (state (cons nil n))
	 (timer (run-with-idle-timer 0.1 t #'scroll-timer-function state buffer
				     done)))
    (setcar state timer)))


(defun main2 ()
  (start 80 (lambda () (stp-end) (kill-emacs))))

(defun main ()
  (start 80 #'kill-emacs))
