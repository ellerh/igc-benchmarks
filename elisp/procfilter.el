;; -*- lexical-binding: t -*-

;; This program starts a sub-process that sends a large number of
;; zeros to its stdout.  Then a process filter splits the sub-process
;; output into "packets".  The program exits when it has received a
;; certain number of packets.

(require 'cl-lib)

(defun make-byte-buffer ()
  (with-current-buffer (generate-new-buffer " *test*")
    (set-buffer-multibyte nil)
    (buffer-disable-undo (current-buffer))
    (current-buffer)))

(defun packet-size () (* 64 1024))

(defun writer-process-command (packet-size count)
  (list "dd" "if=/dev/zero" "status=none"
	(format "bs=%d" packet-size)
	(format "count=%d" count)))

(defvar nbytes-recevied 0)

(defun receive-input (proc bytes counter)
  (with-current-buffer (process-buffer proc)
    (goto-char (1+ (buffer-size)))
    (insert bytes)
    (while (>= (buffer-size) (packet-size))
      (let ((pkt (buffer-substring-no-properties 1 (+ 1 (packet-size)))))
	(cl-incf nbytes-recevied (length pkt))
	(cl-incf (aref counter 0)))
      (delete-region 1 (+ 1 (packet-size))))
    (cl-assert (< (buffer-size) (packet-size)))))
  
(defun receive-packets (count)
  (let* ((buffer (make-byte-buffer))
	 (counter (vector 0))
	 (proc (make-process
		:name "test"
		:noquery t
		:command (writer-process-command (packet-size) count)
		:connection-type 'pipe
		:coding 'binary
		:buffer buffer
		:filter (lambda (p s) (receive-input p s counter)))))
    (unwind-protect
	(while (< (aref counter 0) count)
	  (accept-process-output proc))
      (when (process-live-p proc)
	(kill-process proc))
      (kill-buffer buffer))))

(defun main ()
  (receive-packets 10000)
  (kill-emacs))
