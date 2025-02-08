;;; -*- lexical-binding: t -*-

(defun compile-comp ()
  (let* ((filename (expand-file-name "lisp/emacs-lisp/comp.el"
				     source-directory))
	 (tmpfile (make-temp-file "compile"))
	 (byte-compile-dest-file-function (lambda (_) tmpfile)))
    (unwind-protect (byte-compile-file filename)
      (delete-file tmpfile))))

(defun main ()
  (dotimes (_ 4)
    (compile-comp))
  (kill-emacs))
