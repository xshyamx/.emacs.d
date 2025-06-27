;;; temp-buffer.el --  -*- lexical-binding: t -*-
;;;

;;; Commentary:

;;; Create temporary buffers

;;; Code:



(defvar temp-buffer-mode-list
  '(json-mode xml-mode emacs-lisp-mode restclient-mode)
  "List of modes to start temporary buffers")

;; Open temp buffer
(defun temp-buffer (prefix)
  "Switch to `*temp*' buffer. Use with prefix `C-u' to switch to a temporary buffer with the selected mode. `temp-buffer-mode-list' contains the list of modes which can be selected.

If buffer does not exist, create it first."
  (interactive "p")
  (switch-to-buffer
   (if (> prefix 1)
       (when-let ((mode (completing-read
			 "Select mode: "
			 temp-buffer-mode-list nil t)))
	 (let ((buffer (get-buffer-create
			(format "*%s*"
				(string-remove-suffix "-mode" mode)))))
	   (with-current-buffer buffer
	     (funcall (intern mode)))
	   buffer))
     (get-buffer-create "*temp*"))))

(keymap-global-set "C-x t" #'temp-buffer)

(provide 'temp-buffer)
;;; temp-buffer.el -- Ends here
