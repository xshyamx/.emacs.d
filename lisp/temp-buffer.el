;;; temp-buffer.el --  -*- lexical-binding: t -*-
;;;

;;; Commentary:

;;; Create temporary buffers

;;; Code:



(defvar temp-buffer-mode-alist
  '(("json" .  json-mode)
    ("shell" . sh-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("restclient" . restclient-mode)
    ("html" . html-mode)
    ("org" . org-mode)
    ("xml" . xml-mode)
    ("sql" . sql-mode)
    ("csv" . csv-mode))
  "Alist of modes to start temporary buffers")

;; Open temp buffer
(defun temp-buffer (prefix)
  "Switch to `*temp*' buffer. Use with prefix `C-u' to switch to a
temporary buffer with the selected mode. `temp-buffer-mode-list'
contains the list of modes which can be selected.

If buffer does not exist, create it first."
  (interactive "p")
  (let ((mode (if (> prefix 1)
		  (completing-read
		   "Select mode: " temp-buffer-mode-alist nil t)
		"temp"))
	(bufname) (buffer))
    (setq bufname (format "*%s*" mode))
    (when (>= prefix 16)
      (setq bufname (generate-new-buffer-name bufname)))
    (setq buffer (get-buffer-create bufname))
    (unless (string= "temp" mode)
      (with-current-buffer buffer
	(funcall (cdr (assoc-string mode temp-buffer-mode-alist)))))
    (switch-to-buffer buffer)))

(keymap-global-set "C-x t" #'temp-buffer)

(provide 'temp-buffer)
;;; temp-buffer.el -- Ends here
