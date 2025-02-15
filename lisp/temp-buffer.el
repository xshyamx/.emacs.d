;;; temp-buffer.el --  -*- lexical-binding: t -*-

;; Open temp buffer
(defun temp-buffer ()
  "Switch to `*temp*' buffer.
If buffer does not exist, create it first."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*temp*")))

(keymap-global-set "C-x t" #'temp-buffer)

(provide 'temp-buffer)
;;; temp-buffer.el -- Ends here
