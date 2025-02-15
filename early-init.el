;;; early-init.el --  -*- lexical-binding: t -*-

(defun set-emacs-gc-threshold (value)
  (message "Setting gc-cons-threshold to %d" value)
  (setq original-gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold value))
;; increase gc-cons-threshold
(message "Increase gc-cons-threshold")
(set-emacs-gc-threshold (* 50 (* 1024 1024))) ; 50MB

(let ((messages "*Messages*"))
  (switch-to-buffer messages)
  (with-current-buffer messages
    (delete-other-windows)))

;;; have this as early as possible
(setq vc-follow-symlinks t)
(let ((paths (mapcar #'locate-user-emacs-file '("site-lisp/compat" "site-lisp/no-littering"))))
  (dolist (path paths)
    (when (file-exists-p path)
      (add-to-list 'load-path path))))

;; setup no littering
(require 'no-littering)
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;;; early-init.el -- Ends here
