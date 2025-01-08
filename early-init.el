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

;;; Initialize package
(require 'package)
;; Add melpa repository
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

;; (setq use-package-verbose t)
(require 'use-package-ensure)
;; no need to use `:ensure'
(setq use-package-always-ensure t)

;; setup no littering
(use-package no-littering
  :config
  (require 'no-littering)
  (when (and (fboundp 'startup-redirect-eln-cache)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

;;; early-init.el -- Ends here
