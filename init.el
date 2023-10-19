;;; init.el --  -*- lexical-binding: t -*-

(defun set-emacs-gc-threshold (value)
	(message "Setting gc-cons-threshold to %d" value)
	(setq original-gc-cons-threshold gc-cons-threshold)
	(setq gc-cons-threshold value))
;; increase gc-cons-threshold
(message "Increase gc-cons-threshold")
(set-emacs-gc-threshold (* 50 (* 1024 1024))) ; 50MB

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
  (package-refresh-contents)
  (package-install 'use-package))

;; (setq use-package-verbose t)

;;; have this as early as possible
(setq vc-follow-symlinks t)

;; Move customizations to separate file
;; mainly `package-selected-packages' & `fixed-pitch' font
(setq custom-file (locate-user-emacs-file "customizations.el"))
(load-file custom-file)

;;; load literate configuration
(org-babel-load-file (locate-user-emacs-file "emacs.org"))


(provide 'init)
;;; init.el -- Ends here
