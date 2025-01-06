;;; init.el --  -*- lexical-binding: t -*-

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
(require 'use-package-ensure)
;; no need to use `:ensure'
(setq use-package-always-ensure t)

;;; have this as early as possible
(setq vc-follow-symlinks t)

;; Move customizations to separate file
;; mainly `package-selected-packages' & `fixed-pitch' font
(setq custom-file (locate-user-emacs-file "customizations.el"))
(when (file-exists-p custom-file)
  (load custom-file t))
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;;; load literate configuration
(let ((literate-config (locate-user-emacs-file "emacs.org")))
	(org-babel-load-file literate-config))

(require 'post-init-local nil t)

(provide 'init)
;;; init.el -- Ends here
