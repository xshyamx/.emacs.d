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
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))
