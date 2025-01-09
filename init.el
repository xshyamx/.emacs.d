;;; init.el --  -*- lexical-binding: t -*-

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

;; Move customizations to separate file
;; mainly `package-selected-packages' & `fixed-pitch' font
(setq custom-file (no-littering-expand-var-file-name "customizations.el"))
(if (file-exists-p custom-file)
    (load custom-file t)
  (with-temp-buffer
    (let ((prefix ";;; customizations.el ---")
	  (suffix "-*- lexical-binding: t ; -*-"))
      (insert (format "%s %s %s"
		      prefix
		      (make-string
		       (- 80 (+ 2 (length prefix) (length suffix))) ? )
		      suffix)))
    (write-region (point-min) (point-max) custom-file)))

;; add to load-path
;; lisp       - Functional modules
;; site-lisp  - Packages with customizations / in development
;; site-local - Files relevant for current host/site
(let ((paths '("lisp" "site-lisp" "site-local")))
	(dolist (path (mapcar #'locate-user-emacs-file paths))
		(when (file-exists-p path)
			(add-to-list 'load-path path))))


;;; load literate configuration
(let ((literate-config (locate-user-emacs-file "emacs.org")))
	(org-babel-load-file literate-config))

(require 'post-init-local nil t)

(provide 'init)
;;; init.el -- Ends here
