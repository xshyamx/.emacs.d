;;; Initialize package
(require 'package)
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
;;; load literate configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(visual-basic-mode ox-reveal lsp-metals company company-mode rust-mode svelte-mode rjsx-mode scala-mode org org-drill adoc-mode mermaid-mode fzf zenburn-theme typescript-mode powershell dired-subtree projectile edit-indirect php-mode plantuml-mode nord-theme htmlize wsd-mode cql-mode kotlin-mode csv-mode graphviz-dot-mode yasnippet yaml-mode which-key web-mode vlf use-package try terraform-mode markdown-mode json-mode groovy-mode go-mode emmet-mode dockerfile-mode cypher-mode command-log-mode auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code")))))

