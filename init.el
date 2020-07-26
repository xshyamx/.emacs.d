;; Initialize package
(require 'package)
(add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org)
(add-to-list 'org-structure-template-alist
             '("x" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(org-babel-load-file (expand-file-name "~/.emacs.d/shyam-emacs.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" default)))
 '(fzf/executable "/usr/local/bin/fzf")
 '(graphviz-dot-dot-program "/usr/local/bin/dot")
 '(graphviz-dot-indent-width 2)
 '(markdown-command "~/go/bin/mdtool +h +ta +l")
 '(package-selected-packages
   (quote
    (fzf zenburn-theme typescript-mode powershell dired-subtree projectile edit-indirect php-mode plantuml-mode restclient nord-theme htmlize wsd-mode cql-mode kotlin-mode csv-mode graphviz-dot-mode yasnippet yaml-mode which-key web-mode vlf use-package try terraform-mode markdown-mode json-mode groovy-mode go-mode emmet-mode dockerfile-mode cypher-mode command-log-mode base16-theme auto-complete)))
 '(plantuml-default-exec-mode (quote jar))
 '(plantuml-jar-path
   "/usr/local/Cellar/plantuml/1.2020.1_1/libexec/plantuml.jar"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
