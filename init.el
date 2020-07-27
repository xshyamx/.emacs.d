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

;; have this as early as possible
(setq vc-follow-symlinks t)

(require 'org)
(add-to-list 'org-structure-template-alist
             '("x" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(org-babel-load-file (expand-file-name "~/.emacs.d/shyam-emacs.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" default)))
 '(fci-rule-color "#383838")
 '(fzf/executable "/usr/local/bin/fzf")
 '(graphviz-dot-dot-program "/usr/local/bin/dot")
 '(graphviz-dot-indent-width 2)
 '(markdown-command "~/go/bin/mdtool +h +ta +l")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/projects/inovalon/inovalon-notes.org" "~/shyam/home/payments.org" "~/shyam/macbook/today.org" "~/shyam/macbook/buf.org")))
 '(package-selected-packages
   (quote
    (fzf zenburn-theme typescript-mode powershell dired-subtree projectile edit-indirect php-mode plantuml-mode restclient nord-theme htmlize wsd-mode cql-mode kotlin-mode csv-mode graphviz-dot-mode yasnippet yaml-mode which-key web-mode vlf use-package try terraform-mode markdown-mode json-mode groovy-mode go-mode emmet-mode dockerfile-mode cypher-mode command-log-mode base16-theme auto-complete)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(plantuml-default-exec-mode (quote jar))
 '(plantuml-jar-path
   "/usr/local/Cellar/plantuml/1.2020.1_1/libexec/plantuml.jar")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
