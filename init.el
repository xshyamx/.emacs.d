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
(setq use-package-verbose t)

;; have this as early as possible
(setq vc-follow-symlinks t)

(require 'org)
;(add-to-list 'org-structure-template-alist
;             '("x" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

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
	 '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" default))
 '(exec-path
	 '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/Users/shyam/bin" "/Users/shyam/go/bin" "/Users/shyam/Library/Python/2.7/bin"))
 '(fci-rule-color "#383838")
 '(fzf/executable "/usr/local/bin/fzf")
 '(graphviz-dot-dot-program "/usr/local/bin/dot")
 '(graphviz-dot-indent-width 2)
 '(markdown-command "~/go/bin/mdtool +h +ta +l")
 '(nrepl-message-colors
	 '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files
	 '("~/projects/inovalon/inovalon-notes.org" "~/shyam/home/payments.org" "~/shyam/macbook/today.org" "~/shyam/macbook/buf.org"))
 '(package-selected-packages
	 '(lsp-metals company company-mode rust-mode svelte-mode rjsx-mode scala-mode org org-drill adoc-mode mermaid-mode fzf zenburn-theme typescript-mode powershell dired-subtree projectile edit-indirect php-mode plantuml-mode nord-theme htmlize wsd-mode cql-mode kotlin-mode csv-mode graphviz-dot-mode yasnippet yaml-mode which-key web-mode vlf use-package try terraform-mode markdown-mode json-mode groovy-mode go-mode emmet-mode dockerfile-mode cypher-mode command-log-mode auto-complete))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(safe-local-variable-values '((org-image-actual-width)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
	 '((20 . "#BC8383")
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
		 (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
