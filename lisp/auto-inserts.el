;;; auto-inserts.el --- Auto-insert definitions  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;;

;;; Code:

;; Graphviz diagrams
(define-auto-insert
	'(graphviz-dot-mode . "Graphviz diagram")
	'("Diagram skeleton: "
		"digraph G {" \n
		"  // " (file-name-base (buffer-file-name)) \n
		> _ \n
		"}" \n))

;; PlantUML diagrams
(define-auto-insert
	'(plantuml-mode . "PlantUML diagram")
	'("Diagram skeleton: "
		"@startuml" \n
		"' " (file-name-base (buffer-file-name)) \n
		"header Draft" \n
		"title "
		(string-join
		 (mapcar #'capitalize
						 (split-string (file-name-base (buffer-file-name)) "-+"))
		 " ")
		\n
		"footer " (format-time-string "%Y-%m-%d") \n
		\n
		> _
		\n "@enduml"))

;; Org mode
(define-auto-insert
 	'(org-mode . "Org file")
	'("Org file skeleton: "
		"#+title: " (capitalize (file-name-base (buffer-file-name))) \n
		"#+author: " user-login-name " <" user-mail-address ">" \n
		"# -*- org-export-use-babel: nil; -*-" \n
		"#+year: " (format-time-string "%Y") \n
		"#+created: " (format-time-string "%Y-%m-%d") \n
		"#+options:  num:nil broken-links:mark" \n
		"#+property: header-args :eval never-export" \n
		"#+startup: overview hideblocks" \n
		"#+html_head: <link rel=\"stylesheet\" href=\""
		(file-relative-name
		 (expand-file-name "org-includes/org.css" daily-home) default-directory)
		"\"></link>" \n
		"#+html_head: <link rel=\"stylesheet\" href=\""
		(file-relative-name
		 (expand-file-name "org-includes/slideshow.css" daily-home) default-directory)
		"\"></link>" \n
		"#+html_head: <script src=\""
		(file-relative-name
		 (expand-file-name "org-includes/org.js" daily-home) default-directory)
		"\"></script>" \n
		"#+html_head: <script src=\""
		(file-relative-name
		 (expand-file-name "org-includes/slideshow.js" daily-home) default-directory)
		"\"></script>" \n
		"* " > _
		))

;; remove existing entry
(setq auto-insert-alist
			(seq-filter
			 (lambda (l) (not
							 (and
								(listp (car l))
								(stringp (cdar l))
								(string= "Emacs Lisp header" (cdar l)))))
			 auto-insert-alist))

(define-auto-insert
	'("\\.el\\'" . "Emacs Lisp header")
	'("Short description: "
		";;; " (file-name-nondirectory (buffer-file-name))	" --- " str
		(make-string	(max 2
											 (- fill-column (current-column) 27))
									32)
		"-*- lexical-binding: t; -*-"
		'(setq lexical-binding t)
		"

;; Author: " (user-login-name)
		'(end-of-line 1)
		"
;;; Commentary:

;; " _ "

;;; Code:



(provide '" (file-name-base	(buffer-file-name)) ")
;;; " (file-name-nondirectory	(buffer-file-name))) " ends here
")

(define-auto-insert
	'("\\.gitignore\\'" . ".gitignore file")
	'("Short description: "
		".*" \n
		"!.gitignore" \n
		_))

(provide 'auto-inserts)
;;; auto-inserts.el
