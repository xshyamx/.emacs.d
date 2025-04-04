;;; org-insert-image.el --  -*- lexical-binding: t -*-
;; Keywords: lisp, org, convenience

;;; Commentary:

;; Provide command to insert image with caption in org file
;; and open new buffer for the image source

;;; Code:

(defconst org-insert-preview-extension ".png"
	"Default image preview extension")

(defconst org-insert-plantuml-file-extension ".plantuml"
	"Default file extension for PlantUML files")

(defconst org-insert-graphviz-file-extension ".gv"
	"Default file extension for Graphviz dot files")

(defvar org-insert-image-dirs '("./img/screenshots/" "./img/")
	"List of directories to search for images")

(defun org-insert-image--target-dir (fn projects-home)
	"Return the target directory for the source image file"
	(let* ((bn (file-name-base fn))
				 (bd (file-name-directory fn))
				 (is-daily-notes (string-prefix-p "daily-log-" bn)))
		(expand-file-name
		 (if is-daily-notes
				 (concat "org-includes/" (string-replace "daily-log-" "" bn))
			 "img")
		 bd)))

(defun org-insert-image--target-file (org-insert-image--target-dir slug &optional suffix)
	(setq suffix (or suffix org-insert-plantuml-file-extension))
	(let ((i 1)
				(tfn (expand-file-name (concat slug suffix) org-insert-image--target-dir)))
		(while (file-exists-p tfn)
			(setq tfn (expand-file-name
								 (format "%s-%02d%s" slug i suffix)
								 org-insert-image--target-dir))
			(setq i (1+ i)))
		tfn))

(defun org-insert-image--execute (slug ext &optional prefix)
	"Insert an image with the preview extension in the org file and
open a new buffer to SLUG.EXT"
	(let* ((itd (org-insert-image--target-dir (buffer-file-name) projects-home))
				 (itf (if (eql 4 prefix)
									(expand-file-name (concat slug ext) itd)
								(org-insert-image--target-file itd slug ext)))
				 (ci (make-string (current-indentation) ? ))
				 (png (concat
							 "./"
							 (file-relative-name
								(concat (file-name-sans-extension itf) org-insert-preview-extension)
								(file-name-directory (buffer-file-name)))))
				 (map (concat
							 "./"
							 (file-relative-name
								(concat (file-name-sans-extension itf) ".cmapx")
								(file-name-directory (buffer-file-name))))))
		(insert
		 (format "#+caption: %s\n%s[[%s]]\n"
						 slug
						 (if (file-exists-p map)
								 (format
									"%s#+attr_html: :usemap #%s_map\n%s"
									ci slug ci)
							 ci)
						 png))
		(when (file-exists-p map)
			(insert (format
							 "%s#+include: %s export html\n" ci map)))
		(find-file itf)))

(defun org-extract-slug-prefix ()
	(let ((el (org-element-context (org-element-at-point))))
    (or (org-entry-get el "slug-prefix")
        (org-entry-get el "slug-prefix" t))))

(defun org-insert-image (prefix slug)
	"Insert image"
	(interactive
	 (list
		(prefix-numeric-value current-prefix-arg)
		(read-string
		 "Image slug: "
		 (if-let (p (org-extract-slug-prefix))
				 (concat p "--")
			 ""))))
	(org-insert-image--execute
	 slug
	 org-insert-plantuml-file-extension
	 prefix))

(defun org-insert--initial-directory ()
	(let ((image-dirs org-insert-image-dirs))
		(when-let (file buffer-file-name)
			(setq slug (file-name-base file))
			(when (string-match
						 (rx bol "daily-log-" (group (repeat 4 digit)) eol)
						 slug)

				(add-to-list 'image-dirs
										 (concat "./org-includes/" (match-string 1 slug) "/"))))
		(seq-find
		 #'file-exists-p
		 (mapcar #'expand-file-name image-dirs))))

(defun org-insert-existing-image (prefix)
	(interactive "p")
	(when-let ((file (read-file-name
										"Insert image: "
										(org-insert--initial-directory)
										nil t nil
										(lambda (f) (or
														(string-suffix-p "/" f)
														(string-suffix-p ".png" f)))))
						 (ci (make-string (current-indentation) ? ))
						 (basedir (file-name-directory (buffer-file-name))))
		(insert "#+caption: "
						(or (extract-plantuml-title file)
								(file-name-base file))
						"\n")
		(insert ci
						"[[./" (file-relative-name file basedir) "]]\n")))

(defun extract-plantuml-title (file)
	(let ((plantuml-file (concat (file-name-sans-extension file)
															 ".plantuml")))
		(when (file-exists-p plantuml-file)
			(with-temp-buffer
				(insert-file-contents plantuml-file)
				(goto-char 0)
				(when (re-search-forward
							 (rx bol "title" (+ space) (group (* any)) eol)
							 nil t)
					(match-string-no-properties 1))))))

(defun org-update-plantuml-title ()
	(interactive)
	(let ((el (org-element-context (org-element-at-point))))
		(when (and (eq 'link (org-element-type el))
							 (string= "file" (org-element-property :type el)))
			(when-let (title (extract-plantuml-title
												(expand-file-name
												 (org-element-property :path el))))
				(when (re-search-backward
							 (rx bol "#+caption:" (+ space) (group (* any)) eol)
							 (save-excursion (forward-line -2) (point))
							 nil)
					(replace-match (concat "#+caption: " title) t t))))))


(keymap-set org-mode-map "C-c i i" #'org-insert-image)
(keymap-set org-mode-map "C-c i e" #'org-insert-existing-image)
(keymap-set org-mode-map "C-c u" #'org-update-plantuml-title)

(provide 'org-insert-image)
;;; org-insert-image.el -- Ends here
