;;; project-notes.el --  -*- lexical-binding: t -*-

;; Author: shyam
;; Keywords: convenience

;;; Commentary

;; Helpers for project notes

;;; Code

(defvar project-meetings-headline "Meetings"
  "Headline for the meetings top-level heading")

(defvar project-notes-file nil
  "File to cache the contents of `project-notes'")

(defun project-info (file)
  (let ((fn (expand-file-name file projects-home))
	(m))
    (push (file-name-base file) m)
    (push :project m)
    (push file m)
    (push :file m)
    (when (string-match (rx (or bow "/") (group (repeat 4 digit)) "--" (+ (not "/")) "/") file)
      (push (string-to-number (match-string 1 file)) m)
      (push :year m))
    ;; insert first 1k of the file and extract title & year elements
    (with-temp-buffer
      (insert-file-contents fn nil 0 1024)
      (goto-char 0)
      (when (re-search-forward (rx bol "#+title:" (+ space) (group (+ any)) eol) nil t)
	(push (match-string-no-properties 1) m)
	(push :title m))
      (goto-char 0)
      (unless (plist-member m :year)
	(when (re-search-forward (rx bol "#+year:" (+ space) (group (+ any)) eol) nil t)
	  (push (string-to-number (match-string-no-properties 1)) m)
	  (push :year m))))
    m))

(defun add-project-meeting-note ()
  "Find heading to add today's meetings"
  (let ((ts (format-time-string "[%Y-%m-%d %a]"))
	(target))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward
	       (rx-to-string
		`(seq bol "* " ,project-meetings-headline eow) t)
	       nil t)
	  (org-narrow-to-subtree)
	  (goto-char (point-min))
	  (unless (re-search-forward
		   (rx-to-string `(seq bol "** " ,ts) t) nil t)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert "** " ts "\n")))
	(goto-char (point-max))
	(unless (bolp) (insert "\n"))
	(setq target (point))
	))
    (goto-char target)))

(defun add-project-meeting-note-capture (key name file)
	"Add a capture template for project meetings in the project notes"
	(let ((template (concat
									 "*** TODO %?\n"
									 "DEADLINE: %^{Meeting Time}T\n"
									 "**** Meeting Details" (format "%56s" ":noexport:")
									 "\n")))
		(add-to-list 'org-capture-templates
								 `(,key ,(format "%s: Meeting Notes" name) plain
												(file+function ,file add-project-meeting-note)
												,template
												:jump-to-captured t))))

(defun setup-meeting-note-capture (key search-suffix project-shortname)
  (add-hook
   'project-notes-hook
   `(lambda ()
      (when-let
	  (current-project
	   (seq-find
	    (lambda (p) (string-suffix-p ,search-suffix (car p)))
	    project-notes nil))
	(add-project-meeting-note-capture
	 ,key
	 ,project-shortname
	 (expand-file-name (cdr current-project) projects-home))))))

(defun add-project-weekly-update-capture (key name file template)
  "Create a project specific capture template for weekly updates"
  (add-to-list 'org-capture-templates
	       `(,key ,(format "%s: Weekly Update" name) entry
		      (file+headline ,file "Weekly Updates")
		      ,template :jump-to-captured t)))

(defun setup-project-weekly-update-note-capture
    (key search-suffix project-shortname template)
  "Hook to `project-notes-hook' and add the capture weekly template"
  (add-hook
   'project-notes-hook
   `(lambda ()
      (when-let
	  (current-project
	   (seq-find
	    (lambda (p) (string-suffix-p ,search-suffix (car p)))
	    project-notes nil))
	(add-project-weekly-update-capture
	 ,key
	 ,project-shortname
	 (expand-file-name (cdr current-project) projects-home)
	 ,template)))))

(defun write-project-notes ()
  "Write `project-notes' value to `project-notes-file'"
  (with-temp-buffer
    (insert ";;; -*- coding: utf-8; mode: lisp-data -*-\n")
    (insert ";;; Automatically generated at: " (format-time-string "%FT%T%z") "\n")
    (pp project-notes (current-buffer))
    (write-region (point-min) (point-max) project-notes-file)))

(defun read-project-notes ()
  "Read value of `project-notes' from `project-notes-file'"
  (with-temp-buffer
    (insert-file-contents project-notes-file)
    (read (current-buffer))))

(defun project-notes-updated-since (days)
  "Check comment line for upated timestamp was more than number of
DAYS specified"
  (with-temp-buffer
    (insert-file-contents (locate-user-emacs-file "project-notes") nil 0 256)
    (goto-char 0)
    (forward-line)
    (when (re-search-forward "generated at: \\(.*\\)$")
      (> (time-to-number-of-days (time-since (match-string-no-properties 1))) days))))

(defun project-notes-home-relative-path (file)
  "Get the relative path to the home directory from FILE"
  (let ((rp (file-relative-name file (getenv "HOME"))))
    (mapconcat
     (lambda (s) "..")
     (butlast (file-name-split rp))
     "/")))

(defun project-notes-clean-relative-path (path)
  "Remove any `..' in the PATH"
  (string-join
   (seq-filter
    (lambda (s) (not (string= s "..")))
    (file-name-split path))
   "/"))

(defun project-notes-fix-paths ()
  "Fix the paths in `#+html_head' to be relative"
  (interactive)
  (when (buffer-file-name)
    (let ((prefix (project-notes-home-relative-path (buffer-file-name))))
      (save-excursion
	(goto-char 0)
	(while (re-search-forward
		(rx bol "#+html_head:"
		    (group (*? any))
		    (group bow (or "href" "src") "=")
		    "\"" (group (+ (not "\""))) "\""
		    (group (* any))
		    eol)
		nil t)
	  (replace-match
	   (concat prefix "/" (project-notes-clean-relative-path (match-string-no-properties 3)))
	   t t nil 3))))))
(provide 'project-notes)
;;; project-notes.el -- Ends here
