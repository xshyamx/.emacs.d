;;; daily-notes.el --- Load daily notes    -*- lexical-binding: t; -*-

;; Author: shyam

;;; Commentary:

;; Load daily notes from `daily-home' directory

;;; Code:

(defvar daily-notes nil
  "Variable to hold daily notes as a list of pairs of the form
(`year' . `path-relative-to-daily-home')")

(defvar daily-notes-hook nil
  "Hook for running things after `daily-notes' has been loaded")

(defconst daily-notes--regexp
  (rx "daily-log-" (repeat 4 digit) ".org" eol)
  "Regexp to match daily notes org files")

(defun daily-notes--make-pair (path)
  "Create a pair with the year & filename"
  (cons (replace-regexp-in-string
	 "daily-log-" "" (file-name-base path))
	path))

(defun load-daily-notes ()
  "Load daily-log-<year>.org files from `daily-home' and build list for
`daily-notes'"
  (when (and daily-home (file-exists-p daily-home))
    (setq daily-notes
	  (mapcar
	   #'daily-notes--make-pair
  	   (directory-files daily-home nil
			    daily-notes--regexp)))
    (message "Loaded %d daily notes" (length daily-notes))
    (run-hooks 'daily-notes-hook)))

(defun daily-notes-find-file ()
  "Select a year from `daily-notes' and open it"
  (interactive "i")
  (when-let ((year
	      (assoc
	       (completing-read
		"Select year: "
		(seq-map #'car daily-notes)
		nil t)
	       daily-notes #'string=)))
    (find-file (expand-file-name (cdr year) daily-home))))

(provide 'daily-notes)
;;; daily-notes.el
