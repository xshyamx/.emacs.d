;;; project-notes.el --  -*- lexical-binding: t -*-

;; Author: shyam
;; Keywords: convenience

;;; Commentary

;; Helpers for project notes

;;; Code

(defvar project-meeetings-headline "Meetings"
	"Headline for the meetings top-level heading")

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
								`(seq bol "* " ,project-meeetings-headline eow) t)
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
	(add-to-list 'org-capture-templates
							 `(,key ,(format "%s: Meeting Notes" name) plain
								 (file+function ,file add-project-meeting-note)
								 "*** %?
**** Meeting Details                                                 :noexport:" :jump-to-captured t)))

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

(provide 'project-notes)
;;; project-notes.el -- Ends here
