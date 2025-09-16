;;; project-notes-mode.el --- Project notes minor mode  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;;

;;; Code:

(require 'org)

(defun project-notes--reset-day (contents date)
  (with-temp-buffer
    (insert contents)
    ;; Set the date to current date
    (goto-char 0)
    (while (re-search-forward
	    (rx bow (repeat 4 digit)
                "-" (repeat 2 digit)
                "-" (repeat 2 digit)
                (+ space)
                (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	    nil t)
      (replace-match date))
    ;; remove CLOSED tags
    (goto-char 0)
    (while (re-search-forward
	    (rx bol "CLOSED: [" (+ (not "]")) "]" (+ space))
	    nil t)
      (replace-match ""))
    ;; remove DONE
    (goto-char 0)
    (while (re-search-forward
	    (rx bol (repeat 3 "*") (+ space)
		(? (or "TODO" "DONE") eow (+ space)))
	    nil t)
      (replace-match "*** TODO "))
    ;; Remove meeting notes
    (goto-char 0)
    (while (re-search-forward (rx bol "DEADLINE" eow) nil t)
      (forward-line)
      (while (not (or (eobp)
		      (looking-at (rx bol (+ "*") (+ space)))))
        (delete-line)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun project-notes--parse-iso-date (iso-date)
  (when (string-match
	 (rx (group (repeat 4 digit))
	     "-" (group (repeat 2 digit))
	     "-" (group (repeat 2 digit)))
	 iso-date)
    (let ((year (string-to-number (match-string 1 iso-date)))
          (month (string-to-number (match-string 2 iso-date)))
          (day (string-to-number (match-string 3 iso-date))))
      (encode-time 0 0 0 day month year))))

(defun project-notes-reset-meeting-dates (date)
  "Use `org-read-date' to read the DATE from the calendar and reset the
current day entry to match the selected date and reset completed times
and meeting notes"
  (interactive
   (list (format-time-string
	  "%Y-%m-%d %a"
	  (project-notes--parse-iso-date (org-read-date)))))
  (when (> (length date) 0)
    (save-excursion
      (save-restriction
	(when-let ((level (org-outline-level))
                   (heading (org-element-at-point)))
          ;; if at the appropriate heading level
          (when (eq 2 level)
	    (org-narrow-to-subtree heading)))
	(let ((day-contents (buffer-substring-no-properties
			     (point-min) (point-max))))
          ;; replace narrowed region with processed content
          (delete-region (point-min) (point-max))
          (insert (project-notes--reset-day day-contents date)))))))

(defun project-notes-delete-meeting-details ()
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward
	      (rx bol (+ "*") " Meeting Details" eow) nil t)
        (org-cut-subtree)))))

(defun project-notes--yank-meeting-item (password)
  (let ((entry (substring-no-properties (org-get-entry)))
	(regexp (if password
		    (rx bol "Meeting password" (* (not ":"))
			":" (* space) (group (+ any)) eol)
		  (rx bol "Meeting number" (* (not ":"))
		      ":" (* space) (group (+ any)) eol))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
	(kill-new (match-string-no-properties 1))
	(message "Copied meeting %s"
		 (if password "password" "number"))))))

(defun project-notes-yank-meeting-number ()
  (interactive)
  (project-notes--yank-meeting-item nil))

(defun project-notes-yank-meeting-password ()
  (interactive)
  (project-notes--yank-meeting-item t))

(defvar project-notes--map
  (let ((keymap (make-sparse-keymap)))
    (keymap-set keymap "n" #'project-notes-yank-meeting-number)
    (keymap-set keymap "p" #'project-notes-yank-meeting-password)
    (keymap-set keymap "r" #'project-notes-reset-meeting-dates)
    (keymap-set keymap "d" #'project-notes-delete-meeting-details)
    keymap)
  "Project notes keymap without any prefix")

(defvar project-notes-mode-map
  (let ((keymap (make-sparse-keymap)))
    (keymap-set keymap "C-c m" project-notes--map)
    keymap)
  "Project notes keymap")

(define-minor-mode project-notes-mode
  "Minor mode for project notes in org mode"
  :lighter " PN")

(provide 'project-notes-mode)
;;; project-notes-mode.el
