;;; region-extras.el --- Convenience commands on regions  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Convenience commands on regions

;;; Code:

(defun quote-string (s)
	"Quote string by escaping `\"' with `\\\"'"
	(concat
	 "\""
	 (string-replace "\"" "\\\"" s)
	 "\""))

(defun quote-lines-in-region (start end)
	"Quote all lines in the region"
	(interactive "r")
	(when (use-region-p)
		(let ((s (buffer-substring-no-properties start end)))
			(delete-region start end)
			(insert (mapconcat
							 #'quote-string
							 (seq-filter
								(lambda (s) (> (length s) 0))
								(split-string s "\n"))
							 "\n"))
			(when (string-suffix-p "\n" s)
				(insert "\n")))))

(defun join-lines-in-region (prefix start end)
	"Join all lines in region with `,' to override use `C-u' prefix"
	(interactive "p\nr")
	(when (use-region-p)
		(let ((separator ",")
					(s (buffer-substring-no-properties start end)))
			(when (> prefix 1)
				(setq separator (read-string "Separator: " nil nil separator)))
			(delete-region start end)
			(insert (string-join
							 (seq-filter
								(lambda (s) (> (length s) 0))
								(split-string s "\n"))
							 separator))
			(when (string-suffix-p "\n" s)
				(insert "\n")))))

(keymap-global-set "C-c j" #'join-lines-in-region)
(keymap-global-set "C-c \"" #'quote-lines-in-region)

(provide 'region-extras)
;;; region-extras.el
