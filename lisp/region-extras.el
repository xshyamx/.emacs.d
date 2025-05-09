;;; region-extras.el --- Convenience commands on regions  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Convenience commands on regions

;;; Code:

(defun quote-string (s quote-char)
	"Quote string by escaping `\"' with `\\\"'"
	(concat
	 quote-char
	 (string-replace quote-char (concat "\\" quote-char) s)
	 quote-char))

(defun quote-lines-in-region (prefix start end)
	"Quote all lines in the region. When invoked with single universal
argument (\\`C-u' \\[quote-lines-in-region]) allows overriding default
quote character `\"' which is useful for single quoting strings. With
double universal argument (\\`C-u' \\`C-u' \\[quote-lines-in-region]) allows
overriding the default separator char `\\n'. For eg. Selecting the
following region

a,b,c,d

And invoking \\`C-u' \\`C-u' \\[quote-lines-in-region] giving separator as
`,' and quote character as `\"' the region is transformed to

\"a\",\"b\",\"c\",\"d\"
 "
	(interactive "p\nr")
	(when (use-region-p)
		(let ((quote-char "\"")
					(separator "\n")
					(s (buffer-substring-no-properties start end)))
			(when (> prefix 4)
				(setq separator (read-string "Separator: " separator nil separator)))
			(when(> prefix 1)
				(setq quote-char (read-string "Quote with: " quote-char nil quote-char)))
			(atomic-change-group
				(delete-region start end)
				(insert (mapconcat
								 (lambda (s) (quote-string s quote-char))
								 (seq-filter
									(lambda (s) (> (length s) 0))
									(split-string s separator))
								 separator))
				(when (string-suffix-p separator s)
					(insert separator))))))

(defun join-lines-in-region (prefix start end)
	"Join all lines in region with `,' to override use \\`C-u' prefix"
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

(defun genereate-sequence--placeholder-count (template)
	"Returns count of the number of integer placeholders in TEMPLATE looking
for `%d's"
	(with-temp-buffer
		(insert template)
		(goto-char 0)
		(count-matches "%[0-9]*d")))

(defun generate-sequence--insert (count template indent)
	(let ((pn (genereate-sequence--placeholder-count template)))
		(push-mark)
		(dolist (i (number-sequence 1 count))
			(when (> i 1)
				(insert "\n" indent))
			(insert (apply #'format `(,template ,@(make-list pn i)))))))

(defvar generate-sequence-template-default
	(let ((component-type "component"))
		(format "%s %s%%d as \"%s-%%02d\""
						component-type
						(substring component-type 0 1)
						component-type))
	"Default template to use with `generate-sequence'")

(defun generate-sequence (prefix)
	"Insert a sequence of items based on a template prompted from the user
where `%d' will be populated with the index. The PREFIX is used to
determine the count of items

So, for eg.
Running:
\\`C-u 3' \\[generate-sequence]
or equivalent
\\`C-u 3' \\`M-x generate-sequence'

and use the template \"component c%d as \\\"component-%02d\\\"\"
template should generate the following 3 items

component c1 as \"component-01\"
component c2 as \"component-02\"
component c3 as \"component-03\"
"
	(interactive "p")
	(let* ((component-type "component")
				 (template
					(read-string
					 "Template: " generate-sequence-template-default))
				 (ci (if (eq (point) (line-beginning-position)) 0
							 (current-indentation)))
				 (indent (make-string ci ? )))
		(generate-sequence--insert prefix template indent)))

(keymap-global-set "C-c j" #'join-lines-in-region)
(keymap-global-set "C-c \"" #'quote-lines-in-region)
(keymap-global-set "C-c n" #'generate-sequence)

(provide 'region-extras)
;;; region-extras.el
