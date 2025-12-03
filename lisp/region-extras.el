;;; region-extras.el --- Convenience commands on regions  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Convenience commands on regions

;;; Code:

(defconst region-extras-join-separator-default " "
  "Default separator used for joining strings with `join-lines-in-region'")

(defconst region-extras-join-separator-alt ","
  "Alternate separator used for joining strings with `join-lines-in-region'")

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
  "Join all lines in region with `region-extras-join-separator-default' to
override use \\`C-u' prefix"
  (interactive "p\nr")
  (when (use-region-p)
    (let ((separator region-extras-join-separator-default)
	  (s (buffer-substring-no-properties start end)))
      (when (> prefix 1)
	(setq separator
	      (read-string "Separator: "
			   region-extras-join-separator-alt
			   nil separator)))
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

(defun camel-case (input &optional upper)
  "Convert INPUT to camel case string. If UPPER is passed then the
first word is capitalized"
  (save-match-data
    (let ((parts (split-string input "[^[:word:]]+")))
      (apply #'concat
	     (cons (funcall (if upper #'capitalize #'downcase) (car parts))
		   (mapcar 'capitalize (cdr parts)))))))

(defun camel-case-region (begin end prefix)
  "Convert selected region to camel case"
  (interactive "r\np")
  (when (use-region-p)
    (let ((repl
	   (camel-case
	    (buffer-substring-no-properties begin end) (> prefix 1))))
      (delete-region begin end)
      (push-mark)
      (insert repl))))

(defun kebab-case (s)
  "Convert to kebab case by splitting on spaces, underscores &
hyphens"
  (save-match-data
    (string-trim
     (string-join (split-string
		   (downcase (string-replace "'" "" s)) "[^[:word:]]+")
		  "-")
     "-" "-")))

(defun kebab-case-region (begin end)
  "Convert selected region to kebab case"
  (interactive "r")
  (when (use-region-p)
    (let ((repl (kebab-case
		 (buffer-substring-no-properties begin end))))
      (delete-region begin end)
      (push-mark)
      (insert repl))))

(defun snake-case (s)
  "Convert to snake case by splitting on non-word characters"
  (save-match-data
    (string-trim
     (string-join (split-string
		   (string-replace "'" "" s) "[^[:word:]]+")
		  "_")
     "_" "_")))

(defun snake-case-region (begin end prefix)
  "Convert selected region to snake case"
  (interactive "r\np")
  (when (use-region-p)
    (let ((repl (snake-case
		 (buffer-substring-no-properties begin end))))
      (delete-region begin end)
      (push-mark)
      (insert (funcall (if (> prefix 1) #'upcase #'identity) repl)))))

(keymap-global-set "C-c j" #'join-lines-in-region)
(keymap-global-set "C-c \"" #'quote-lines-in-region)
(keymap-global-set "C-c n" #'generate-sequence)

(provide 'region-extras)
;;; region-extras.el
