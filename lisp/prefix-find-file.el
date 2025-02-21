;;; prefix-find-file.el --- Prefix find file            -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary: convenience

;; Override find-file keybinding to look at specified directories
;; based on the number of universal arguments

;;; Code:

(defcustom prefix-directories nil
  "List of directories to search for files based on the number of
universal arguments passed to the `prefix-find-file' command"
  :type '(choice
		  (repeat :tag "List of directories" directory)
		  (directory :tag "Search directory\n" :value "~/")))

(defun prefix--lookup-alist ()
  "Construct prefix lookup list of the form (prefix . directory)
from `prefix-directories'"
	(let ((ds))
		(seq-do-indexed
		 (lambda (d i) (push (cons (expt 2 (+ 2 (* 2 i))) d) ds))
		 prefix-directories)
		ds))

(defun prefix-find-file (prefix)
  "Replacement for `find-file' keybinding which can switch to
opening files from a predefined list of directories from
`prefix-directories' using the universal argument.
Eg.
C-x C-f          Opens the find file in default directory

C-u C-x C-f      Opens the find file in the first directory from `prefix-directories' etc.
C-u C-u C-x C-f  Opens the find file in the second directory from `prefix-directories' etc."
  (interactive "p")
  (let ((dir (assoc-default prefix (prefix--lookup-alist))))
    (if dir
		(let ((default-directory dir))
		  (call-interactively #'find-file))
      (call-interactively #'find-file))))

(keymap-global-set "C-x C-f" #'prefix-find-file)

(defun prefix-write-file (prefix)
	"Replacement for `write-file' keybinding which can switch to
different directories based on the universal prefix
arguments (See `prefix-find-file'). The directories are defined
in `prefix-directories'"
	(interactive "p")
	(let ((dir (assoc-default prefix (prefix--lookup-alist))))
		(if dir
				(let ((default-directory dir))
					(call-interactively #'write-file))
			(call-interactively #'write-file))))

(keymap-global-set "C-x C-w" #'prefix-write-file)

(provide 'prefix-find-file)
;;; prefix-find-file.el
