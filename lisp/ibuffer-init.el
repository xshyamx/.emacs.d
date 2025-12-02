;;; ibuffer-init.el --- ibuffer customizations  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; ibuffer customizations

;;; Code:

(require 'ibuf-ext)

(defvar ibuffer-ignore-special-buffers-alist
  '("*scratch*" "*Messages*" "*temp*")
  "List of special buffers to skip marking")

(defun ibuffer-mark-special-buffers-skip ()
  "Mark all special buffers except those in the
`ibuffer-ignore-special-buffers-alist'"
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (and (string-match (rx bol "*" (* any) "*" eol)
			       (buffer-name buf))
		 (not (member
		       (buffer-name buf)
		       ibuffer-ignore-special-buffers-alist))))))

(defun ibuffer-mark-read-only-buffers-skip ()
  "Mark all read-only buffers but not the `*Messages*' buffer."
  (ibuffer-mark-on-buffer
   (lambda (buf) (and (buffer-local-value 'buffer-read-only buf)
		 (not (string= "*Messages*" (buffer-name buf)))))))

(advice-add
 #'ibuffer-mark-special-buffers
 :override #'ibuffer-mark-special-buffers-skip)

(advice-add
 #'ibuffer-mark-read-only-buffers
 :override #'ibuffer-mark-read-only-buffers-skip)

(provide 'ibuffer-init)
;;; ibuffer-init.el
