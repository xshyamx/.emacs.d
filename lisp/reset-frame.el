;;; reset-frame.el --- Reset frame to preferred size  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  shyam

;; Author: shyam
;; Keywords: frames

;;; Commentary:

;;; Reset current frame to defaults

;;; Code:

(defvar default-frame-width 100
  "Default frame width for `reset-frame'")
(defvar default-frame-height 40
  "Default frame height for `reset-frame'")
(defvar default-frame-position '(231 . 717)
	"Default frame position (`top' . `left'")

(defun maximize-frame (&optional frame)
	"Maximize the FRAME"
	(interactive)
	(set-frame-parameter
	 (or frame (selected-frame))
	 'fullscreen 'maximized))

(defun reset-frame (prefix &optional frame)
  "Resize specified frame to the defaults"
  (interactive "p")
  (set-frame-width (or frame (selected-frame)) default-frame-width)
  (set-frame-height (or frame (selected-frame)) default-frame-height)
	(unless (>= prefix 4)
		(set-frame-parameter
		 (or frame (selected-frame))
		 'fullscreen
		 nil)
		(set-frame-parameter
		 (or frame (selected-frame))
		 'top (car default-frame-position))
		(set-frame-parameter
		 (or frame (selected-frame))
		 'left (cdr default-frame-position))))

(provide 'reset-frame)
;;; reset-frame.el ends here
