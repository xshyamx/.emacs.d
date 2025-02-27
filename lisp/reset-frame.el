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

(defun reset-frame (&optional frame)
  "Resize specified frame to the defaults"
  (interactive)
  (set-frame-width (or frame (selected-frame)) default-frame-width)
  (set-frame-height (or frame (selected-frame)) default-frame-height))

(provide 'reset-frame)
;;; reset-frame.el ends here
