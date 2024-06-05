;; org-insert-slideshow.el --  -*- lexical-binding: t -*-

(defun slideshow--content (indent)
	"Generate the slide format template with placeholders for caption
and filename with the initial indent from INDENT"
	(let* ((prefix (make-string (if (wholenump indent) indent 0) ? ))
				 (template "%s#+html: <div class=\"slide\">
%s#+caption: %%s
%s[[./%%s]]
%s#+html: </div>")
				 (n (1- (length (split-string template "\n"))))
				 (args (cl-fill (number-sequence 0 n) prefix)))
		(push template args)
		(apply #'format args)))

(defun slideshow--insert-marked-files ()
	"Generate the org mode snippet to be inserted at the point of
invocation and close the opened dired buffer"
	(interactive)
	(unwind-protect
			(let* ((marked (dired-get-marked-files))
						 (buffer (current-buffer))
						 (base-directory (buffer-local-value 'base-directory buffer))
						 (target-buffer (buffer-local-value 'target-buffer buffer))
						 (target-point (buffer-local-value 'target-point buffer))
						 (target-indent (buffer-local-value 'target-indent buffer))
						 (template (slideshow--content target-indent))
						 (prefix (make-string target-indent ? )))
				(when (> (length marked) 0)
					(with-current-buffer target-buffer
						(goto-char target-point)
						(insert (format "%s#+html: <div class=\"slideshow\">\n" prefix))
						(insert (mapconcat
										 (lambda (pair) (format template (car pair) (cdr pair)))
										 (mapcar (lambda (f)
															 (cons
																(file-name-base f)
																(file-relative-name f base-directory)))
														 marked)
										 "\n"))
						(insert (format "\n%s#+html: </div>" prefix)))))
		(kill-buffer (current-buffer))))

(defun slideshow--abort ()
	(interactive)
	(let ((buffer (current-buffer))
				(target-buffer (get-buffer (buffer-local-value 'target-buffer (current-buffer)))))
		(kill-buffer buffer)
		(when (buffer-live-p target-buffer)
			(switch-to-buffer target-buffer))))

(defvar slideshow-image-dirs '("./img/screenshots" "./img")
	"Initial list of directories to look for slideshow images")

(defun org-insert-slideshow ()
	(interactive)
	(let* ((initial-dir
					(seq-find
					 #'file-exists-p
					 (mapcar #'expand-file-name slideshow-image-dirs)))
				 (base-directory (file-name-directory (buffer-file-name)))
				 (dbuf (find-file-noselect (read-string "Images directory: " initial-dir)))
				 (target-buffer (buffer-name))
				 (target-point (point))
				 (target-indent (current-indentation)))
		(with-current-buffer dbuf
			(setq-local base-directory base-directory
									target-buffer target-buffer
									target-point target-point
									target-indent target-indent)
			(keymap-local-set "C-c C-c" #'slideshow--insert-marked-files)
			(keymap-local-set "C-c C-k" #'slideshow--abort))
		(switch-to-buffer-other-window  dbuf)
		(message "Use C-c C-c to insert & C-c C-k to abort")))

(keymap-set org-mode-map "C-c s" #'org-insert-slideshow)
(provide 'org-insert-slideshow)
;;; org-insert-slideshow.el -- Ends here
