;;; interview-csv.el --  -*- lexical-binding: t -*-
;; Keywords: lisp, org, csv

;;; Commentary:

;; Provides command `interview-csv' which has to be invoked in the
;; context of an org file. With the headlines of the following
;; structure

;; * [yyyy-MM-dd]
;; ** Candidate Name
;; - Notes
;; - (Recommended|Not Recommended|No Show)

;; The generated csv buffer contains Date, Candidate & Result

;;; Code:

(defun element-bounds (el)
	(list
	 (or (org-element-property :contents-begin el) (org-element-property :begin el))
	 (or (org-element-property :contents-end el) (org-element-property :end el))))

(defun element-contents (el)
	(apply #'buffer-substring-no-properties (element-bounds el)))

(defun narrow-to-element (el)
	(apply #'narrow-to-region (element-bounds el)))

(defun is-candidate ()
	(let ((el (org-element-at-point)))
		(when (and (eq 3 (org-element-property :level el))
							 (string= "Interviews" (org-element-property :raw-value (org-element-property :parent el))))
			el)))

(defun get-interview-date (candidate)
	(let ((parent (org-element-property :parent  (org-element-property :parent  candidate))))
		(replace-regexp-in-string
		 " .*$"
		 ""
		 (string-replace
			"["
			""
			(org-element-property :raw-value parent)))))


(defun get-outcome (candidate)
	(save-restriction
		(widen)
		(apply #'narrow-to-region (element-bounds candidate))
		(let* ((tree (org-element-parse-buffer))
					 (pl (car (org-element-map tree 'plain-list #'identity)))
					 (outcome (string-trim
										 (element-contents
											(car (org-element-map
															 (car (last (org-element-map pl 'item #'identity)))
															 'paragraph #'identity)))))
					 (case-fold-search t))
			(if (string-match-p "not recommended" outcome)
					"Reject"
				(if (string-match-p "recommended" outcome)
						"Select"
					outcome)))))

(defun get-row (candidate)
	(list
	 (get-interview-date candidate)
	 (org-element-property :raw-value candidate)
	 (get-outcome candidate)))

(defun csv-string (rs &optional header)
	"Generate CSV string for rows with optional header"
	(when (and  header
							(= (length header) (length (car rs)))) 
		(push header rs))
	(mapconcat
	 (lambda (r) (string-join r ",")) 
	 rs "\n"))

(defun unique-candidates-by-date (rows)
	(reverse
	 (seq-uniq
		(seq-sort-by
		 #'car
		 #'string>
		 rows)
		(lambda (a b) (string= (nth 1 a) (nth 1 b))))))

(defun interview-csv ()
	(interactive)
	(let ((csv)
				(buf (get-buffer-create "*csv*"))
				(rows 
				 (mapcar #'get-row
								 (remove nil  (org-map-entries
															 #'is-candidate)))))
		(setq csv (csv-string
							 (unique-candidates-by-date rows)
							 '("Date" "Candidate" "Result")))
		(with-current-buffer buf
			(erase-buffer)
			(insert csv)
			(when (symbolp 'csv-mode)
				(csv-mode)))
		(switch-to-buffer buf)))

(provide 'interview-csv)
;;; interview-csv.el -- Ends here
