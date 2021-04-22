;;; complete-hide-drawer -- Summary
;;; Commentary:
;;; Patch for clean org mode view without cluttering darwers.

;;; Code:
(defsubst hgj/overlay-beginning ()
  "Return the start position of an overlay region."
  (save-excursion
    (+ (goto-char (line-beginning-position))
       (skip-chars-backward " \t\n"))))

;;;###autoload
(defun hgj/org--hide-wrapper-toggle (element category force no-error)
  "Toggle visibility for ELEMENT.

ELEMENT is a block or drawer type parsed element.  CATEGORY is
either `block' or `drawer'.  When FORCE is `off', show the block
or drawer.  If it is non-nil, hide it unconditionally.  Throw an
error when not at a block or drawer, unless NO-ERROR is non-nil.

Return a non-nil value when toggling is successful."
  (let ((type (org-element-type element)))
    (cond
     ((memq type
	    (pcase category
	      (`drawer '(drawer property-drawer))
	      (`block '(center-block
			comment-block dynamic-block example-block export-block
			quote-block special-block src-block verse-block))
	      (_ (error "Unknown category: %S" category))))
      (let* ((drawer? (eq category 'drawer))
             (post (org-element-property :post-affiliated element))
             (start (save-excursion
                      (goto-char post)
                      (or (and drawer?
                               (hgj/overlay-beginning))
                          (line-end-position))))
             (end (save-excursion
		    (goto-char (org-element-property :end element))
		    (skip-chars-backward " \t\n")
		    (line-end-position))))
	;; Do nothing when not before or at the block opening line or
	;; at the block closing line.
	(unless (let ((eol (line-end-position)))
                  (and (> (or (and drawer?
                                   (hgj/overlay-beginning))
                              eol)
                          start)
                       (/= eol end)))
	  (let* ((spec (if (eq category 'block) 'org-hide-block 'outline))
		 (flag
		  (cond ((eq force 'off) nil)
			(force t)
			((eq spec (get-char-property start 'invisible)) nil)
			(t t))))
	    (org-flag-region start end flag spec))
	  ;; When the block is hidden away, make sure point is left in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post))
	  ;; Signal success.
	  t)))
     (no-error nil)
     (t
      (user-error (if (eq category 'drawer)
		      "Not at a drawer"
		    "Not at a block"))))))

;; ;;;###autoload
;; (defun hgj/org-clock-find-position (find-unclosed)
;;   "Find the location where the next clock line should be inserted.
;; When FIND-UNCLOSED is non-nil, first check if there is an unclosed clock
;; line and position cursor in that line."
;;   (org-back-to-heading t)
;;   (catch 'exit
;;     (let* ((beg (line-beginning-position))
;; 	   (end (save-excursion (outline-next-heading) (point)))
;; 	   (org-clock-into-drawer (org-clock-into-drawer))
;; 	   (drawer (org-clock-drawer-name)))
;;       ;; Look for a running clock if FIND-UNCLOSED in non-nil.
;;       (when find-unclosed
;; 	(let ((open-clock-re
;; 	       (concat "^[ \t]*"
;; 		       org-clock-string
;; 		       " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
;; 		       " *\\sw+ +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
;; 	  (while (re-search-forward open-clock-re end t)
;; 	    (let ((element (org-element-at-point)))
;; 	      (when (and (eq (org-element-type element) 'clock)
;; 			 (eq (org-element-property :status element) 'running))
;; 		(beginning-of-line)
;; 		(throw 'exit t))))))
;;       ;; Look for an existing clock drawer.
;;       (when drawer
;; 	(goto-char beg)
;; 	(let ((drawer-re (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$")))
;; 	  (while (re-search-forward drawer-re end t)
;; 	    (let ((element (org-element-at-point)))
;; 	      (when (eq (org-element-type element) 'drawer)
;; 		(let ((cend (org-element-property :contents-end element)))
;; 		  (if (and (not org-log-states-order-reversed) cend)
;; 		      (goto-char cend)
;; 		    (forward-line))
;; 		  (throw 'exit t)))))))
;;       (goto-char beg)
;;       (let ((clock-re (concat "^[ \t]*" org-clock-string))
;; 	    (count 0)
;; 	    positions)
;; 	;; Count the CLOCK lines and store their positions.
;; 	(save-excursion
;; 	  (while (re-search-forward clock-re end t)
;; 	    (let ((element (org-element-at-point)))
;; 	      (when (eq (org-element-type element) 'clock)
;; 		(setq positions (cons (line-beginning-position) positions)
;; 		      count (1+ count))))))
;; 	(cond
;; 	 ((null positions)
;; 	  ;; Skip planning line and property drawer, if any.
;; 	  (org-end-of-meta-data)
;; 	  (unless (bolp) (insert "\n"))
;; 	  ;; Create a new drawer if necessary.
;; 	  (when (and org-clock-into-drawer
;; 		     (or (not (wholenump org-clock-into-drawer))
;; 			 (< org-clock-into-drawer 2)))
;; 	    (let ((beg (point)))
;; 	      (insert ":" drawer ":\n:END:\n")
;; 	      (org-indent-region beg (point))
;; 	      (org-flag-region
;;                (save-excursion
;;                  (+ (goto-char (line-beginning-position -1))
;;                     (skip-chars-backward " \t\n")))
;;                (1- (point)) t 'outline)
;;               (forward-line -1))))
;; 	 ;; When a clock drawer needs to be created because of the
;; 	 ;; number of clock items or simply if it is missing, collect
;; 	 ;; all clocks in the section and wrap them within the drawer.
;; 	 ((if (wholenump org-clock-into-drawer)
;; 	      (>= (1+ count) org-clock-into-drawer)
;; 	    drawer)
;; 	  ;; Skip planning line and property drawer, if any.
;; 	  (org-end-of-meta-data)
;; 	  (let ((beg (point)))
;; 	    (insert
;; 	     (mapconcat
;; 	      (lambda (p)
;; 		(save-excursion
;; 		  (goto-char p)
;; 		  (org-trim (delete-and-extract-region
;; 			     (save-excursion (skip-chars-backward " \r\t\n")
;; 					     (line-beginning-position 2))
;; 			     (line-beginning-position 2)))))
;; 	      positions "\n")
;; 	     "\n:END:\n")
;; 	    (let ((end (point-marker)))
;; 	      (goto-char beg)
;; 	      (save-excursion (insert ":" drawer ":\n"))
;; 	      (org-flag-region
;;                (hgj/overlay-beginning)
;;                (1- end) t 'outline)
;;               (org-indent-region (point) end)
;; 	      (forward-line)
;; 	      (unless org-log-states-order-reversed
;; 		(goto-char end)
;; 		(beginning-of-line -1))
;; 	      (set-marker end nil))))
;; 	 (org-log-states-order-reversed (goto-char (car (last positions))))
;; 	 (t (goto-char (car positions))))))))

(provide 'complete-hide-drawer)
;;; complete-hide-drawer.el ends here
