;;; my-org -- Summary
;;; Commentary:
;;; This is my custom org functions which overrides default counterparts.

;;; I assume this package being loaded only after org.

;;; Code:

;;;###autoload
(defun hgj/org-show-notification (notification)
  "Show notification."
  (set-text-properties 0 (length notification) nil notification)
  (mac-do-applescript (format "display notification %S" notification)))

;;;###autoload
(defun hgj/org-agenda-skip-if-not-blocked ()
  (let ((next-headline (save-excursion
                         (or (outline-next-heading) (point-max)))))
    (if (not (org-entry-blocked-p)) next-headline)))

;;;###autoload
(defun hgj/org-agenda-skip-blocked-or-timestamped ()
  "Skip blocked entries."
  (let ((heading-end (save-excursion (or (outline-next-heading) (point-max)))))
    (if (org-entry-blocked-p)
        heading-end
        (org-agenda-skip-entry-if 'timestamp)
        )))

;;;###autoload
(defun hgj/org-store-link-fuzzy (arg &optional interactive?)
  (interactive "P\np")
  (let ((org-id-link-to-org-use-id nil))
    (org-store-link arg interactive?)))

;;;###autoload
(defun hgj/handle-current-heading ()
  "Select current heading."
  (let ((orig-buffer (nth 0 (buffer-list)))
        pt)
    (when (with-current-buffer orig-buffer
            (and (eq major-mode 'org-mode)
                 (setq pt (save-excursion (outline-next-heading) (point)))))
      (org-capture-put
       :immediate-finish t
       :jump-to-captured t)
      (switch-to-buffer orig-buffer)
      (goto-char pt))))

;;;###autoload
(defun hgj/handle-current-point ()
  "Select current point."
  (let ((orig-buffer (nth 0 (buffer-list)))
        pt)
    (when (with-current-buffer orig-buffer
            (eq major-mode 'org-mode))
      (org-capture-put
       :immediate-finish t
       :jump-to-captured t)
      (switch-to-buffer orig-buffer))))

(defcustom hgj/wash-list
  '((" - Emacs Stack Exchange" "")
    (" - Stack Overflow" ""))
  "A list of (REGEX REP) to be applied on link title."
  :type 'list)

;;;###autoload
(defun hgj/wash-title (title)
  "Return strip downed web site's TITLE."
  (dolist (repl hgj/wash-list)
    (setq title (replace-regexp-in-string
                 (nth 0 repl) (nth 1 repl) title)))
  title)

(provide 'my-org)
;;; my-org.el ends here
