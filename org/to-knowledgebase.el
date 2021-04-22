;;; to-knowledgebase -- Summary
;;; Commentary:
;;; Smart source code capturing

;;; Code:
;; https://org-roam.discourse.group/t/creating-an-org-roam-note-from-an-existing-headline/978

(defun hgj/fix-relative-links (old-path)
  "Fix file-relative links in current buffer.
File relative links are assumed to originate from OLD-PATH. The
replaced links are made relative to the current buffer."
  (org-with-point-at 1
    (let (link new-link type path)
      (while (re-search-forward org-link-bracket-re nil t)
        (when (setq link (save-match-data (org-element-lineage (org-element-context) '(link) t)))
          (setq type (org-element-property :type link))
          (setq path (org-element-property :path link))
          (pcase type
            ("file"
             (when (and (not (file-exists-p path)) (f-relative-p path)) ;fix only broken links
               (setq new-link
                     (concat type ":" (org-roam-link-get-path (expand-file-name path (file-name-directory old-path)))))
               (replace-match new-link nil t nil 1)))
            ("fuzzy"
             (unless (or (string-match-p "notmuch:id:" path) ;notmuch mail
                         (hgj/valid-fuzzy-link? path))
               (or (and (with-current-buffer (get-file-buffer old-path)
                          (hgj/valid-fuzzy-link? path))
                        (progn (setq new-link
                                     (concat "file:" (org-roam-link-get-path old-path) "::" path))
                               (replace-match new-link nil t nil 1)
                               t))
                   (let ((new-file (concat (save-match-data (funcall org-roam-title-to-slug-function path)) ".org"))) ;assume flat hiearchical note system
                     (and (org-roam--org-roam-file-p new-file)
                          (progn (setq new-link
                                       (concat "file:" new-file))
                                 (replace-match new-link nil t nil 1)
                                 t)))
                   (alert (format "Processing invalid link \"%s\" failed, need manual fix." path)))))
            ))))))

(defun hgj/valid-fuzzy-link? (path)
  "Check whether given fuzzy link PATH valid."
  (let ((org-link-search-must-match-exact-headline t))
    (if (condition-case nil
            (save-excursion
              (save-match-data
                (org-link-search path (point) t)))
          (error nil))
        t nil)))

;;;###autoload
(defun hgj/process-to-knowledge-base ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block))
        (old-file buffer-file-name)
        (exist?))
    (when has-properties
      (let ((propb (- (car has-properties) 13))
            (prope (+ (cdr has-properties) 6)))
        (copy-to-register 0 propb prope)
        (delete-region propb prope)))
    (org-cut-subtree)
    (setq exist? (org-roam-find-file title nil nil 'no-confirm)) ;Inspect the difference of returned values from existing and non-existing ones.
    (org-paste-subtree)
    (kill-line 1)                       ;Delete newline character also
    (while (outline-next-heading)
      (org-promote))
    (goto-char (point-min))
    (when has-properties
      (when exist? (alert "Processed headline may confilct with the existing one."))
      (insert-register 0))
    ;;Fix links
    (hgj/fix-relative-links old-file)))

(provide 'to-knowledgebase)
;;; to-knowledgebase.el ends here
