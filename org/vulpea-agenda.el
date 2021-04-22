;;; vulpea-agenda -- Summary
;;; Commentary:
;;; Dynamic agenda file manager.

;;; Code:

(declare-function org-depend-block-todo "org-depend" (change-plist))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (and (eq (org-element-property :todo-type h)
               'todo)
           (not (eq (org-element-property :priority h)
                    ?D))
           (save-excursion
             (goto-char (org-element-property :begin h))
             (run-hook-with-args-until-failure
                 'org-blocker-hook
                 (list :type 'todo-state-change
                       :position (point)
                       :from 'todo
                       :to 'done)))))
    nil 'first-match))

(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (prop-tags (org-roam--extract-tags-prop file))
           (tags prop-tags))
      (if (vulpea-project-p)
          (setq tags (add-to-list 'tags "Project"))
        (setq tags (remove "Project" tags)))
      (unless (eq prop-tags tags)
        (org-roam--set-global-prop
         "ROAM_TAGS"
         (combine-and-quote-strings tags))))))

(defvar vulpea-exclude-files-regexp
  "\\(^archive\\.org$\\|^old\\.org$\\|^gcal\\.org$\\)"
  "Exclude files from vulpea realm.")

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (not (string-match-p vulpea-exclude-files-regexp (file-name-nondirectory buffer-file-name)))
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
  "Return a list of note files containing Project tag."
  (seq-map
   #'car
   (org-roam-db-query
    [:select file
     :from tags
     :where (like tags (quote "%\"Project\"%"))])))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (car-safe (org-roam--extract-titles-title)))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (truncate-string-to-width result len nil ?  "...")
      result)))


(provide 'vulpea-agenda)
;;; vulpea-agenda.el ends here
