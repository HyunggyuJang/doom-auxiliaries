;;; vulpea-agenda -- Summary
;;; Commentary:
;;; Dynamic agenda file manager.

;;; Code:

(eval-when-compile 'org-macs)

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

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (vulpea-buffer-prop-set "filetags" (string-join tags " ")))
(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun vulpea-buffer-title-set (title)
  "Set TITLE in current buffer.
If the title is already set, replace its value."
  (vulpea-buffer-prop-set "title" title))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (let ((value (string-trim
                    (buffer-substring-no-properties
                     (match-beginning 1)
                     (match-end 1)))))
        (unless (string-empty-p value)
          value)))))

(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (vulpea-buffer-prop-get name)))
    (when value
      (split-string-and-unquote value separators))))

(defun vulpea-buffer-tags-get ()
  "Return filetags value in current buffer."
  (vulpea-buffer-prop-get-list "filetags" " "))

(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (let* ((prop-tags (vulpea-buffer-tags-get))
           (tags prop-tags))
      (if (vulpea-project-p)
          (setq tags (add-to-list 'tags "project"))
        (setq tags (remove "project" tags)))
      (unless (eq prop-tags tags)
        (apply #'vulpea-buffer-tags-set tags)))))

(defvar vulpea-exclude-files-regexp
  "\\(^archive\\.org$\\|^old\\.org$\\)"
  "Exclude files from vulpea realm.")

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (not (string-match-p vulpea-exclude-files-regexp (file-name-nondirectory buffer-file-name)))
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

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
         (title (vulpea-buffer-prop-get "title"))
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
