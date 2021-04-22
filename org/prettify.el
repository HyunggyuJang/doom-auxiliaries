;;; Prettify -- Summary
;;; Commentary:
;;; Prettify org mode for multi-linguistic environment.

;;; I assume this package being loaded only after org.

;;; Code:

;; ;; Make =Class=ed available  https://emacs.stackexchange.com/a/18511
(setcar org-emphasis-regexp-components "-[:space:]('\"{\u200B")
(setcar (nthcdr 1 org-emphasis-regexp-components)  "-[:space:].,:!?;'\")}\\[\u200B")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;; Auxiliarly functions
(defun hgj/org-zw-as-word ()
  (modify-syntax-entry ?\u200B "w"))

;;;###autoload
(defun hgj/org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be the marker character.  If it is a space, it means to
remove the emphasis of the selected region.
If CHAR is not given (for example in an interactive call) it will be
prompted for."
  (interactive)
  (let ((erc org-emphasis-regexp-components)
	(string "")
	(zw "\x200B")
	beg end move s)
    (if (org-region-active-p)
	(setq beg (region-beginning)
	      end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (unless char
      (message "Emphasis marker or tag: [%s]"
	       (mapconcat #'car org-emphasis-alist ""))
      (setq char (read-char-exclusive)))
    (if (equal char ?\s)
	(setq s ""
	      move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(user-error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    ;; Ugly. Need refactor.
    ;; Strip zero width characters around captured string
    (while (and (> (length string) 1)
		(equal (substring string 0 1) zw))
      (setq string (substring string 1)))
    (while (and (> (length string) 1)
		(equal (substring string -1) zw))
      (setq string (substring string 0 -1)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (when beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert zw))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert zw) (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(provide 'prettify)
;;; prettify.el ends here
