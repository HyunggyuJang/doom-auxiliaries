;;; source-code-capture -- Summary
;;; Commentary:
;;; Smart source code capturing

;;; Code:
(defun hgj/org-capture-get-src-block-string (major-mode)
    "Given a major mode symbol, return the associated org-src block
    string that will enable syntax highlighting for that language

    E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

    (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
      (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;;;###autoload
(defun hgj/org-capture-code-snippet (f)
  (with-current-buffer (find-buffer-visiting f)
    (let ((code-snippet (buffer-substring-no-properties (mark) (point)))
          (func-name (which-function))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (hgj/org-capture-get-src-block-string major-mode)))
      (format
       "[[file:%s::%s][Source]]
In ~%s~:
#+BEGIN_SRC %s :tangle no
%s
#+END_SRC"
       file-name
       line-number
       func-name
       org-src-mode
       code-snippet))))

;;;###autoload
(defun hgj/org-copy-code-snippet ()
  "Copy current marked source code region in org mode format to `kill-ring'."
  (interactive)
  (kill-new (hgj/org-capture-code-snippet buffer-file-name)))

(provide 'source-code-capture)
;;; source-code-capture.el ends here
