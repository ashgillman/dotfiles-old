(add-hook 'eshell-mode-hook 'eshell-load-aliases)
    (defun get-string-from-file (filePath)
      "Return filePath's file content."
      (with-temp-buffer
        (insert-file-contents filePath)
        (buffer-string)))

    (defun re-n-matches ()
      (1- (/ (length (match-data)) 2)))

    (defun match-strings-all (&optional string)
      "Return the list of all expressions matched in last search.
      STRING is optionally what was given to `string-match'."
      (loop for i from 0 to (re-n-matches)
        collect (match-string-no-properties i string)))

    (defun re-find-all (regexp string &optional groups yes-props)
      "like python's re.find_all"
      (let (
        (groups (or groups (list (regexp-count-capture-groups regexp))))
        (match-string-fun (if (not yes-props) 'match-string 'match-string-no-properties))
        (start 0)
        (matches nil )
        )
        (while (setq start (and (string-match regexp string start) (match-end 0)))
        (setq matches (cons (cdr (match-strings-all string)) matches))
        )
        (setq matches (reverse matches))
        (if (not (cdar matches))
        (mapcar 'car matches)
          matches
          )
        )
      )

    (defun apply-eshell-alias (alias &rest definition)
      "basically taken from eshell/alias function"
        (if (not definition)
        (setq eshell-command-aliases-list
              (delq (assoc alias eshell-command-aliases-list)
                    eshell-command-aliases-list))
          (and (stringp definition)
           (set-text-properties 0 (length definition) nil definition))
          (let ((def (assoc alias eshell-command-aliases-list))
            (alias-def (list alias
                             (eshell-flatten-and-stringify definition))))
        (if def
            (setq eshell-command-aliases-list
                  (delq def eshell-command-aliases-list)))
        (setq eshell-command-aliases-list
              (cons alias-def eshell-command-aliases-list))))
      )
    (defun eshell-load-aliases ()
      (interactive)
      (mapc (lambda (alias-def) (apply 'eshell/alias alias-def))
        (re-find-all "^alias \\([^=]+\\)='?\\(.+?\\)'?$"
                     (get-string-from-file  (concat (getenv "HOME") "/" ".aliases"))
                     )
        )
      )

(provide 'init-eshell)
