(when (and (maybe-require-package 'org)
           (maybe-require-package 'evil-leader))

  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files "~/Dropbox/org" "\.org$"))

  (evil-leader/set-key-for-mode 'org-mode
    "t"  'org-set-tags
    "P"  '(lambda ()
            (interactive)
            (org-insert-property-drawer))
    "p"  'my-org-screenshot
    "d"  'org-deadline
    "s"  'org-schedule
    "ns" 'org-narrow-to-subtree
    "$"  'org-archive-subtree)

    (defun my-org-screenshot ()
      "Take a screenshot into a time stamped unique-named file in the
    same directory as the org-buffer and insert a link to this file."
      (interactive)
      (org-display-inline-images)
      (setq filename
            (concat
             (make-temp-name
              (concat (file-name-nondirectory (buffer-file-name))
                      "_imgs/"
                      (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename)))
      (call-process "screencapture" nil nil nil "-i" filename)
      (if (file-exists-p filename)
        (insert (concat "[[./" filename "]]"))))

  (add-hook 'org-mode-hook
            (lambda ()
              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
              (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
              (auto-fill-mode)
              (flyspell-mode)))

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-log-done t)

  ;; https://www-public.tem-tsp.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
  (defun my-rtcite-export-handler (path desc format)
    (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
    (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                     (match-string 1 path)))
           (path (substring path 0 (match-beginning 0))))
      (cond ((eq format 'latex)
             (if (or (not desc)
                     (equal 0 (search "rtcite:" desc)))
                 (format "\\cite{%s}" search)
               (format "\\cite[%s]{%s}" desc search))))))

  (require 'org)
  (org-add-link-type "rtcite"
                 'org-bibtex-open
                 'my-rtcite-export-handler)

  (advice-add 'org-set-tags :around 'kk/run-with-no-helm)

  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-todo-keywords
       '((sequence "TODO" "WAIT" "ONGO" "|" "DONE" "CANC")))

  (setq reftex-default-bibliography '("~/Dropbox/org/phd/bibliography.bib"))

  (setq org-src-fontify-natively t)
  )

(when (maybe-require-package 'org-ac)
  (org-ac/config-default))

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (sh . t)))

;; RefTeX
;(setq reftex-bibliography-commands
;      '("bibliography" "nobibliography" "addbibresource"))
;'(reftex-use-external-file-finders t)
;(require 'ox-bibtex)
(setq reftex-default-bibliography '("./bibliography.bib"))

;; https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         ;(reftex-parse-all)
         ;add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l][%l-bib]]")
            (?n . "[[notes:%l][%l-notes]]")
            (?p . "[[papers:%l][%l-paper]]")
            (?t . "\\textcite{%l}")
            (?a . "\\autocite{%l}")
            (?h . "*** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n\\fullcite{%l}\n****     :noexport:\n[[papers:%l][%l-paper]] [[bib:%l][BibTeX]]")))
         ))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

;(eval-after-load 'reftex-vars
;  '(progn
;      (add-to-list 'reftex-cite-format-builtin
;                   '(org "Org-mode citation"
;                         ((?b . "[[bib:%l][%l-bib]]")
;                          (?n . "[[notes:%l][%l-notes]]")
;                          (?p . "[[papers:%l][%l-paper]]")
;                          (?t . "\\textcite{%l}")
;                          (?a . "\\autocite{%l}")
;                          (?h . "*** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n\\fullcite{%l}\n****     :noexport:\n[[papers:%l][%l-paper]] [[bib:%l][BibTeX]]")
;                          (?d . ",%l")            ; for appending
;                          )))))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

(setq org-link-abbrev-alist
    '(("bib" . "./bibliography.bib::%s")
      ("notes" . "./bibliography.org::#%s")
      ("papers" . "./papers/%s.pdf")))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun kk/run-with-no-helm (orig-func &rest args)
  "Run org-set-tags without helm."
  (if (boundp 'helm-mode)
      (let ((orig-helm-mode helm-mode))
	(unwind-protect
	    (progn
	      (helm-mode 0)
	      (apply orig-func args)
	      )
	  (helm-mode (if orig-helm-mode 1 0))))
    (apply orig-func args)
    ))

(when (maybe-require-package 'wc-mode)
  (add-hook 'org-mode 'wc-mode))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (sh . t)
   (octave . t)
   ))

(provide 'init-org)
