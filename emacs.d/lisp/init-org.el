(when (and (maybe-require-package 'org)
           (maybe-require-package 'evil-leader))

  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files "~/Documents/workspace/org" "\.org$"))

  (evil-leader/set-key-for-mode 'org-mode
    "t"  'org-set-tags
    "p"  '(lambda ()
            (interactive)
            (org-insert-property-drawer))
    "d"  'org-deadline
    "s"  'org-schedule
    "a"  'org-agenda
    "ns" 'org-narrow-to-subtree
    "$"  'org-archive-subtree)

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

  ;; https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
         (progn
           ;enable auto-revert-mode to update reftex when bibtex file changes on disk
           (global-auto-revert-mode t)
           (reftex-parse-all)
           ;add a custom reftex cite format to insert links
           (reftex-set-cite-format
            '((?b . "[[bib:%l][%l-bib]]")
              (?n . "[[notes:%l][%l-notes]]")
              (?p . "[[papers:%l][%l-paper]]")
              (?t . "%t")
              (?h . "**** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n*****     :noexport:\n[[papers:%l][%l-paper]] [[bib:%l][BibTeX]]")))))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

  (defun org-mode-reftex-search ()
    ;;jump to the notes for the paper pointed to at from reftex search
    (interactive)
    (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

  (setq org-link-abbrev-alist
      '(("bib" . "./bibliography.bib::%s")
	("notes" . "./bibliography.org::#%s")
	("papers" . "./papers/%s.pdf")))

  (add-hook 'org-mode-hook 'org-mode-reftex-setup)

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
                 'my-rtcite-export-handler))

(provide 'init-org)
