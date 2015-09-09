(when (maybe-require-package 'evil-leader)
  ;; Evil leader must be loaded before evil (as documented).
  (global-evil-leader-mode)

  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    ","  'avy-goto-char-2
    "."  'switch-to-previous-buffer
    ":"  'eval-expression
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "ag" 'org-agenda
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-mode
    "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-semantic-or-imenu ;; Jump to function in buffer
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "k"  'kill-buffer
    "nn" 'narrow-and-set-normal ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'delete-other-windows  ;; C-w o
    "s"  'ag-project            ;; Ag search from project's root
    "r"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))
(semantic-mode t)

(when (maybe-require-package 'evil-jumper)
  (global-evil-jumper-mode))
(maybe-require-package 'evil-surround)
(maybe-require-package 'evil-indent-textobject)

(when (maybe-require-package 'evil)
  ;; Always use evil mode.
  (evil-mode 1)

  ;; My personal evil settings.
  (setq evil-want-C-u-scroll t)
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-symbol-word-search t)

  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-set-initial-state 'sunshine-mode 'emacs)
  (evil-set-initial-state 'octopress-mode 'emacs)
  (evil-set-initial-state 'octopress-server-mode 'emacs)
  (evil-set-initial-state 'octopress-process-mode 'emacs)
  (evil-set-initial-state 'twittering-edit-mode 'insert)

  (add-to-list 'evil-buffer-regexps '("\\*magit:"))
  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)

  (evil-add-hjkl-bindings ag-mode-map 'normal
    "n"   'evil-search-next
    "N"   'evil-search-previous
    "RET" 'compile-goto-error)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
    "RET" 'org-agenda-switch-to)

  (defun next-conflict-marker ()
    (interactive)
    (forward-line 1)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (forward-line -1))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)
  ;(define-key evil-normal-state-map (kbd "C-p")   'helm-projectile)
  (define-key evil-normal-state-map (kbd "-")     'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-]")   'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")    'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")    'show-first-occurrence)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line) ;; I know...
  (define-key evil-normal-state-map (kbd "C-p")
    (lambda ()
      (interactive)
      (previous-line 10)
      (evil-scroll-line-up 10)))
  (define-key evil-normal-state-map (kbd "C-n")
    (lambda ()
      (interactive)
      (next-line 10)
      (evil-scroll-line-down 10)))

  (evil-define-key 'normal org-mode-map
    (kbd "RET")   'org-open-at-point
    "zA"          'org-shifttab
    "zC"          'hide-all
    "zO"          'show-all
    "za"          'org-cycle
    "zc"          'hide-subtree
    "zm"          'hide-body
    "zo"          'show-subtree
    "zr"          'show-all
    "zt"          'org-babel-tangle
    (kbd "C-h")   'org-metaleft
    (kbd "C-j")   'org-metadown
    (kbd "C-k")   'org-metaup
    (kbd "C-l")   'org-metaright
    (kbd "C-S-h") 'org-shiftleft
    (kbd "C-S-j") 'org-shiftdown
    (kbd "C-S-k") 'org-shiftup
    (kbd "C-S-l") 'org-shiftright)

  (define-key evil-normal-state-map "zp" 'helm-projectile)
  (define-key evil-normal-state-map "zj" 'evil-window-prev)
  (define-key evil-normal-state-map "zk" 'evil-window-next)

  (evil-define-key 'insert org-mode-map
    (kbd "C-h")   'org-metaleft
    (kbd "C-j")   'org-metadown
    (kbd "C-k")   'org-metaup
    (kbd "C-l")   'org-metaright
    (kbd "C-S-h") 'org-shiftleft
    (kbd "C-S-j") 'org-shiftdown
    (kbd "C-S-k") 'org-shiftup
    (kbd "C-S-l") 'org-shiftright)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  (evil-ex-define-cmd "om" 'om-status)

  ;; http://superuser.com/questions/684540/evil-mode-evil-shift-left-loses-selection
  ; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab]     'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab]   'djoyner/evil-shift-left-visual)

  (defun djoyner/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore)))

(setq-default evil-shift-width 2)

;(setq evil-default-cursor t)
(setq evil-emacs-state-cursor    '("red"     box))
(setq evil-normal-state-cursor   '("grey60"  box))
(setq evil-visual-state-cursor   '("orange"  hollow))
(setq evil-insert-state-cursor   '("green"   bar))
(setq evil-replace-state-cursor  '("red"     box))
(setq evil-operator-state-cursor '("red"     hollow))

(provide 'init-evil)
