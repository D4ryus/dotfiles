(when (and (< emacs-major-version 27)
           (< emacs-minor-version 3))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package try)

(use-package ivy
  :diminish (ivy-mode . "")
  :config (ivy-mode 1))

(use-package rainbow-mode
  :config (add-hook 'css-mode-hook
                    #'(lambda () (rainbow-mode 1))))

(use-package edit-color-stamp)

(use-package restclient)

(use-package doom-themes
  :config (load-theme 'doom-molokai t))

(use-package which-key
  :config (which-key-mode t))

(use-package magit
  :config
  (progn
    (setf git-commit-summary-max-length 50
          magit-diff-refine-hunk 'all
          magit-diff-highlight-indentation '(("" . tabs)))
    (define-key magit-file-mode-map (kbd "C-x g") nil)))

(use-package trident-mode
  :config (add-hook 'lisp-mode-hook
                    #'(lambda () (trident-mode 1))))

(use-package slime
  :config (slime-setup '(slime-fancy
                         slime-company
                         slime-indentation
                         slime-compiler-notes-tree
                         slime-hyperdoc
                         slime-xref-browser
                         slime-references
                         slime-asdf))
          (setq inferior-lisp-program "/usr/bin/sbcl"
                lisp-loop-indent-subclauses nil
                lisp-loop-indent-forms-like-keywords t
                lisp-indent-function 'common-lisp-indent-function
                slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
                slime-highlight-compiler-notes t
                slime-repl-history-remove-duplicates t
                slime-repl-history-trim-whitespaces t)
          (defun re-eval ()
            (interactive)
            (with-current-buffer (get-buffer "*slime-repl sbcl*")
              (slime-repl-resend))))

(use-package geiser)

(use-package cider)

(use-package company
  :diminish ""
  :config (setq company-idle-delay nil)
          (global-set-key (kbd "C-x TAB") 'company-complete)
          (global-company-mode)
          (mapc (lambda (kbd-fn)
                  (define-key company-active-map (car kbd-fn) (cdr kbd-fn)))
                (list (cons (kbd "\C-n")      'company-select-next)
                      (cons (kbd "\C-p")      'company-select-previous)
                      (cons (kbd "\C-d")      'company-show-doc-buffer)
                      (cons (kbd "<tab>")     'company-complete-selection)
                      (cons (kbd "TAB")       'company-complete-selection)
                      (cons (kbd "S-<tab>")   'company-complete-common)
                      (cons (kbd "<backtab>") 'company-complete-common)
                      (cons [return]          'newline-and-indent)
                      (cons (kbd "RET")       'newline-and-indent))))

(use-package slime-company)

(use-package evil
  :init (setq evil-want-C-i-jump nil
              evil-symbol-word-search t)
  :config (evil-mode t)
          (mapc (lambda (pair)
                  (evil-set-initial-state (car pair) (cdr pair)))
                '((term-mode . emacs)
                  (dired-mode . emacs)
                  (Buffer-menu-mode . emacs)
                  (grep-mode . emacs)
                  (slime-fuzzy-completions-mode . emacs)
                  (slime-repl-mode . emacs)
                  (geiser-repl-mode . emacs)
                  (help-mode . emacs)))
          (define-key evil-normal-state-map (kbd "j")
                      'evil-next-visual-line)
          (define-key evil-normal-state-map (kbd "k")
                      'evil-previous-visual-line))

(defvar d4-lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    clojure-mode-hook
    geiser-mode-hook))

(use-package paredit
  :config (mapc (lambda (hook)
                  (add-hook hook #'enable-paredit-mode))
                (append d4-lisp-mode-hooks
                        '(slime-repl-mode-hook
                          geiser-repl-mode-hook))))

(use-package evil-paredit
  :config (mapc (lambda (hook)
                  (add-hook hook #'evil-paredit-mode))
                d4-lisp-mode-hooks))

(use-package rainbow-delimiters
  :config (mapc (lambda (hook)
                  (add-hook hook #'rainbow-delimiters-mode))
                d4-lisp-mode-hooks))

(use-package org-ref)

(use-package irony
  :bind ("C-x c" . compile)
  :config
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook
              (lambda ()
                (define-key irony-mode-map [remap completion-at-point]
                  'irony-completion-at-point-async)
                (define-key irony-mode-map [remap complete-symbol]
                  'irony-completion-at-point-async)))))

(use-package company-irony)

(use-package hydra)

(use-package erc-hl-nicks)

(use-package erc
  :hook (erc-mode-hook .
         (lambda ()
           (set (make-local-variable 'scroll-conservatively) 100)))
  :config
  (progn
    (setq erc-remove-parsed-property nil
          erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                    "324" "329" "332" "333" "353" "477")
          erc-enable-logging 'erc-log-all-but-server-buffers
          erc-input-line-position -2
          erc-timestamp-format "%H:%M "
          erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-modules '(completion log hl-nicks autojoin button irccontrols
                        list match menu move-to-prompt netsplit networks
                        noncommands readonly ring sound stamp track))
    (erc-update-modules)
    (erc-fill-disable)
    (erc-hl-nicks-mode)
    (erc-log-mode)
    (erc-track-mode)))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package skewer-mode
  :init (add-hook 'js2-mode-hook #'skewer-mode)
        (add-hook 'css-mode-hook #'skewer-css-mode)
        (add-hook 'html-mode-hook #'skewer-html-mode))

(use-package lsp-mode
  :init (setq lsp-enable-snippet nil))

(use-package lsp-ui)

(use-package company-lsp)

(use-package lsp-python
  :hook (python-mode . lsp))

(defun d4-toggle-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))

(global-set-key
 (kbd "C-x t")
 (defhydra toggle ()
   "toggle modes and settings"
   ("l" toggle-truncate-lines "truncate")
   ("w" whitespace-mode "whitespace")
   ("c" rainbow-mode "rainbow")
   ("n" linum-mode "line numbers")
   ("t" d4-toggle-trailing-whitespace "trailing whitespace")
   ("q" nil "cancel")))

(provide 'd4-pkg)
