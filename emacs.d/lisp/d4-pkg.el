(when (and (< emacs-major-version 27)
           (< emacs-minor-version 3))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

(setq package-enable-at-startup nil)

;; (require 'package)
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

(setq use-package-always-ensure t)

(require 'use-package)

(use-package try
  :ensure t)

(use-package ivy
  :diminish (ivy-mode . "")
  :ensure t
  :config (ivy-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package edit-color-stamp
  :ensure t)

(use-package restclient
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package magit
  :ensure t
  :config
  (progn
    (setf git-commit-summary-max-length 50
          magit-diff-refine-hunk 'all
          magit-diff-highlight-indentation '(("" . tabs)))
    (define-key magit-file-mode-map (kbd "C-x g") nil)))

(use-package trident-mode
  :ensure t
  :config (add-hook 'lisp-mode-hook
                    #'(lambda () (trident-mode 1))))

(use-package slime
  :ensure t
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

(use-package geiser
  :ensure t)

(use-package cider
  :ensure t)

(use-package company
  :ensure t
  :diminish ""
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (setq company-idle-delay 0.2)
          (define-key company-active-map (kbd "\C-n") 'company-select-next)
          (define-key company-active-map (kbd "\C-p") 'company-select-previous)
          (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
          (define-key company-active-map (kbd "TAB")
                      'company-complete-selection)
          (define-key company-active-map (kbd "S-<tab>")
                      'company-complete-common)
          (define-key company-active-map (kbd "<backtab>")
                      'company-complete-common)
          (define-key company-active-map [return] 'newline-and-indent)
          (define-key company-active-map (kbd "RET") 'newline-and-indent))

(use-package slime-company
  :ensure t)

(use-package evil
  :init (setq evil-want-C-i-jump nil
              evil-symbol-word-search t)
  :ensure t
  :config (evil-mode t)
          (mapc (lambda (pair)
                  (evil-set-initial-state (car pair) (cdr pair)))
                '((term-mode . emacs)
                  (dired-mode . emacs)
                  (Buffer-menu-mode . emacs)))
          (define-key evil-normal-state-map (kbd "j")
                      'evil-next-visual-line)
          (define-key evil-normal-state-map (kbd "k")
                      'evil-previous-visual-line)
          (add-hook 'term-mode-hook
                    (lambda ()
                      (setq show-trailing-whitespace nil
                            indicate-empty-lines nil))))

(defvar d4-lisp-mode-hooks '(emacs-lisp-mode-hook
                             lisp-mode-hook
                             clojure-mode-hook
                             geiser-mode-hook))

(use-package paredit
  :ensure t
  :config (mapc (lambda (hook)
                  (add-hook hook #'enable-paredit-mode))
                d4-lisp-mode-hooks))

(use-package evil-paredit
  :ensure t
  :config (mapc (lambda (hook)
                  (add-hook hook #'evil-paredit-mode))
                d4-lisp-mode-hooks))

(use-package rainbow-delimiters
  :ensure t
  :config (mapc (lambda (hook)
                  (add-hook hook #'rainbow-delimiters-mode))
                d4-lisp-mode-hooks))

(use-package org-ref
  :ensure t)

(use-package irony
  :ensure t
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

(use-package company-irony
  :ensure t)

(use-package hydra
  :ensure t)

(use-package erc-hl-nicks
  :ensure t)

(use-package erc
  :ensure t
  :hook (erc-mode-hook .
         (lambda ()
           (set (make-local-variable 'scroll-conservatively) 100)))
  :config
  (progn
    (setq erc-remove-parsed-property nil
          erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477")
          erc-enable-logging 'erc-log-all-but-server-buffers
          erc-input-line-position -2
          erc-timestamp-format "%H:%M "
          erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-modules '(completion log hl-nicks autojoin button irccontrols
                        list match menu move-to-prompt netsplit networks noncommands
                        readonly ring sound stamp track))
    (erc-update-modules)
    (erc-fill-disable)
    (erc-hl-nicks-mode)
    (erc-log-mode)
    (erc-track-mode)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package skewer-mode
  :ensure t
  :init (add-hook 'js2-mode-hook #'skewer-mode)
        (add-hook 'css-mode-hook #'skewer-css-mode)
        (add-hook 'html-mode-hook #'skewer-html-mode))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package lsp-python
  :ensure t
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
