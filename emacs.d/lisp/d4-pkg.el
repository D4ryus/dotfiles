(when (and (< emacs-major-version 27)
           (< emacs-minor-version 3))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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

(use-package winner
  :config (winner-mode))

(use-package try)

(use-package diminish)

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines-tail)
        whitespace-display-mappings nil)
  (set-face-attribute 'whitespace-tab nil
                      :background "#181818")
  (add-hook 'prog-mode-hook
            (lambda ()
              (whitespace-mode 1)
              (setq show-trailing-whitespace t))))

(use-package eldoc
  :diminish eldoc-mode)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config (add-hook 'css-mode-hook
                    (lambda () (rainbow-mode 1))))

(use-package edit-color-stamp)

(use-package restclient)

(use-package doom-themes
  :config (load-theme 'doom-molokai t))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package magit
  :config
  (setf git-commit-summary-max-length 50
        magit-diff-refine-hunk 'all
        magit-diff-highlight-indentation '(("" . tabs)))
  (define-key magit-file-mode-map (kbd "C-x g") nil))

(use-package magit-todos
  :config (magit-todos-mode))

(use-package trident-mode
  :diminish trident-mode
  :config (add-hook 'lisp-mode-hook
                    (lambda () (trident-mode 1))))

(use-package slime
  :config
  (slime-setup '(slime-fancy
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
        slime-repl-history-trim-whitespaces t
        slime-inhibit-pipelining nil)
  (defun re-eval ()
    (interactive)
    (with-current-buffer (get-buffer "*slime-repl sbcl*")
      (slime-repl-resend))))

(use-package geiser)

(use-package cider)

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay nil)
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
  :config
  (evil-mode t)
  (mapc (lambda (pair)
          (evil-set-initial-state (car pair) (cdr pair)))
        '((term-mode . emacs)
          (dired-mode . emacs)
          (Buffer-menu-mode . emacs)
          (grep-mode . emacs)
          (slime-fuzzy-completions-mode . emacs)
          (slime-repl-mode . emacs)
          (geiser-repl-mode . emacs)
          (help-mode . emacs)
          (slime-trace-dialog-mode . emacs)
          (slime-connection-list-mode . emacs)
          (tabulated-list-mode . emacs)
          (text-mode . normal)
          (git-commit-mode . normal)))
  (add-to-list 'magit-blame-disable-modes 'evil-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook
            (lambda () (evil-emacs-state 1)))
  (add-hook 'hexl-mode-hook
            (lambda () (evil-emacs-state 1)))
  (define-key evil-normal-state-map (kbd "j")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k")
    'evil-previous-visual-line))

(use-package evil-surround
  :config (global-evil-surround-mode t))

(defvar d4-lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    clojure-mode-hook
    geiser-mode-hook))

(use-package paredit
  :diminish paredit-mode
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
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook
            (lambda ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))))

(use-package company-irony)

(use-package hydra)

(use-package erc-hl-nicks)

(use-package erc
  :hook (erc-mode-hook
         . (lambda ()
             (set (make-local-variable 'scroll-conservatively) 100)))
  :config
  (setq erc-remove-parsed-property nil
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        erc-enable-logging 'erc-log-all-but-server-buffers
        erc-log-insert-log-on-open t
        erc-log-write-after-insert t
        erc-log-write-after-send t
        erc-input-line-position -1
        erc-timestamp-format "%H:%M "
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-modules '(completion log hl-nicks autojoin button irccontrols
                      list match menu move-to-prompt netsplit networks
                      noncommands readonly ring sound stamp track))
  (erc-update-modules)
  (erc-fill-disable)
  (erc-hl-nicks-mode)
  (erc-log-mode)
  (erc-track-mode)
  (erc-scrolltobottom-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :mode "\\.ned\\'"
  :mode "\\.io\\'"
  :bind (:map lua-prefix-mode-map
              ("C-c" . lua-send-defun)))

(use-package skewer-mode
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode)
  (add-hook 'html-mode-hook #'skewer-html-mode))

(use-package lsp-mode
  :init (setq lsp-enable-snippet nil
              lsp-prefer-flymake :none))

(use-package lsp-ui)

(use-package company-lsp)

(use-package flycheck)

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

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
