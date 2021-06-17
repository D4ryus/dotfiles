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

(use-package python
  :config (setq python-shell-buffer-name "Python REPL")
  :bind (:map python-mode-map
         ("C-c C-c" . python-shell-send-defun)
         ("C-c C-k" . python-shell-send-buffer)))

(use-package request)

(use-package try)

(use-package diminish)

(use-package whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-style '(face tabs lines-tail))
  (whitespace-display-mappings nil)
  :config
  (set-face-attribute 'whitespace-tab nil
                      :underline t
                      :foreground nil
                      :background nil)
  (set-face-attribute 'whitespace-line nil
                      :strike-through t
                      :foreground nil
                      :background nil)
  (add-hook 'prog-mode-hook
            (lambda ()
              (whitespace-mode 1)
              (setq show-trailing-whitespace t))))

(use-package eldoc
  :diminish eldoc-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config (add-hook 'css-mode-hook
                    (lambda () (rainbow-mode 1))))

(use-package edit-color-stamp)

(use-package restclient)

(use-package modus-themes
  :config (load-theme 'modus-vivendi t))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package magit
  :custom
  (magit-define-global-key-bindings nil)
  :config
  (setf git-commit-summary-max-length 50
        magit-diff-refine-hunk 'all
        magit-diff-highlight-indentation '(("" . tabs))))

(use-package magit-todos
  :config (magit-todos-mode))

(use-package trident-mode
  :diminish trident-mode
  :config (add-hook 'lisp-mode-hook
                    (lambda () (trident-mode 1))))

(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay nil)
  :config
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

(use-package slime-company
  :custom
  (slime-company-completion 'fuzzy))

(use-package slime
  :custom
  (lisp-loop-indent-subclauses nil)
  (lisp-loop-indent-forms-like-keywords t)
  (lisp-indent-function 'common-lisp-indent-function)
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
        slime-highlight-compiler-notes t
        slime-repl-history-remove-duplicates t
        slime-repl-history-trim-whitespaces t
        slime-inhibit-pipelining nil)
  (add-hook 'slime-mode-hook
            (lambda ()
              ;; Should not be required, but slime-company broke
              ;; somehow and is not activated automatically :/
              (add-to-list 'company-backends 'company-slime)
              (define-key evil-normal-state-local-map (kbd "M-.")
                'slime-edit-definition)))
  (defun re-eval ()
    (interactive)
    (with-current-buffer (get-buffer "*slime-repl sbcl*")
      (slime-repl-resend))))

(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  :config
  (add-hook 'geiser-repl-mode-hook
            (lambda ()
              (define-key geiser-repl-mode-map (kbd "\C-d") 'geiser-repl-exit))))

(use-package geiser-guile)

(use-package cider)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-tree)
  (evil-want-C-i-jump nil)
  (evil-symbol-word-search t)
  (evil-intercept-maps nil)
  (evil-overriding-maps nil)
  (evil-disable-insert-state-bindings t)
  (evil-emacs-state-modes nil)
  (evil-motion-state-modes nil)
  (evil-insert-state-modes '(dired-mode
                             elfeed-search-mode
                             elfeed-show-mode
                             geiser-repl-mode
                             geiser-repl-mode
                             grep-mode
                             sldb-mode
                             slime-connection-list-mode
                             slime-fuzzy-completions-mode
                             slime-repl-mode
                             slime-trace-dialog-mode
                             special-mode
                             tabulated-list-mode
                             term-mode
                             eshell-mode
                             org-agenda-mode))
  (evil-want-minibuffer t)
  (evil-normal-state-modes '(git-commit-mode
                             prog-mode
                             text-mode))
  (evil-set-leader 'normal (kbd ","))
  (evil-define-key 'normal 'global (kbd "<leader>gg")
                   'vc-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader>ms")
                   'magit-status)
  (define-key evil-normal-state-map (kbd "M-.")
    'xref-find-definitions)
  :config
  (evil-mode 1))

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

(use-package hydra)

(use-package erc-hl-nicks)

(use-package erc
  :custom
  (erc-remove-parsed-property nil)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-enable-logging 'erc-log-all-but-server-buffers)
  (erc-save-buffer-on-part t)
  (erc-log-insert-log-on-open t)
  (erc-input-line-position -1)
  (erc-timestamp-format "%H:%M ")
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-modules '(completion log hl-nicks autojoin button irccontrols
                 list match menu move-to-prompt netsplit networks
                 noncommands readonly ring sound stamp track))
  :config
  (erc-update-modules)
  (erc-fill-disable)
  (erc-hl-nicks-mode)
  (erc-track-mode)
  (erc-scrolltobottom-mode)
  (erc-log-mode))

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

(use-package eglot)

(use-package flycheck)

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map)))

(use-package highlight-symbol
  :diminish highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook
            (lambda ()
              (highlight-symbol-mode))))

(use-package yaml-mode
  :mode "\\.yml\\'")

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
