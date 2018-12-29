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
  :config (setf git-commit-summary-max-length 50
                magit-diff-refine-hunk 'all
                magit-diff-highlight-indentation '(("" . tabs))))

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
          (evil-escape-mode t)
          (evil-set-initial-state 'term-mode 'emacs)
          (define-key evil-normal-state-map (kbd "j")
                      'evil-next-visual-line)
          (define-key evil-normal-state-map (kbd "k")
                      'evil-previous-visual-line)
          (add-hook 'term-mode-hook
                    (lambda ()
                      (setq show-trailing-whitespace nil
                            indicate-empty-lines nil))))

(use-package evil-escape
  :ensure t
  :diminish ""
  :config (setq-default evil-escape-key-sequence "jk")
          (add-to-list 'evil-escape-excluded-major-modes 'term-mode))

(use-package paredit
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package evil-paredit
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)
          (add-hook 'lisp-mode-hook #'evil-paredit-mode)
          (add-hook 'clojure-mode-hook #'evil-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
          (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook
                    (lambda ()
                      (org-bullets-mode t))))

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

(defun toggle-trailing-whitespace ()
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
   ("t" toggle-trailing-whitespace "trailing whitespace")
   ("q" nil "cancel")))

(provide 'd4-pkg)
