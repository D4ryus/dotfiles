(package-initialize)

(setq package-enable-at-startup nil)

(setq custom-file "~/.emacs.d/local.el")
(load custom-file t)

;; (require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; dont show tool- or scroll-bar
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(setq-default
 ;; disable startup message
 inhibit-startup-message t

 ;; indent with spaces per default
 indent-tabs-mode nil

 ;; show column number on mode line
 column-number-mode t

 ;; show empty lines and trailing whitespaces
 indicate-empty-lines t
 show-trailing-whitespace t

 ;; dont break words by wrapping to new line
 word-wrap t

 ;; org-agenda show holidays
 org-agenda-include-diary t

 ring-bell-function 'ignore)

;; always show matching parens
(show-paren-mode t)

;; default c coding styles and settings
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq c-default-style "bsd"
		  c-basic-offset 8
		  tab-width 8
		  indent-tabs-mode t)))

;; auto updated proced
(add-hook 'proced-mode-hook
          (lambda ()
            (proced-toggle-auto-update t)))

(add-hook 'lisp-mode-hook
          (lambda () (setq mode-name "λ")))

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

(use-package magit
  :ensure t)

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
                slime-complete-symbol-function 'slime-fuzzy-complete-symbol
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
          (define-key company-active-map (kbd "<tab>")
            'company-complete-selection)
          (define-key company-active-map (kbd "<backtab>")
            'company-complete-common)
          (define-key company-active-map [return] 'newline-and-indent)
          (define-key company-active-map (kbd "RET") 'newline-and-indent))

(use-package slime-company
  :ensure t)

(use-package evil-escape
  :ensure t
  :diminish ""
  :config (setq-default evil-escape-key-sequence "jk")
          (add-to-list 'evil-escape-excluded-major-modes 'term-mode))

(use-package evil
  :ensure t
  :config (evil-mode t)
          (evil-escape-mode t)
          (evil-set-initial-state 'term-mode 'emacs)
          (add-hook 'term-mode-hook (lambda ()
                                      (setq show-trailing-whitespace nil
                                            indicate-empty-lines nil))))

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

(use-package molokai-theme
  :ensure t
  :config (load-theme 'molokai t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
          (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook
                    (lambda ()
                      (org-bullets-mode t))))

(let ((local-init "~/.emacs.d/local.el"))
  (when (file-exists-p local-init)
    (setq custom-file local-init)
    (load local-init)))
