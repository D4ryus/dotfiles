;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; set ohsnap as font, if available
(when (member "ohsnap" (font-family-list))
  (set-frame-font "ohsnap" t t))

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
 org-agenda-include-diary t)

;; always show matching parens
(show-paren-mode t)

;; default c coding styles and settings
(setq c-default-style "bsd"
      c-basic-offset 8
      tab-width 8
      indent-tabs-mode t)

;; packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

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
  :config (setq inferior-lisp-program "/usr/bin/sbcl")
          (slime-setup '(slime-fancy slime-company))
          (defun re-eval ()
            (interactive)
            (with-current-buffer (get-buffer "*slime-repl sbcl*")
              (slime-repl-resend))))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (define-key company-active-map (kbd "\C-n") 'company-select-next)
          (define-key company-active-map (kbd "\C-p") 'company-select-previous)
          (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer))

(use-package slime-company
  :ensure t)

(use-package evil-escape
  :ensure t
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
          (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(use-package evil-paredit
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)
          (add-hook 'lisp-mode-hook #'evil-paredit-mode))

(use-package molokai-theme
  :ensure t
  :config (load-theme 'molokai t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
          (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(let ((local-init "~/.emacs.d/local.el"))
  (when (file-exists-p local-init)
    (setq custom-file local-init)
    (load local-init)))
