;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; set ohsnap as font, if available
(when (member "ohsnap" (font-family-list))
  (set-frame-font "ohsnap" t t))

;; disable startup message and toolbar
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; show empty lines and trailing whitespaces
(setq-default indicate-empty-lines t
              show-trailing-whitespace t)

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

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "/usr/bin/sbcl"
                slime-contribs '(slime-fancy))
          (defun re-eval ()
	    (interactive)
	    (let ((buff (current-buffer)))
	      (switch-to-buffer "*slime-repl sbcl*")
	      (slime-repl-resend)
	      (switch-to-buffer buff))))

(use-package evil-escape
  :ensure t
  :config (setq-default evil-escape-key-sequence "jk")
          (add-to-list 'evil-escape-excluded-major-modes 'term-mode))

(use-package evil
  :ensure t
  :config (evil-mode 1)
          (evil-escape-mode 1)
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
