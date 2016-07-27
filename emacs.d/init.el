;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; disable startup message and toolbar
(setq inhibit-startup-message t)
(tool-bar-mode -1)

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
  :config (setq inferior-lisp-program "/usr/bin/sbcl"
                slime-contribs '(slime-fancy)))

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package key-chord
  :ensure t
  :config (key-chord-mode 1)
          (key-chord-define-global "jk"
                                   'evil-normal-state))

(use-package molokai-theme
  :ensure t
  :config (load-theme 'molokai t))

(use-package paredit
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'lisp-mode-hook #'enable-paredit-mode))
