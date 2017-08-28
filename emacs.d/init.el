(package-initialize)

(let ((pkgconf "~/.emacs.d/pkgconf.el"))
  (when (file-exists-p pkgconf)
    (load pkgconf)))

;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; cache settings
(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/" t)))

;; dont show tool- or scroll-bar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; auto revert buffers
(global-auto-revert-mode)

(unless (window-system)
  (menu-bar-mode 0))

(setq-default
 ;; disable startup message
 inhibit-startup-message t

 ;; indent with spaces per default
 indent-tabs-mode nil

 ;; show column number on mode line
 column-number-mode t

 ;; show empty lines
 indicate-empty-lines t

 ;; dont break words by wrapping to new line
 word-wrap t

 ;; org-agenda dont show holidays
 org-agenda-include-diary nil

 ring-bell-function 'ignore)

;; always show matching parens
(show-paren-mode t)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

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

(setq custom-file (concat user-emacs-directory "local.el"))

;; load other files
(mapc (lambda (file)
        (let ((file (concat user-emacs-directory file)))
          (when (file-exists-p file)
            (load file))))
      (list "d4.el"
            "local.el"))
