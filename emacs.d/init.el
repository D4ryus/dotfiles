(package-initialize)

(add-to-list 'load-path
             (concat user-emacs-directory "lisp"))

(require 'd4-pkg)
(require 'd4-org)
(require 'd4)
(require 'd4-tea)
(require 'd4-local)
(require 'd4-clocking)
(require 'd4-overwrites)

;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; cache settings
(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/" t)))

;; dont show tool-bar
(tool-bar-mode -1)

;; dont show scroll-bar and default background to black
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (background-color . "#000000")
        (foreground-color . "#FFFFFF")))

;; alias st and screen (tmux) to xterm
(mapc (lambda (term)
        (add-to-list 'term-file-aliases
                     (cons term "xterm-256color")))
      '("st-256color"
        "screen-256color"))

;; auto revert buffers
(global-auto-revert-mode)

;; show current function in modeline
(which-function-mode t)

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

 org-tags-column 80

 org-return-follows-link t

 ;; add timestamp to done tasks
 org-log-done 'time

 ;; use drawer (LOGBOOK)
 org-log-into-drawer t
 org-clock-into-drawer t

 ring-bell-function 'ignore

 ;; grep recursive inside current directory
 grep-command "grep -nH -R . -e ")

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

(setq custom-file
      (concat user-emacs-directory "custom.el"))
(load custom-file)
