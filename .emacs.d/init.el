(package-initialize)

(setq custom-file
      (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar local-lisp-directory
  (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path local-lisp-directory)

(require 'd4-pkg)
(require 'd4-org)
(require 'd4)
(require 'd4-tea)
(when (file-exists-p (concat local-lisp-directory "d4-local.el"))
  (require 'd4-local))
(require 'd4-clocking)
(require 'd4-overwrites)

;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; cache settings
(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/" t)))

;; dont show tool-bar
(tool-bar-mode 0)
;; auto revert buffers
(global-auto-revert-mode)
;; show current function in modeline
(which-function-mode)
;; always show matching parens
(show-paren-mode)

;; dont show scroll-bar and set font
(setq default-frame-alist
      '((font . "JetBrains Mono 10")
        (vertical-scroll-bars . nil)))

;; alias st and screen (tmux) to xterm
(mapc (lambda (term)
        (add-to-list 'term-file-aliases
                     (cons term "xterm-256color")))
      '("st-256color"
        "screen-256color"
        "rxvt-unicode-256color"))

(menu-bar-mode 0)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x g")
  (lambda ()
    (interactive)
    (grep (format "grep -nH '\\<%s\\>' -R ."
                  (thing-at-point 'symbol)))))

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
 ;; Align text with headline
 org-adapt-indentation t
 org-hide-leading-stars nil
 org-ellipsis "⮷"
 ring-bell-function 'ignore
 ;; grep recursive inside current directory
 grep-command "grep -nH -R . -e "
 ;; scroll single lines when cursor moves out of window
 scroll-conservatively 101
 ;; Disable vc-mode on tramp buffers
 vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                              vc-ignore-dir-regexp
                              tramp-file-name-regexp))

(defun d4-recenter-bottom-hook (frame)
  (when (eql (point) (point-max))
    (recenter (window-body-height))))

(add-to-list 'window-size-change-functions
             'd4-recenter-bottom-hook)

;; copied from /r/emacs
(defun d4-backup-scratch ()
  (with-current-buffer "*scratch*"
    (when (> (buffer-size)
             (length (substitute-command-keys initial-scratch-message)))
      (let ((dir (concat user-emacs-directory "scratch/")))
        (make-directory dir t)
        (write-file (expand-file-name
                     (format-time-string "scratch-%F.%T.el")
                     dir))))))

(add-hook 'kill-emacs-hook #'d4-backup-scratch)

;; default c coding styles and settings
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "bsd"
                  c-basic-offset 8
                  tab-width 8
                  fill-column 80
                  indent-tabs-mode t
                  cc-search-directories (append cc-search-directories
                                                '("../*"
                                                  "../../*"
                                                  "../../../*")))))

;; auto update proced
(add-hook 'proced-mode-hook
          (lambda ()
            (proced-toggle-auto-update t)))
