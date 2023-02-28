(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (message "Native compilation is available")
  (setq native-comp-deferred-compilation t))

(package-initialize)

(defvar backup-directory
  (concat user-emacs-directory "backup/"))
(catch 'file-already-exists
  (make-directory backup-directory t))

(setq custom-file
      (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'org-agenda)

;; --- package configuration

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
  (whitespace-style '(face tabs))
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
  :hook
  (prog-mode . (lambda ()
                 (whitespace-mode 1)
                 (setq show-trailing-whitespace t))))

(use-package eldoc
  :diminish eldoc-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist
        `(("." . ,backup-directory))))

(use-package vertico
  :custom
  (vertico-group-format nil)
  (vertico-count 20)
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x g" . consult-ripgrep-thing-at-point)
         ("C-c c l" . consult-line)
         ("C-c c b" . consult-buffer)
         ("C-c c f" . consult-find)
         ("C-c c i" . consult-imenu))
  :init
  (consult-customize
   consult-ripgrep
   :add-history (seq-some #'thing-at-point '(region symbol)))
  (defalias 'consult-ripgrep-thing-at-point 'consult-ripgrep)
  (consult-customize
   consult-ripgrep-thing-at-point
   :initial (thing-at-point 'symbol)))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (css-mode))

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
  :config (magit-todos-mode)
  :custom (magit-todos-ignored-keywords '()))

(use-package trident-mode
  :diminish trident-mode
  :hook (lisp-mode))

(use-package company
  :diminish company-mode
  :bind (("C-x TAB" . company-complete)
         :map company-active-map
         ("\C-n"      . company-select-next)
         ("\C-p"      . company-select-previous)
         ("\C-d"      . company-show-doc-buffer)
         ("<tab>"     . company-complete-selection)
         ("TAB"       . company-complete-selection)
         ("S-<tab>"   . company-complete-common)
         ("<backtab>" . company-complete-common)
         ([return]    . newline-and-indent)
         ("RET"       . newline-and-indent))
  :custom
  (company-idle-delay nil)
  :config
  (global-company-mode))

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
  (defun re-eval ()
    (interactive)
    (with-current-buffer (get-buffer "*slime-repl sbcl*")
      (slime-repl-resend)))
  :hook
  (slime-mode . (lambda ()
                  ;; Should not be required, but slime-company broke
                  ;; somehow and is not activated automatically :/
                  (add-to-list 'company-backends 'company-slime)
                  (define-key evil-normal-state-local-map (kbd "M-.")
                    'slime-edit-definition))))

(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  :hook
  (geiser-repl-mode . (lambda ()
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
  (evil-motion-state-modes nil)
  (evil-insert-state-modes '(dired-mode
                             Custom-mode
                             elfeed-search-mode
                             elfeed-show-mode
                             geiser-repl-mode
                             geiser-debug-mode
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
                             org-agenda-mode
                             rg-mode
                             compilation-mode
                             inf-ruby-mode))
  (evil-normal-state-modes '(git-commit-mode
                             prog-mode
                             text-mode))
  :config
  (define-key evil-normal-state-map (kbd "M-.")
    'xref-find-definitions)
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
                        '(slime-repl-mode-hook))))

(use-package evil-paredit
  :config (mapc (lambda (hook)
                  (add-hook hook #'evil-paredit-mode))
                d4-lisp-mode-hooks))

(use-package hydra)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :mode "\\.ned\\'"
  :mode "\\.io\\'"
  :bind (:map lua-prefix-mode-map
              ("C-c" . lua-send-defun)))

(use-package skewer-mode
  :hook (js2-mode css-mode html-mode))

(use-package eglot
  :hook
  (go-mode . eglot-ensure))

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package rust-mode)

(use-package wgrep)

(use-package flycheck
  :hook (prog-mode))

(use-package ruler-mode)

(use-package path-headerline-mode)

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-c" . ruby-send-definition)
              ("C-c C-l" . ruby-send-line)
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-f" . ruby-load-current-file)))

(use-package org-download)

(use-package rg
  :bind ("C-x g" . rg-dwim-current-dir)
  :custom
  (rg-executable (expand-file-name ".cargo/bin/rg" (getenv "HOME")))
  (rg-group-result nil))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always nil))

(use-package popper
  :ensure t
  :bind (("C-x /"  . popper-toggle-latest)
         ("C-x M-/" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              "\\*Flycheck errors\\*"
                              xref--xref-buffer-mode
                              rg-mode
                              grep-mode
                              help-mode
                              compilation-mode))
  (popper-window-height
   (lambda (win)
     (let ((height (floor (frame-height) 3)))
       (fit-window-to-buffer win height height))))
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

(use-package tree-sitter
  :init (global-tree-sitter-mode)
  ;; Using prog-mode breaks with unsupported modes:
  ;: File mode specification error: (error No language registered for major mode ‘emacs-lisp-mode’)
  :hook (go-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package fzf
  :bind ("C-x p" . fzf))

;; Inject display-buffer-use-some-window by overiding
;; next-error-no-select from emacs to prevent it from opening up
;; multiple windows while cycling through errors.
(defun next-error-no-select (&optional n)
  "Move point to the next error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).  Finds and highlights the source line
like \\[next-error], but does not select the source buffer."
  (interactive "p")
  (save-selected-window
   (let ((next-error-highlight next-error-highlight-no-select)
         (display-buffer-overriding-action
           '(display-buffer-use-some-window (inhibit-same-window . t))))
     (next-error n))))

(defun d4-toggle-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))

(defun d4-toggle-path-headerline-mode ()
  (interactive)
  (if path-headerline-mode
      (progn
        (path-headerline-mode 0)
        (path-header-line-off))
    (progn
      (path-headerline-mode 1)
      (path-header-line-on))))

(defun d4-toggle-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode
        (not indent-tabs-mode)))

(global-set-key
 (kbd "C-x t")
 (defhydra toggle ()
   "toggle modes and settings"
   ("l" toggle-truncate-lines "truncate")
   ("w" whitespace-mode "whitespace")
   ("c" rainbow-mode "rainbow")
   ("n" linum-mode "line numbers")
   ("t" d4-toggle-trailing-whitespace "trailing whitespace")
   ("r" ruler-mode "ruler mode")
   ("h" d4-toggle-path-headerline-mode "path headerline mode")
   ("i" d4-toggle-indent-tabs-mode "indent tabs")
   ("q" nil "cancel")))

;; --- org functions

(defun d4-org-min->string (minutes)
  "convert given minutes to a \"hh:mm\" string
see: d4-org-string->min (inverse)"
  (let* ((hs (* 60 60))
         (ms 60)
         (h (floor (/ minutes ms)))
         (m (mod minutes ms)))
    (format "%d:%02d" h m)))

(defun d4-org-string->min (time)
  "convert given \"hh:mm\" string to minutes
see: d4-org-min->string (inverse)"
  (cl-destructuring-bind (h m)
     (cl-map 'list 'string-to-number (split-string time ":"))
    (+ (* 60 h) m)))

(defun d4-org-strange-time->min (strange-time)
  "convert given 'strange-time' (number 110 for 01:10) to minutes (->
returns 70 (60 + 10))"
  (+ (* 60 (floor strange-time 100))
     (mod strange-time 100)))

(defun d4-org-min-diff (t1 t2)
  "Time difference in minutes between two time strings in \"hh:mm\"
format.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (abs (- (d4-org-string->min t1)
           (d4-org-string->min t2)))))

(defun d4-org-sum (&rest args)
  "Sum up all given \"hh:mm\" strings.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (cl-reduce '+ (cl-map 'list 'd4-org-string->min args))))

(defun d4-org-avg (&rest args)
  "Average all given \"hh:mm\" strings.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (/ (cl-reduce '+ (cl-map 'list 'd4-org-string->min args))
      (length args))))

;; --- custom functions

(defun d4-set-background (mode)
  "set background to given mode which is either 'dark or 'light"
  (setq frame-background-mode mode)
  (mapc 'frame-set-background-mode (frame-list))
  (let ((dark  "#000000")
        (light "#FFFFFF"))
    (if (eql mode 'dark)
        (progn (set-background-color dark)
               (set-foreground-color light))
        (progn (set-background-color light)
               (set-foreground-color dark)))))

(defun d4-buffer-mode (buffer-or-name)
  "return mode of given buffer"
  (with-current-buffer buffer-or-name major-mode))

(defun d4-filter-buffers-by-mode (mode &optional buffer-list)
  "return all buffers with given mode"
  (delq nil
        (mapcar (lambda (buffer)
                  (and (eq (d4-buffer-mode buffer) mode) buffer))
                (or buffer-list (buffer-list)))))

(defun d4-set-style (style &rest modes)
  "set style to all buffers with given modes"
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (c-set-style style)))
        (apply 'append (mapcar 'd4-filter-buffers-by-mode modes))))

(defun d4-to-bin (number)
  (apply 'concat
         (reverse
          (cl-loop for i to 31
                   collect
                   (format
                    (if (and (not (eql i 0))
                             (eql (mod i 4) 0))
                        "%d "
                        "%d")
                    (logand #x1 (lsh number (- i))))))))

(defun d4-to-ascii (number)
  (apply 'concat
         (reverse
          (cl-loop for i to 7
                   collect
                   (format
                    "%c"
                    (c-int-to-char
                     (logand #xff (lsh number (- (* i 8))))))))))

(defun d4-YYY ()
  (interactive)
  (insert
   "#define YYY(__fmt, ...)					/* YYY */\\\n"
   "	do {							/* YYY */\\\n"
   "		printf(\"YYY:%s:%s:%d:\" __fmt \"\\n\",		/* YYY */\\\n"
   "		    __FILE__, __PRETTY_FUNCTION__, __LINE__,	/* YYY */\\\n"
   "		    ##__VA_ARGS__);				/* YYY */\\\n"
   "	} while (0)						/* YYY */\n"))

(require 'dbus)

(defun jarvis-copy (&optional text)
  (interactive)
  (dbus-call-method
   :session "net.d4ryus.Jarvis"
   "/net/d4ryus/Jarvis"
   "net.d4ryus.Clipboard" "Copy"
   (or text
       (and (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end)))
       (error "Copy requires region or text"))))

(defun jarvis-kill ()
  (interactive)
  (let ((text
          (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
              (error "Select text to kill"))))
    (delete-active-region t)
    (jarvis-copy text)))

(defun jarvis-paste ()
  (interactive)
  (let ((text (dbus-call-method
               :session "net.d4ryus.Jarvis"
               "/net/d4ryus/Jarvis"
               "net.d4ryus.Clipboard" "Paste")))
    (when (region-active-p)
      (delete-active-region t))
    (insert text)))

;; --- clocking configuration

;; used to cache results
(defvar d4-last-timestamp nil
  "contains timestamp of last modified org-agenda file")
(defvar d4-last-date nil
  "last date we checked")
(defvar d4-last-result nil
  "cache of last results")

(defun d4-get-current-time ()
  "return minutes since 00:00"
  (abs (floor (- (float-time (org-current-time))
                 (org-time-today))
              60)))

(defun d4-clock-into (&optional scope selector)
  "clock into a task inside the given scope (default 'file) by using
selector (default 'ivy-completing-read) to select it.  see
org-map-entries's scope argument for possible scopes selector gets a
list of entries (strings) and should return the entry to clock into or
nil"
  (interactive)
  (let ((scope (or scope 'file))
        (selector (or selector 'ivy-completing-read))
        (entries nil)
        (pos (point))
        (parents nil))
    (cl-flet ((filter (entry-string)
                      (let* ((depth (cl-position #x20 entry-string))
                             (entry (cl-subseq entry-string (+ 1 depth)))
                             (p-depth (length parents)))
                        (cond
                          ((> depth p-depth) (setf parents (cons entry parents)))
                          ((= depth p-depth) (setf (car parents) entry))
                          ((< depth p-depth) (setf parents
                                                   (cons entry
                                                         (cl-subseq parents
                                                                    (+ 1 (- p-depth depth)))))))
                        (cl-reduce (lambda (new accum)
                                     (concat accum "/" new))
                                   parents))))
             ;; TODO: goto 'clock-task-tree' then use 'tree as argument
             (org-map-entries (lambda ()
                                (setf entries
                                      (cons (cons (filter (org-current-line-string))
                                                  (point))
                                            entries)))
                              nil scope))
    (let ((entry (cl-find (funcall selector "Clock into: >"
                                   (reverse
                                    (mapcar 'car entries)))
                          entries
                          :key 'car
                          :test 'equal)))
      (if entry
          (save-excursion
           (goto-char (cdr entry))
           (org-clock-in))
          (message "no entry found")))))

(defun d4-get-entry-time (entry)
  "returns the time in minutes of a given text entry"
  (let ((time (get-text-property 0 'time-of-day entry)))
    (and time
         (d4-org-strange-time->min time))))

(defun d4-get-entry-duration (entry)
  "returns the duration in minutes of a given text entry"
  (get-text-property 0 'duration entry))

(defun d4-last-change (file)
  "return last-change timestamp of given file"
  (float-time
   (nth 5 (file-attributes file))))

(defun d4-calculate-entries ()
  "return a sorted list (closest first) of all agenda entries of
today, excluding already passed entries.
note: (d4-get-agenda-time-entries) wraps d4-calculate-entries and
caches results if files where not modified"
  (let ((current-time (d4-get-current-time))
        (date (calendar-current-date)))
    (sort
     (cl-delete-if 'null
                   (mapcar (lambda (entry)
                             (let ((entry-start-time (d4-get-entry-time entry)))
                               (when (and entry-start-time
                                          (> (+ entry-start-time
                                                (or (d4-get-entry-duration entry)
                                                    (- (* 24 60) entry-start-time)))
                                             current-time))
                                 entry)))
                           (apply 'append
                                  (mapcar
                                   (lambda (agenda-file)
                                     (org-agenda-get-day-entries agenda-file date))
                                   org-agenda-files))))
     (lambda (a b)
       (< (d4-get-entry-time a)
          (d4-get-entry-time b))))))

(defun d4-get-agenda-time-entries ()
  "call d4-calculate-entries, but only if files where modified or the
day changed. Cached results will be saved in
d4-last-{timestamp,date,result}"
  (let ((last-modified (apply 'max
                              (mapcar 'd4-last-change
                                      org-agenda-files))))
    (if (and d4-last-timestamp d4-last-date
             (<= last-modified d4-last-timestamp)
             (cl-equalp d4-last-date (calendar-current-date)))
        d4-last-result
        (setq d4-last-timestamp last-modified
              d4-last-date (calendar-current-date)
              d4-last-result (d4-calculate-entries)))))

(defun d4-format-agenda-entry (entry)
  "format given entry to a nice printable string like:
'entry-name' 'when' 'timeframe'
examples:
daily in 1:30 (11:40-12:00)
daily now (11:40-12:00)"
  (let ((ctime (d4-get-current-time))
        (etime (d4-get-entry-time entry)))
    (format "%s %s (%s)"
            (let ((txt (get-text-property 0 'txt entry)))
              (apply 'substring-no-properties txt
                     (when (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
                                         txt)
                       (list 0 (match-beginning 0)))))
            (if (> etime ctime)
                (format "in %s"
                        (d4-org-min->string
                         (- etime ctime)))
                "now")
            (get-text-property 0 'time entry))))

(defun d4-upcoming-entries ()
  "nicely formatted string with displays upcoming org agenda entries"
  (let* ((ctime (d4-get-current-time))
         (entries (d4-get-agenda-time-entries))
         (upcoming nil))
    (cl-loop for entry in entries
             if (let* ((etime-start (d4-get-entry-time entry))
                       (etime-end (+ etime-start
                                     (or (d4-get-entry-duration entry) 0))))
                  (or (and (<= etime-start ctime)
                           (>= etime-end ctime))
                      (and (null upcoming)
                           (> etime-start ctime))))
             do (push entry upcoming))
    (when upcoming
      (concat "["
              (cl-reduce (lambda (accum &optional new)
                           (if new
                               (concat accum ", " new)
                               accum))
                         (mapcar 'd4-format-agenda-entry
                                 (reverse upcoming)))
              "]"))))

;; --- local config

(let ((local-file (concat user-emacs-directory "local.el")))
  (when (file-exists-p local-file)
    (load local-file)))

;; auto revert buffers
(global-auto-revert-mode 1)
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

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 ;; Up to 1 Gigabyte should be fine
 gc-cons-threshold (ash 1 30)
 ;; No need to put active selection into PRIMARY
 select-active-regions nil
 ;; Don't use popups
 use-dialog-box nil
 ;; backup settings
 backup-directory-alist `(("." . ,backup-directory))
 ;; cache settings
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/" t))
 ;; disable blinking cursor
 blink-cursor-mode nil
 ;; dont show tool-bar
 tool-bar-mode nil
 ;; dont show menu-bar
 menu-bar-mode nil
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
 org-hide-leading-stars nil
 org-ellipsis " ⮷"
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
                  indent-tabs-mode t)))

;; auto update proced
(add-hook 'proced-mode-hook
          (lambda ()
            (proced-toggle-auto-update t)))
