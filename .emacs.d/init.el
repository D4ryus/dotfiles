;; -*- lexical-binding: t -*-

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

;; (require 'org-agenda)

;; --- package configuration

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

(when (>= emacs-major-version 30)
  (setq
   remote-file-name-inhibit-delete-by-moving-to-trash t
   remote-file-name-inhibit-auto-save t))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package emacs
  :custom
  (help-enable-variable-value-editing t)
  (dired-dwim-target t))

(global-set-key (kbd "M-o") 'other-window)

(defun d4-other-window-next-buffer ()
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (next-buffer)))

(defun d4-other-window-previous-buffer ()
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (previous-buffer)))

(global-set-key (kbd "C-x C-n") 'd4-other-window-next-buffer)
(global-set-key (kbd "C-x C-p") 'd4-other-window-previous-buffer)

(use-package websocket)

(use-package xref
  :custom
  (xref-after-return-hook '(recenter xref-pulse-momentarily)))

(use-package breadcrumb
  :config
  (breadcrumb-mode))

(use-package emmet-mode
  :config
  (add-to-list 'emmet-css-major-modes 'css-ts-mode)
  :bind (:map emmet-mode-keymap
              (("C-M-j" . emmet-next-edit-point)
               ("C-M-k" . emmet-prev-edit-point)))
  :hook (sgml-mode css-ts-mode css-mode web-mode))

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
  :diminish eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package undo-fu
  :config
  (setq undo-limit 67108864) ; 64mb
  (setq undo-strong-limit 100663296) ; 96mb
  (setq undo-outer-limit 1006632960)) ; 960mb

(use-package vundo)

(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode))

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

(use-package trident-mode
  :diminish trident-mode
  :hook (lisp-mode))

(use-package web-mode
  :mode "\\.erb\\'"
  :mode "\\.html\\'")

(use-package corfu
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.4 . 0.4))
  (corfu-popupinfo-hide nil)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-candidate-overlay
  :after corfu
  :init
  (require 'corfu-auto)
  :config
  (corfu-candidate-overlay-mode +1))

(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package slime
  :custom
  (lisp-loop-indent-subclauses nil)
  (lisp-loop-indent-forms-like-keywords t)
  (lisp-indent-function 'common-lisp-indent-function)
  :config
  (slime-setup '(slime-indentation
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
      (slime-repl-resend))))

(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  :hook
  (geiser-repl-mode . (lambda ()
                        (define-key geiser-repl-mode-map (kbd "\C-d") 'geiser-repl-exit))))

(use-package geiser-guile)

(use-package cider)

(defun evil-set-terminal-cursor ()
  (unless (display-graphic-p)
    (send-string-to-terminal
     (pcase evil-state
       ('normal  "\e[2 q")
       ('insert  "\e[6 q")
       ('visual  "\e[2 q")
       ('replace "\e[4 q")
       (_        "\e[2 q")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-fu)
  (evil-want-C-i-jump nil)
  (evil-symbol-word-search t)
  :config

  (add-hook 'evil-normal-state-entry-hook  'evil-set-terminal-cursor)
  (add-hook 'evil-insert-state-entry-hook  'evil-set-terminal-cursor)
  (add-hook 'evil-visual-state-entry-hook  'evil-set-terminal-cursor)
  (add-hook 'evil-replace-state-entry-hook 'evil-set-terminal-cursor)
  (add-hook 'suspend-resume-hook           'evil-set-terminal-cursor)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defvar d4-lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    clojure-mode-hook
    geiser-mode-hook))

(use-package paredit
  :config (mapc (lambda (hook)
                  (add-hook hook #'enable-paredit-mode))
                (append d4-lisp-mode-hooks
                        '(slime-repl-mode-hook))))

(use-package smartparens
  :hook (prog-mode text-mode)
  :config
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

(use-package hydra)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :mode "\\.ned\\'"
  :mode "\\.io\\'"
  :bind (:map lua-prefix-mode-map
              ("C-c" . lua-send-defun)))

(use-package eglot
  :hook
  (go-mode . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-sync-connect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout nil)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

;; Improves eldoc documentation
(use-package markdown-mode)

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package rust-mode)

(use-package wgrep)

(use-package flycheck)

(use-package ruler-mode)

(use-package enh-ruby-mode
  :mode "\\.rb\\'")

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-c" . ruby-send-definition)
              ("C-c C-l" . ruby-send-line)
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-f" . ruby-load-current-file)))

(use-package robe
  :hook (enh-ruby-mode . robe-mode)
  :hook (robe-mode . (lambda ()
                       (add-to-list 'company-backends 'company-robe))))

(use-package org-download)

(use-package rg
  :bind ("C-x g" . rg-dwim)
  :custom
  (rg-group-result nil))

(use-package popper
  :bind (("C-x /"  . popper-toggle-latest)
         ("C-x M-/" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Flycheck errors\\*"
          "\\*Flycheck errors\\*"
          erlang-shell-mode
          flymake-diagnostics-buffer-mode
          xref--xref-buffer-mode
          rg-mode
          grep-mode
          help-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

(use-package fzf
  :bind ("C-x p" . fzf))

(use-package dtrt-indent
  :custom
  (dtrt-indent-run-after-smie t)
  :config
  (dtrt-indent-global-mode t))

(use-package eat
  :config
  (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode))

(use-package direnv
 :config
 (direnv-mode))

(use-package erlang
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode)))

(use-package company-erlang)

(use-package transient
  :after (eglot))

(defun shell-it ()
  (interactive)
  (let ((cmd (apply #'buffer-substring-no-properties
                    (if (region-active-p)
                        `(,(region-beginning) ,(region-end))
                        `(,(line-beginning-position) ,(line-end-position))))))
    (shell-command cmd)))

(require 'eglot)
(transient-define-prefix
 d4-transient ()
 "A demo transient menu."
 [["Magit "
   ("m s" "Status" magit-status)
   ("m l" "Log" magit-log)
   ("m b" "Blame" magit-blame-addition)]
  ["Other Window"
   ("o n" "Next Buffer" d4-other-window-next-buffer)
   ("o p" "Previous Buffer" d4-other-window-previous-buffer)
   ("o d" "Scroll Down" scroll-other-window)
   ("o u" "Scroll Up" scroll-other-window-down)]
  ["Eglot"
   ("e r" "Rename" eglot-rename)
   ("e f" "Format" eglot-format)
   ("e a" "Code Actions" eglot-code-actions)]
  ["Buffer"
   ("x s" "Save" save-buffer)
   ("x f" "Find File" find-file)
   ("x b" "Switch to Buffer" switch-to-buffer)]]
 [["Project"
   ("p f" "Find file" project-find-file)
   ("p d" "Find dir" project-find-dir)]
  ["Ripgrep"
   ("r g" "dwim" rg-dwim)]
  ["FZF"
   ("f f" "Fzf" fzf)]
  ["Elfeed"
   ("n" "Elfeed" elfeed)]
  ["Shell"
   ("i" "It" shell-it)]])

(use-package general
  :after (transient)
  :config
  (general-evil-setup)
  (general-define-key
   :states 'normal
   :keymaps 'override
   "SPC" 'd4-transient))

(defun d4-inhibit-same-window-advice (original-function &rest args)
  (let ((display-buffer-overriding-action
          '(display-buffer-use-some-window (inhibit-same-window . t))))
    (apply original-function args)))

(advice-add 'compile-goto-error :around #'d4-inhibit-same-window-advice)
(advice-add 'next-error :around #'d4-inhibit-same-window-advice)

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
selector (default 'completing-read) to select it. See
org-map-entries's scope argument for possible scopes selector gets a
list of entries (strings) and should return the entry to clock into or
nil"
  (interactive)
  (let ((scope (or scope 'file))
        (selector (or selector 'completing-read))
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
 ;; Default background is black
 frame-background-mode 'dark
 ;; Up to 8 Mbyte should be fine
 gc-cons-threshold (ash 1 23)
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
 ;; indicator on left side when line is wrapped
 wrap-prefix (propertize
              "\N{ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS}"
              'face '(:foreground "red"
                      :background "#333"))
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
 ;; Speed up things by reading bigger chunks
 read-process-output-max (* 4 1024 1024)
 ;; Do not create .#<name> files
 create-lockfiles nil
 ;; Fix initial connection not working due to invalid prompt which is
 ;; caused since TERM=dump is not set (anymore?).
 tramp-ssh-controlmaster-options "-o SetEnv=TERM=dumb -o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=no"
 ;; Directly copy between two hosts
 tramp-use-scp-direct-remote-copying t
 ;; Disable vc-mode on tramp buffers
 vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                              vc-ignore-dir-regexp
                              tramp-file-name-regexp)
 ;; Speed up redisplay, wont edit right-to-left
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t
 mode-line-format
 '("%e" ;; Shows error when near OOM
   (:eval (propertize evil-mode-line-tag
           'face `(foreground-color . ,(alist-get evil-state
                                        '((emacs . "red")
                                          (normal . "white")
                                          (insert . "green")
                                          (operator . "red")
                                          (replace . "red")
                                          (motion . "red")
                                          (visual . "cyan"))
                                        "blue"))))
   (vc-mode ((:propertize
              (:eval (vc-git--symbolic-ref buffer-file-name))
              face (foreground-color . "orange"))
             " "))
   (:propertize (:eval (let ((host (file-remote-p default-directory 'host)))
                         (if host (format "%s " host))))
    face (foreground-color . "yellow"))
   (:eval (apply 'propertize (buffer-name (current-buffer))
           (when (buffer-modified-p) '(face bold))))

   (:propertize (buffer-read-only " (Read only)")
    face italic)
   " "
   "+%l:%C"
   " "
   "%I"
   (:eval (when (region-active-p)
            `(:propertize
              ,(format " [lines: %s, chars: %s, words: %s]"
                       (count-lines (region-beginning) (region-end))
                       (- (region-end) (region-beginning))
                       (count-words (region-beginning) (region-end)))
              face (foreground-color . "cyan"))))
   mode-line-format-right-align ;; Everything after this is right aligned
   mode-line-modes
   mode-line-misc-info))

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

(find-file-noselect "~/.emacs.d/init.el")
